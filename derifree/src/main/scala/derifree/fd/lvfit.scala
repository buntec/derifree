package derifree
package fd

import cats.syntax.all.*
import derifree.math.LevenbergMarquardt
import derifree.math.LinearInterpolation
import derifree.syntax.*
import derifree.syntax.given

import scala.math.max
import scala.math.pow
import scala.math.sqrt

object lvfit:

  trait VolSurface[T]:
    def apply(expiry: T, strike: Double): Double

  case class PureObservation(
      strike: Double, // pure strike
      expiry: YearFraction,
      vol: Double, // pure vol
      spread: Double
  )

  case class Settings(
      nKnots: Int,
      minLv: Double,
      maxLv: Double,
      gridQuantile: Double = 1e-5,
      nRannacherSteps: Int = 2,
      alpha: Double = 0.5,
      beta: Double = 0.01,
      dtMin: Double = 1.0 / 365 / 5,
      dtMax: Double = 10.0 / 365
  )

  case class Result(
      expiries: List[YearFraction],
      gridBounds: List[(Double, Double)],
      lvKnots: List[IndexedSeq[Double]] = Nil,
      lvAtKnots: List[IndexedSeq[Double]] = Nil,
      settings: Settings
  )

  private case class State(
      grids: List[IndexedSeq[Double]] = Nil,
      values: List[IndexedSeq[Double]] = Nil,
      lvKnots: List[IndexedSeq[Double]] = Nil,
      lvAtKnots: List[IndexedSeq[Double]] = Nil,
      gridBounds: List[(Double, Double)] = Nil
  ):
    def isFirstExpiry: Boolean = lvKnots.isEmpty

  private case class State2(
      grids: List[IndexedSeq[Double]] = Nil,
      values: List[IndexedSeq[IndexedSeq[Double]]] = Nil
  ):
    def isFirstExpiry: Boolean = grids.isEmpty

  def volSurface[T: TimeLike](
      result: Result,
      forward: Forward[T],
      discount: YieldCurve[T],
      refTime: T
  ): VolSurface[T] =
    val pureSurface = pureVolSurface(result)
    new VolSurface[T]:
      def apply(expiry: T, strike: Double): Double =
        val yf = TimeLike[T].yearFractionBetween(refTime, expiry)
        val pureStrike =
          buehler.strikeToPureStrike(strike, forward(expiry), forward.dividendFloor(expiry))
        pureSurface(yf, pureStrike)

  def pureVolSurface(result: Result): VolSurface[YearFraction] =
    val expiries = result.expiries
    val settings = result.settings
    val s0 = 1.0
    val spatialGridFactory = SpatialGrid.Factory.logSinh(250, 0.1, s0)
    val timeGridFactory =
      TimeGrid.Factory.powerRule(settings.alpha, settings.beta, settings.dtMin, settings.dtMax)
    val timegrid = timeGridFactory(expiries.toSet)

    val state = expiries
      .zip(expiries.tail)
      .zip(result.lvKnots)
      .zip(result.lvAtKnots)
      .zip(result.gridBounds)
      .foldLeft(State2()):
        case (state, ((((t1, t2), lvKnots), lvAtKnots), (lb, ub))) =>
          val timeGridSlice = timegrid.slice(t1, t2).get.yearFractions
          val grid = spatialGridFactory(lb, ub)

          val interiorPoints = grid.slice(1, grid.length - 1)
          val nInteriorPoints = interiorPoints.length

          val initialInteriorValues =
            (state.grids.headOption, state.values.headOption.flatMap(_.headOption)).tupled.fold(
              interiorPoints.map(s => max(s0 - s, 0.0))
            )((prevGrid, prevVals) =>
              val spline = CubicSpline.natural(prevGrid, prevVals)
              interiorPoints.map(spline(_))
            )

          val interiorValuesOnGrid =
            val lvSlice = LinearInterpolation.withFlatExtrapolation(lvKnots, lvAtKnots)

            val reaction = Array.ofDim[Double](nInteriorPoints)
            val convection = Array.ofDim[Double](nInteriorPoints)
            val diffusion = Array.ofDim[Double](nInteriorPoints)

            var k = 0
            while (k < nInteriorPoints) {
              val s = interiorPoints(k)
              val lv = lvSlice(s)
              diffusion(k) = 0.5 * s * s * lv * lv
              k += 1
            }

            val op =
              Operator(
                grid,
                IArray.unsafeFromArray(convection),
                IArray.unsafeFromArray(diffusion),
                IArray.unsafeFromArray(reaction),
                BoundaryCondition.Linear,
                BoundaryCondition.Linear
              )

            (timeGridSlice zip timeGridSlice.tail).zipWithIndex.scanLeft(initialInteriorValues):
              case (v1, ((t1, t2), stepIndex)) =>
                val dt = (t2 - t1).toDouble
                if state.isFirstExpiry && stepIndex < settings.nRannacherSteps then
                  // Rannacher smoothing
                  op.implicitStep(op.implicitStep(v1, 0.5 * dt), 0.5 * dt)
                else
                  // Crank-Nicolson
                  op.thetaStep(v1, dt, 0.5)

          val values = interiorValuesOnGrid
            .map(ivals =>
              SpatialGrid
                .addBoundaryValues(
                  grid,
                  ivals,
                  BoundaryCondition.Linear,
                  BoundaryCondition.Linear
                )
                .toIndexedSeq
            )

          State2(grid :: state.grids, values :: state.values)

    val interpolatedValues = state.grids
      .zip(state.values)
      .flatMap((grid, vals) => vals.map(CubicSpline.natural(grid, _)))
      .toIndexedSeq

    def interpolatedVol(expiry: YearFraction, strike: Double) =
      val i = timegrid.yearFractions.search(expiry).insertionPoint
      val t = expiry.toDouble
      val t1 = timegrid.yearFractions(i - 1).toDouble
      val t2 = timegrid.yearFractions(i).toDouble
      val c1 = interpolatedValues(i - 1)(strike)
      val c2 = interpolatedValues(i)(strike)
      val vol1 = black.impliedVol(black.OptionType.Call, strike, t1, 1.0, 1.0, c1).toTry.get
      val vol2 = black.impliedVol(black.OptionType.Call, strike, t2, 1.0, 1.0, c2).toTry.get
      val var1 = vol1 * vol1 * t1
      val var2 = vol2 * vol2 * t2
      val totVar = var1 + (var2 - var1) * (t - t1) / (t2 - t1)
      sqrt(totVar / t)

    new VolSurface[YearFraction]:
      def apply(expiry: YearFraction, strike: Double): Double =
        interpolatedVol(expiry, strike)

  private[derifree] def pureImpl(
      obs: List[PureObservation],
      settings: Settings
  ): Result =
    val obsByExpiry =
      obs.groupBy(_.expiry).toList.sortBy(_(0)).filter((t, _) => t.toDouble > 0.0)

    val expiries = YearFraction.zero :: obsByExpiry.map(_(0))

    val s0 = 1.0
    val spatialGridFactory = SpatialGrid.Factory.logSinh(250, 0.1, s0)

    val timeGridFactory =
      TimeGrid.Factory.powerRule(settings.alpha, settings.beta, settings.dtMin, settings.dtMax)

    val timegrid = timeGridFactory(expiries.toSet)

    val state = expiries
      .zip(obsByExpiry)
      .foldLeft(State()):
        case (state, (t1, (t2, obs))) =>
          val refVol = obs.map(_.vol).sum / obs.length

          val timeGridSlice = timegrid.slice(t1, t2).get.yearFractions

          val minStrike = obs.map(_.strike).min
          val maxStrike = obs.map(_.strike).max

          val lvKnots = List
            .tabulate(settings.nKnots)(i =>
              minStrike * pow(maxStrike / minStrike, i.toDouble / (settings.nKnots - 1))
            )
            .toIndexedSeq

          val sMin =
            SpatialGrid.lognormalPercentile(refVol, t2.toDouble, 0, settings.gridQuantile)

          val sMax =
            SpatialGrid.lognormalPercentile(refVol, t2.toDouble, 0, 1 - settings.gridQuantile)

          val grid = spatialGridFactory(sMin, sMax)
          val interiorPoints = grid.slice(1, grid.length - 1)
          val nInteriorPoints = interiorPoints.length

          val initialInteriorValues =
            (state.grids.headOption, state.values.headOption).tupled.fold(
              interiorPoints.map(s => max(s0 - s, 0.0))
            )((prevGrid, prevVals) =>
              val spline = CubicSpline.natural(prevGrid, prevVals)
              interiorPoints.map(spline(_))
            )

          def interiorValuesOnGrid(lvAtKnots: IndexedSeq[Double]) =
            val lvSlice = LinearInterpolation.withFlatExtrapolation(lvKnots, lvAtKnots)

            val reaction = Array.ofDim[Double](nInteriorPoints)
            val convection = Array.ofDim[Double](nInteriorPoints)
            val diffusion = Array.ofDim[Double](nInteriorPoints)

            var k = 0
            while (k < nInteriorPoints) {
              val s = interiorPoints(k)
              val lv = lvSlice(s)
              diffusion(k) = 0.5 * s * s * lv * lv
              k += 1
            }

            val op =
              Operator(
                grid,
                IArray.unsafeFromArray(convection),
                IArray.unsafeFromArray(diffusion),
                IArray.unsafeFromArray(reaction),
                BoundaryCondition.Linear,
                BoundaryCondition.Linear
              )

            (timeGridSlice zip timeGridSlice.tail).zipWithIndex.scanLeft(initialInteriorValues):
              case (v1, ((t1, t2), stepIndex)) =>
                val dt = (t2 - t1).toDouble
                if state.isFirstExpiry && stepIndex < settings.nRannacherSteps then
                  // Rannacher smoothing
                  op.implicitStep(op.implicitStep(v1, 0.5 * dt), 0.5 * dt)
                else
                  // Crank-Nicolson
                  op.thetaStep(v1, dt, 0.5)

          def objectiveFun(lvAtKnots: IndexedSeq[Double]) =
            val vog = interiorValuesOnGrid(lvAtKnots)
            val terminalVals = SpatialGrid.addBoundaryValues(
              grid,
              vog.last,
              BoundaryCondition.Linear,
              BoundaryCondition.Linear
            )
            val spline = CubicSpline.natural(grid, terminalVals)
            obs
              .map(obs =>
                black
                  .impliedVol(
                    black.OptionType.Call,
                    obs.strike,
                    t2.toDouble,
                    1.0,
                    1.0,
                    spline(obs.strike)
                  )
                  .toTry
                  .get
              )
              .toIndexedSeq

          val guess = IndexedSeq.fill(lvKnots.length)(refVol)

          val optResult = LevenbergMarquardt.optimize(
            objectiveFun,
            lvKnots.length,
            obs.length,
            obs.map(_.vol).toIndexedSeq,
            guess,
            obs.map(m => 1.0 / (m.spread * m.spread)).toIndexedSeq,
            IndexedSeq.fill(lvKnots.length)(settings.minLv),
            IndexedSeq.fill(lvKnots.length)(settings.maxLv),
            0.001,
            0.0001
          )

          val lvAtKnots = optResult.optimum

          val vals = interiorValuesOnGrid(lvKnots)
            .map(interiorVals =>
              SpatialGrid
                .addBoundaryValues(
                  grid,
                  interiorVals,
                  BoundaryCondition.Linear,
                  BoundaryCondition.Linear
                )
                .toIndexedSeq
            )
            .toList

          State(
            grid :: state.grids,
            vals ::: state.values,
            lvKnots :: state.lvKnots,
            lvAtKnots :: state.lvAtKnots,
            (sMin, sMax) :: state.gridBounds
          )

    Result(
      expiries,
      state.gridBounds,
      state.lvKnots,
      state.lvAtKnots,
      settings
    )
