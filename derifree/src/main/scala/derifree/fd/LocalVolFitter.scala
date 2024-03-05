/*
 * Copyright 2023 buntec
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package derifree
package fd

import cats.syntax.all.*
import derifree.dtos.etd.options.Snapshot
import derifree.math.LevenbergMarquardt
import derifree.math.LinearInterpolation
import derifree.math.RootFinding
import derifree.syntax.*
import derifree.syntax.given

import scala.annotation.tailrec
import scala.collection.Searching.Found
import scala.collection.Searching.InsertionPoint
import scala.math.exp
import scala.math.log
import scala.math.max
import scala.math.min
import scala.math.round
import scala.math.pow
import scala.math.sqrt

import LocalVolFitter.*

sealed trait LocalVolFitter:

  def fitPureObservations(
      obs: List[PureObservation],
      settings: Settings
  ): Either[Error, Result]

  def pureVolSurface(result: Result): Either[Error, VolSurface[YearFraction]]

  def pureLocalVolSurface(result: Result): LocalVolSurface[YearFraction]

  /** Quantile function of the "pure" spot process X_t. */
  def pureQuantile(result: Result): Either[Error, QuantileFunction[YearFraction]]

  /** Quantile function of the "real" spot process S_t. */
  def quantile[T: TimeLike](
      result: Result,
      forward: Forward[T],
      refTime: T
  ): Either[Error, QuantileFunction[T]]

  /** Marginal CDF of the "pure" process X_t. */
  def pureCdf(result: Result): Either[Error, Cdf[YearFraction]]

  /** Marginal cdf of the "real" spot price process S_t. */
  def cdf[T: TimeLike](
      result: Result,
      forward: Forward[T],
      refTime: T
  ): Either[Error, Cdf[T]]

  /** Marginal density of the "pure" process X_t. */
  def pureDensity(result: Result): Either[Error, Density[YearFraction]]

  /** Marginal density of the "real" spot price process S_t. */
  def density[T: TimeLike](
      result: Result,
      forward: Forward[T],
      refTime: T
  ): Either[Error, Density[T]]

  /** Unlike in Buehler's paper, the surface is right-continuous in time. */
  def localVolSurface[T: TimeLike](
      result: Result,
      forward: Forward[T],
      refTime: T
  ): LocalVolSurface[T]

  def volSurface[T: TimeLike](
      result: Result,
      forward: Forward[T],
      refTime: T
  ): Either[Error, VolSurface[T]]

  def fitSnapshot[T: TimeLike](
      snapshot: Snapshot,
      forward: Forward[T],
      refTime: T,
      settings: Settings,
      shortMaturityCutoff: T,
      deltaCutoffPuts: Double = -0.05,
      deltaCutoffCalls: Double = 0.05
  ): Either[Error, Result]

object LocalVolFitter:

  case class Settings(
      minLv: Double = 0.01,
      maxLv: Double = 5.0,
      nKnots: Settings.Knots = Settings.Knots.Fixed(5),
      spatialGrid: Settings.SpatialGrid = Settings.SpatialGrid(),
      timeGrid: Settings.TimeGrid = Settings.TimeGrid(),
      nRannacherSteps: Int = 2
  )

  object Settings:

    /** The step size at time t is min(dtMax, max(dtMin, beta * t^alpha)) */
    case class TimeGrid(
        alpha: Double = 0.5,
        beta: Double = 0.01,
        dtMin: Double = 1.0 / 365 / 5,
        dtMax: Double = 10.0 / 365
    )

    case class SpatialGrid(
        nPoints: Int = 200,
        quantile: Double = 1e-5,
        /** Lower values mean higher concentration of grid points around ATM. */
        sinhConcentration: Double = 0.1,
        expansionFactor: Double = 0.2,
        tailFraction: Double = 0.025,
        expansionMaxIters: Int = 10
    )

    enum Knots:
      case Fixed(n: Int)
      case Dynamic(min: Int, max: Int, alpha: Double, minStrikes: Int, minSpread: Double)

  case class Result(
      /** The fitted expiries in year fraction terms. By convention the first element is always
        * zero.
        */
      expiries: List[YearFraction],

      /** The spatial grid bounds `(lb, ub)` applicable between two expiries . */
      gridBounds: List[(Double, Double)],

      /** The local vol knots in pure spot terms */
      lvKnots: List[IndexedSeq[Double]] = Nil,

      /** The fitted local vol values at the knots. */
      lvAtKnots: List[IndexedSeq[Double]] = Nil,

      /** The settings used for this fit. */
      settings: Settings
  )

  case class PureObservation(
      strike: Double, // pure strike
      expiry: YearFraction,
      vol: Double, // pure vol
      spread: Double
  )

  trait VolSurface[T]:
    def apply(expiry: T, strike: Double): Option[Double]

  trait LocalVolSurface[T]:
    def apply(time: T, spot: Double): Double

  trait Density[T]:
    def apply(time: T, spot: Double): Double

  trait Cdf[T]:
    def apply(time: T, spot: Double): Option[Double]

  trait QuantileFunction[T]:
    def apply(time: T, p: Double): Option[Double]

  def apply = new LocalVolFitter:

    override def pureQuantile(result: Result): Either[Error, QuantileFunction[YearFraction]] =
      pureCdf(result).map(pcdf =>
        new QuantileFunction[YearFraction] {

          override def apply(time: YearFraction, p: Double): Option[Double] =
            val i = result.expiries.search(time).insertionPoint
            val (lb, ub) = result.gridBounds(max(0, i - 1))
            RootFinding
              .brent(
                x => pcdf(time, x).get - p,
                lb,
                ub,
                RootFinding.Settings(absAccuracy = 1e-5)
              )
              .toOption

        }
      )

    override def quantile[T: TimeLike](
        result: Result,
        forward: Forward[T],
        refTime: T
    ): Either[Error, QuantileFunction[T]] = pureQuantile(result).map(pqf =>
      new QuantileFunction[T] {

        override def apply(time: T, p: Double): Option[Double] =
          val t = refTime.yearFractionTo(time)
          val f = forward(time)
          val d = forward.dividendFloor(time)
          pqf(t, p).map(x => (f - d) * x + d)

      }
    )

    override def cdf[T: TimeLike](
        result: Result,
        forward: Forward[T],
        refTime: T
    ): Either[Error, Cdf[T]] =
      pureCdf(result).map(pcdf =>
        new Cdf[T]:
          def apply(time: T, spot: Double): Option[Double] =
            val t = refTime.yearFractionTo(time)
            val f = forward(time)
            val d = forward.dividendFloor(time)
            val x = (spot - d) / (f - d)
            if (spot > d) then pcdf(t, x) else 0.0.some
      )

    def density[T: TimeLike](
        result: Result,
        forward: Forward[T],
        refTime: T
    ): Either[Error, Density[T]] =
      pureDensity(result).map(pdensity =>
        new Density[T]:
          def apply(time: T, spot: Double): Double =
            val t = refTime.yearFractionTo(time)
            val f = forward(time)
            val d = forward.dividendFloor(time)
            val x = (spot - d) / (f - d)
            if spot > d then pdensity(t, x) / (f - d) else 0.0
      )

    def pureDensity(result: Result): Either[Error, Density[YearFraction]] =
      Either
        .catchNonFatal(
          pureDensityImpl(result)
        )
        .leftMap(t => Error.Generic(t.getMessage))

    override def pureCdf(result: Result): Either[Error, Cdf[YearFraction]] =
      Either
        .catchNonFatal(pureCdfImpl(result))
        .leftMap(t => Error.Generic(t.getMessage))

    def fitPureObservations(
        obs: List[PureObservation],
        settings: Settings
    ): Either[Error, Result] =
      Either
        .catchNonFatal(pureImpl(obs, settings))
        .leftMap(t => Error.Generic(t.getMessage))

    def pureVolSurface(result: Result): Either[Error, VolSurface[YearFraction]] =
      Either
        .catchNonFatal(pureVolSurfaceImpl(result))
        .leftMap(t => Error.Generic(t.getMessage))

    def volSurface[T: TimeLike](
        result: Result,
        forward: Forward[T],
        refTime: T
    ): Either[Error, VolSurface[T]] =
      Either
        .catchNonFatal(volSurfaceImpl[T](result, forward, refTime))
        .leftMap(t => Error.Generic(t.getMessage))

    def fitSnapshot[T: TimeLike](
        snapshot: Snapshot,
        forward: Forward[T],
        refTime: T,
        settings: Settings,
        shortMaturityCutoff: T,
        deltaCutoffPuts: Double,
        deltaCutoffCalls: Double
    ): Either[Error, Result] =
      fitPureObservations(
        snapshotToPureObs(
          snapshot,
          forward,
          refTime,
          shortMaturityCutoff,
          deltaCutoffPuts,
          deltaCutoffCalls
        ),
        settings
      )

    def pureLocalVolSurface(result: Result): LocalVolSurface[YearFraction] =
      val slices = result.lvKnots
        .zip(result.lvAtKnots)
        .map:
          case (lvKnots, lvAtKnots) =>
            LinearInterpolation.withFlatExtrapolation(lvKnots, lvAtKnots)
        .toIndexedSeq
      val n = result.expiries.length
      new LocalVolSurface[YearFraction]:
        def apply(time: YearFraction, spot: Double): Double =
          // make it right-continuous in t
          val i = result.expiries.tail.search(time) match
            case Found(foundIndex)              => min(foundIndex + 1, n - 1)
            case InsertionPoint(insertionPoint) => min(insertionPoint, n - 1)
          slices(i)(spot)

    def localVolSurface[T: TimeLike](
        result: Result,
        forward: Forward[T],
        refTime: T
    ): LocalVolSurface[T] =
      val pureLv = pureLocalVolSurface(result)
      new LocalVolSurface[T]:
        def apply(time: T, spot: Double): Double =
          val t = refTime.yearFractionTo(time)
          val f = forward(time)
          val d = forward.dividendFloor(time)
          val x = (spot - d) / (f - d)
          if spot > d then (spot - d) / spot * pureLv(t, x) else 0.0

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

  private def volSurfaceImpl[T: TimeLike](
      result: Result,
      forward: Forward[T],
      refTime: T
  ): VolSurface[T] =
    val pureSurface = pureVolSurfaceImpl(result)
    new VolSurface[T]:
      def apply(expiry: T, strike: Double): Option[Double] =
        val yf = TimeLike[T].yearFractionBetween(refTime, expiry)
        val pureStrike =
          buehler.strikeToPureStrike(strike, forward(expiry), forward.dividendFloor(expiry))
        pureSurface(yf, pureStrike)

  private def pureVolSurfaceImpl(result: Result): VolSurface[YearFraction] =
    val (yfs, interpolatedValues) = pureCallPriceSlices(result)
    val maxT = yfs.last
    def interpolatedVol(expiry: YearFraction, strike: Double) =
      for
        _ <- Either.raiseUnless(expiry.toDouble <= maxT.toDouble)(
          Error.BadInputs(s"cannot extrapolate beyond $maxT")
        )
        _ <- Either.raiseUnless(expiry.toDouble > 0.0)(
          Error.BadInputs(s"expiry must be strictly positive")
        )
        i = yfs.search(expiry).insertionPoint
        t = expiry.toDouble
        t1 = yfs(i - 1).toDouble
        t2 = yfs(i).toDouble
        c1 = interpolatedValues(i - 1)(strike)
        c2 = interpolatedValues(i)(strike)
        vol1 <- black.impliedVol(black.OptionType.Call, strike, t1, 1.0, 1.0, c1)
        vol2 <- black.impliedVol(black.OptionType.Call, strike, t2, 1.0, 1.0, c2)
        tvar1 = vol1 * vol1 * t1
        tvar2 = vol2 * vol2 * t2
        tvar = tvar1 + (tvar2 - tvar1) * (t - t1) / (t2 - t1)
      yield sqrt(tvar / t)

    new VolSurface[YearFraction]:
      def apply(expiry: YearFraction, strike: Double): Option[Double] =
        interpolatedVol(expiry, strike).toOption

  private def pureImpl(
      obs: List[PureObservation],
      settings: Settings
  ): Result =
    val obsByExpiry =
      obs
        .groupBy(_.expiry)
        .toList
        .sortBy(_(0))
        .filter((t, _) => t.toDouble > 0.0)
        .map((t, obs) => t -> obs.toIndexedSeq)

    val expiries = YearFraction.zero :: obsByExpiry.map(_(0))

    val s0 = 1.0
    val spatialGridFactory = SpatialGrid.Factory.logSinh(
      settings.spatialGrid.nPoints,
      settings.spatialGrid.sinhConcentration,
      s0
    )

    val timeGridFactory =
      val s = settings.timeGrid
      TimeGrid.Factory.powerRule(s.alpha, s.beta, s.dtMin, s.dtMax)

    val timegrid = timeGridFactory(expiries.toSet)

    val nKnotsByExpiry = settings.nKnots match
      case Settings.Knots.Fixed(n) => obsByExpiry.map(_ => n)
      case Settings.Knots.Dynamic(minKnots, maxKnots, alpha, minStrikes, minSpread) =>
        obsByExpiry.map: (_, obs) =>
          val nStrikes = obs.map(_.strike).toSet.size
          val nObs = obs.length
          val medianSpread = obs.map(_.spread).sorted.apply(nObs / 2)
          round(
            min(
              maxKnots,
              max(
                minKnots,
                alpha * log(1.0 + max(nStrikes - minStrikes, 0)) / max(
                  medianSpread / minSpread,
                  1.0
                )
              )
            )
          ).toInt

    val state = expiries
      .zip(obsByExpiry)
      .zip(nKnotsByExpiry)
      .foldLeft(State()):
        case (state, ((t1, (t2, obs)), nKnots)) =>
          val timeGridSlice = timegrid.slice(t1, t2).get.yearFractions

          val minStrike = obs.map(_.strike).min
          val maxStrike = obs.map(_.strike).max

          val lvKnots = List
            .tabulate(nKnots)(i =>
              minStrike * pow(maxStrike / minStrike, i.toDouble / (nKnots - 1))
            )
            .toIndexedSeq

          // Our first (very crude) estimate of the grid bounds
          // assumes a log-normal distribution with vol equal
          // to the median implied vol across target strikes.
          // These bounds will be refined iteratively below.
          val medianVol = obs.map(_.vol).sorted.apply(obs.length / 2)
          val sMin =
            val p =
              SpatialGrid.lognormalPercentile(
                medianVol,
                t2.toDouble,
                0,
                settings.spatialGrid.quantile
              )
            state.grids.headOption.fold(p) { grid =>
              min(grid(0), p)
            }

          val sMax =
            val p =
              SpatialGrid.lognormalPercentile(
                medianVol,
                t2.toDouble,
                0,
                1 - settings.spatialGrid.quantile
              )
            state.grids.headOption.fold(p) { grid =>
              max(grid(grid.length - 1), p)
            }

          // Recursive b/c once we have computed the call prices
          // based on the given estimate of the grid bounds,
          // we use the implied distribution to infer whether
          // the grid needs to be widened to accomodate the
          // target quantile, in which case we recurse.
          @tailrec
          def go(sMin: Double, sMax: Double, iter: Int): State = {

            if (iter > settings.spatialGrid.expansionMaxIters) {
              throw new Error.BadNumerics(
                s"max grid bound expansion iters (${settings.spatialGrid.expansionMaxIters}) exceeded"
              )
            }

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

              (timeGridSlice zip timeGridSlice.tail).zipWithIndex
                .foldLeft(initialInteriorValues):
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
                vog,
                BoundaryCondition.Linear,
                BoundaryCondition.Linear
              )
              val spline = CubicSpline.natural(grid, terminalVals)
              val ivols = obs
                .map(obs =>
                  // Might fail when price is below intrinsic due
                  // to numerical inaccuracy, in which case we return 0.
                  black
                    .impliedVol(
                      black.OptionType.Call,
                      obs.strike,
                      t2.toDouble,
                      1.0,
                      1.0,
                      spline(obs.strike)
                    )
                    .toOption
                    .getOrElse(0.0)
                )
                .toIndexedSeq
              ivols

            val guess = IndexedSeq.fill(lvKnots.length)(medianVol)

            val optResult = LevenbergMarquardt
              .optimize(
                objectiveFun,
                lvKnots.length,
                obs.length,
                obs.map(_.vol).toIndexedSeq,
                IndexedSeq.fill(lvKnots.length)(settings.minLv),
                IndexedSeq.fill(lvKnots.length)(settings.maxLv),
                guess,
                obs.map(m => 1.0 / (m.spread * m.spread)).toIndexedSeq,
                0.001,
                0.0001
              )
              .toTry
              .get

            val lvAtKnots = optResult.optimum

            val vals =
              SpatialGrid
                .addBoundaryValues(
                  grid,
                  interiorValuesOnGrid(lvAtKnots),
                  BoundaryCondition.Linear,
                  BoundaryCondition.Linear
                )
                .toIndexedSeq

            // Check that the grid bounds are wide enough to accomodate the
            // desired grid quantile. If not then expand bounds and try again.
            val (newLbMaybe, newUbMaybe) =
              // A cubic spline fitted to the call values at time t2,
              // the first derivative of which essentially gives us the CDF
              // of the implied distribution.
              val spline = CubicSpline.natural(grid, vals)
              val n = grid.length
              val k = max(1, (settings.spatialGrid.tailFraction * n).toInt)

              // lower grid tail
              val lgt = grid.take(k)

              // upper grid tail
              val ugt = grid.takeRight(k)

              // estimate of lower tail quantile
              val lowerQ =
                exp(lgt.map(x => log(1.0 + spline.fstDerivative(x))).sum / lgt.length)

              // estimate of upper tail quantile
              val upperQ = exp(ugt.map(x => log(-spline.fstDerivative(x))).sum / ugt.length)

              val alpha = settings.spatialGrid.expansionFactor
              (lowerQ > settings.spatialGrid.quantile).guard[Option].as(sMin * (1 - alpha)) ->
                (upperQ > settings.spatialGrid.quantile).guard[Option].as(sMax * (1 + alpha))

            (newLbMaybe, newUbMaybe) match {
              case (Some(lb), Some(ub)) => go(lb, ub, iter + 1)
              case (Some(lb), None)     => go(lb, sMax, iter + 1)
              case (None, Some(ub))     => go(sMin, ub, iter + 1)
              case (None, None) =>
                State(
                  grid :: state.grids,
                  vals :: state.values,
                  lvKnots :: state.lvKnots,
                  lvAtKnots :: state.lvAtKnots,
                  (sMin, sMax) :: state.gridBounds
                )
            }

          }

          go(sMin, sMax, 0)

    Result(
      expiries,
      state.gridBounds.reverse,
      state.lvKnots.reverse,
      state.lvAtKnots.reverse,
      settings
    )

  private def snapshotToPureObs[T: TimeLike](
      snapshot: Snapshot,
      forward: Forward[T],
      refTime: T,
      shortMaturityCutoff: T,
      deltaCutoffPuts: Double = -0.05,
      deltaCutoffCalls: Double = 0.05
  ): List[PureObservation] =
    snapshot.quotes
      .groupBy(_.expiry)
      .map: (expiryDate, quotes) =>
        // TODO: use exact expiry times by date/exchange
        val expiry = expiryDate.atTime(16, 0).atZone(snapshot.expiryZone).toInstant
        val timeToMaturity = TimeLike[java.time.Instant]
          .yearFractionBetween(snapshot.timestamp.toInstant, expiry)
        val t = refTime.plusYearFraction(timeToMaturity)
        val f = forward(t)
        val filteredQuotes = quotes.filter: quote =>
          val otm =
            (quote.isPut && quote.strike < f) || (quote.isCall && quote.strike > f)
          val delta = (quote.isPut && quote.delta.exists(
            _ < deltaCutoffPuts
          )) || (quote.isCall && quote.delta.exists(_ > deltaCutoffCalls))
          otm && delta

        t -> filteredQuotes
      .filter((t, _) => t > shortMaturityCutoff)
      .toList
      .flatMap: (t, quotes) =>
        quotes
          .map: quote =>
            val pureStrike = buehler.strikeToPureStrike(
              quote.strike.toDouble,
              forward(t),
              forward.dividendFloor(t)
            )
            for
              iv <- quote.impliedVol
              bid <- iv.bid
              mid <- iv.mid
              ask <- iv.ask
            yield PureObservation(
              pureStrike,
              refTime.yearFractionTo(t),
              mid,
              max(ask - bid, 0.0)
            )
          .flatten

  private def pureDensityImpl(result: Result): Density[YearFraction] =
    val (times, prices) = pureCallPriceSlices(result)
    val maxT = times.last
    def interpolatedDensity(time: YearFraction, spot: Double) =
      for
        _ <- Either.raiseUnless(time.toDouble <= maxT.toDouble)(
          Error.BadInputs(s"cannot extrapolate beyond $maxT")
        )
        _ <- Either.raiseUnless(time.toDouble > 0.0)(
          Error.BadInputs(s"time must be strictly positive")
        )
        i = times.search(time).insertionPoint
        t = time.toDouble
        t1 = times(i - 1).toDouble
        t2 = times(i).toDouble
        c1 = prices(i - 1).sndDerivative(spot)
        c2 = prices(i).sndDerivative(spot)
      yield c1 + (t - t1) * (c2 - c1) / (t2 - t1)

    new Density[YearFraction]:
      def apply(time: YearFraction, spot: Double): Double =
        interpolatedDensity(time, spot).toOption.getOrElse(0.0)

  private def pureCdfImpl(result: Result): Cdf[YearFraction] =
    val (times, prices) = pureCallPriceSlices(result)
    val maxT = times.last
    def interpolatedCdf(time: YearFraction, spot: Double) =
      for
        _ <- Either.raiseUnless(time.toDouble <= maxT.toDouble)(
          Error.BadInputs(s"cannot extrapolate beyond $maxT")
        )
        _ <- Either.raiseUnless(time.toDouble > 0.0)(
          Error.BadInputs(s"time must be strictly positive")
        )
        i = times.search(time).insertionPoint
        t = time.toDouble
        t1 = times(i - 1).toDouble
        t2 = times(i).toDouble
        c1 = prices(i - 1).fstDerivative(spot) + 1.0
        c2 = prices(i).fstDerivative(spot) + 1.0
      yield c1 + (t - t1) * (c2 - c1) / (t2 - t1)

    new Cdf[YearFraction]:
      def apply(time: YearFraction, spot: Double): Option[Double] =
        interpolatedCdf(time, spot).toOption

  private def pureCallPriceSlices(
      result: Result
  ): (IndexedSeq[YearFraction], IndexedSeq[CubicSpline]) =
    val expiries = result.expiries
    val settings = result.settings
    val s0 = 1.0
    val spatialGridFactory = SpatialGrid.Factory.logSinh(
      settings.spatialGrid.nPoints,
      settings.spatialGrid.sinhConcentration,
      s0
    )
    val timeGridFactory =
      val s = settings.timeGrid
      TimeGrid.Factory.powerRule(s.alpha, s.beta, s.dtMin, s.dtMax)
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
            (state.grids.headOption, state.values.headOption.flatMap(_.lastOption)).tupled.fold(
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

          State2(
            grid :: state.grids,
            // for all but the first expiry, the initial values are
            // a duplicate of the final values of the previous expiry
            // (modulo interpolation onto a new grid)
            (if state.isFirstExpiry then values else values.tail) :: state.values
          )

    val interpolatedValues = state.grids
      .zip(state.values)
      .reverse
      .flatMap((grid, vals) => vals.map(CubicSpline.natural(grid, _)))
      .toIndexedSeq

    // assert(timegrid.length == interpolatedValues.length)

    val yfs = timegrid.yearFractions
    yfs -> interpolatedValues
