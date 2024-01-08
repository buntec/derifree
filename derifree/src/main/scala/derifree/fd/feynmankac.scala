package derifree
package fd

import derifree.syntax.*

import scala.collection.immutable.ArraySeq

object feynmankac:

  def blackScholes[T: TimeLike](
      forward: Forward[T],
      discount: YieldCurve[T],
      vol: Double,
      expiry: T,
      payout: Double => Double,
      lowerBoundary: BoundaryCondition,
      upperBoundary: BoundaryCondition,
      refTime: T
  ): Double =
    val specialTimes = expiry :: forward.dividends.map(_.exDiv)
    val specialYfs = specialTimes.map(t => refTime.yearFractionTo(t)).toSet
    val timegrid = TimeGrid.almostEquidistant(YearFraction.oneDay, specialYfs)
    val tMax = specialYfs.max

    val divsByIndex = forward.dividends
      .groupBy(_.exDiv)
      .map((t, divs) =>
        t -> divs.reduce((d1, d2) =>
          d1.copy(cash = d1.cash + d2.cash, prop = d1.prop + d2.prop)
        )
      )
      .map((t, div) => timegrid.indexOf(refTime.yearFractionTo(t)).get -> div)

    val s0 = forward.spot

    val sMin =
      lowerBoundary match
        case BoundaryCondition.Linear =>
          s0 * spatialgrid.lognormalPercentile(vol, tMax.toDouble, 0, 1e-5)
        case BoundaryCondition.Dirichlet(spot, _) =>
          spot

    val sMax = upperBoundary match
      case BoundaryCondition.Linear =>
        s0 * spatialgrid.lognormalPercentile(vol, tMax.toDouble, 0, 1 - 1e-5)
      case BoundaryCondition.Dirichlet(spot, _) =>
        spot

    val ts = timegrid.yearFractions
    val tEps = TimeGrid.tickSize

    val initialGrid = spatialgrid.logSinh(sMin, sMax, 50, 0.01, s0)
    val initialInteriorPoints = initialGrid.slice(1, initialGrid.length - 1)
    val initialValues = initialInteriorPoints.map(payout)

    val (finalGrid, finalValues) =
      (ts zip (ts.zipWithIndex).tail).reverse.zipWithIndex
        .foldLeft((initialGrid, initialValues)):
          case ((grid, v2), ((yf1, (yf2, i)), j)) =>
            val dt = (yf2 - yf1).toDouble
            val t1 = refTime.plusYearFraction(yf1 + tEps)
            val t2 = refTime.plusYearFraction(yf2 - tEps)

            val r = -math.log(discount.discountFactor(t1, t2)) / dt
            val mu = math.log(forward(t2) / forward(t1)) / dt

            val n = grid.length - 2 // number of interior points
            val interiorPoints = grid.slice(1, n + 1)

            val gridNext =
              divsByIndex
                .get(i)
                .fold(grid)(div => grid.map(s => (s + div.cash) / (1 - div.prop)))

            val reaction = interiorPoints.map(_ => r)
            val convection = interiorPoints.map(_ * mu)
            val diffusion = interiorPoints.map(s => 0.5 * s * s * vol * vol)

            val op =
              Operator(gridNext, convection, diffusion, reaction, upperBoundary, lowerBoundary)

            val v1 =
              if j < 2 then
                // Rannacher smoothing
                op.implicitStep(op.implicitStep(v2, 0.5 * dt), 0.5 * dt)
              else
                // Crank-Nicolson
                op.thetaStep(v2, dt, 0.5)

            (gridNext, v1)

    val finalInteriorPoints = finalGrid.slice(1, finalGrid.length - 1)
    interpolation.naturalCubicSpline(finalInteriorPoints, finalValues)(s0)
