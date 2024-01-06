package derifree
package fd

import derifree.syntax.*

import scala.collection.immutable.ArraySeq

object feynmankac:

  def blackScholesWithLinearBoundaryConditions[T: TimeLike](
      forward: Forward[T],
      discount: YieldCurve[T],
      vol: Double,
      expiry: T,
      payout: Double => Double,
      refTime: T
  ): Double =
    val specialTimes = expiry :: forward.dividends.map(_.exDiv)
    val specialYfs = specialTimes.map(t => TimeLike[T].yearFractionBetween(refTime, t)).toSet
    val timegrid = TimeGrid.almostEquidistant(YearFraction.oneDay, specialYfs)
    val tMax = specialYfs.max

    val divsByIndex = forward.dividends
      .groupBy(_.exDiv)
      .map((t, divs) =>
        t -> divs.reduce((d1, d2) =>
          d1.copy(cash = d1.cash + d2.cash, prop = d1.prop + d2.prop)
        )
      )
      .map((t, div) => timegrid.indexOf(TimeLike[T].yearFractionBetween(refTime, t)).get -> div)

    val s0 = forward.spot
    val sMin = s0 * spatialgrid.lognormalPercentile(vol, tMax.toDouble, 0, 1e-5)
    val sMax = s0 * spatialgrid.lognormalPercentile(vol, tMax.toDouble, 0, 1 - 1e-5)

    val ts = timegrid.yearFractions
    val tEps = TimeGrid.tickSize

    val initialGrid = spatialgrid.logSinh(sMin, sMax, 50, 0.01, s0)
    val initialInteriorPoints = initialGrid.slice(1, initialGrid.length - 1)
    val initialValues = initialInteriorPoints.map(payout)

    val (finalGrid, finalValues) =
      (ts zip (ts.zipWithIndex).tail).foldRight((initialGrid, initialValues)):
        case ((yf1, (yf2, i)), (grid, v2)) =>
          val dt = (yf2 - yf1).toDouble
          val t1 = refTime.plusYearFraction(yf1 + tEps)
          val t2 = refTime.plusYearFraction(yf2 - tEps)

          val r = -math.log(discount.discountFactor(t1, t2)) / dt
          val mu = math.log(forward(t2) / forward(t1)) / dt
          // println(s"dt=$dt, t1=$t1, t2=$t2, r=$r, mu=$mu")

          val n = grid.length - 2 // number of interior points
          val interiorPoints = grid.slice(1, n + 1)

          val gridNext =
            divsByIndex.get(i).fold(grid)(div => grid.map(s => (s + div.cash) / (1 - div.prop)))

          val reaction = interiorPoints.map(_ => r)
          val convection = interiorPoints.map(_ * mu)
          val diffusion = interiorPoints.map(s => 0.5 * s * s * vol * vol)

          val op =
            operator.withLinearityBoundaryCondition(gridNext, convection, diffusion, reaction)

          val v1 = timestep.`implicit`(timestep.explicit(v2, 0.5 * dt, op), 0.5 * dt, op)
          (gridNext, v1)

    val finalInteriorPoints = finalGrid.slice(1, finalGrid.length - 1)
    interpolation.naturalCubicSpline(finalInteriorPoints, finalValues)(s0)
