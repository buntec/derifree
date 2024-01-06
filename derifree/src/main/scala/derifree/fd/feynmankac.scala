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
  ): ArraySeq[Double] =
    val specialTimes = expiry :: forward.dividends.map(_.exDiv)
    val specialYfs = specialTimes.map(t => TimeLike[T].yearFractionBetween(refTime, t)).toSet
    val timegrid = TimeGrid.almostEquidistant(YearFraction.oneDay, specialYfs)
    val tMax = specialYfs.max

    val s0 = forward.spot
    val sMin = s0 * spatialgrid.lognormalPercentile(vol, tMax.toDouble, 0, 1e-5)
    val sMax = s0 * spatialgrid.lognormalPercentile(vol, tMax.toDouble, 0, 1 - 1e-5)
    val grid = spatialgrid.logSinh(sMin, sMax, 100, 0.01, s0)

    val n = grid.length - 2 // number of interior points
    val interiorPoints = grid.slice(1, n + 1)

    val v0 = interiorPoints.map(payout)

    val ts = timegrid.yearFractions

    val solutions = (ts zip ts.tail).foldRight(List(v0)) { case ((yf1, yf2), vs) =>
      val dt = (yf2 - yf1).toDouble
      val t1 = refTime.plusYearFraction(yf1)
      val t2 = refTime.plusYearFraction(yf2)

      val r = -math.log(discount.discountFactor(t1, t2)) / dt
      val mu = math.log(forward(t2) / forward(t1)) / dt

      val reaction = Array.fill(n)(r)
      val convection = interiorPoints.map(_ * mu)
      val diffusion = interiorPoints.map(s => 0.5 * s * s * vol * vol)

      val op = operator.withLinearityBoundaryCondition(
        grid,
        convection,
        diffusion,
        ArraySeq.unsafeWrapArray(reaction)
      )

      val v = timestep.`implicit`(timestep.explicit(vs.head, 0.5 * dt, op), 0.5 * dt, op)

      v :: vs
    }
    solutions.head
