package examples
package example5

import derifree.*
import derifree.syntax.*

@main def run: Unit =
  val refTime = YearFraction.zero
  val expiry = YearFraction.oneYear
  val spot = 100.0
  val vol = 0.30
  val divs = Dividend(refTime.plusDays(180), 3.0, 0.02) :: Nil
  val discount = YieldCurve.fromContinuouslyCompoundedRate(0.05.rate, refTime)
  val borrow = YieldCurve.fromContinuouslyCompoundedRate(0.0.rate, refTime)
  val forward = Forward(spot, divs, discount, borrow)

  val payout = (s: Double) => math.max(0.0, s / spot - 1)

  val solution = fd.feynmankac.blackScholes(
    forward,
    discount,
    vol,
    expiry,
    payout,
    fd.BoundaryCondition.Linear,
    fd.BoundaryCondition.Linear,
    refTime
  )

  println(solution)
