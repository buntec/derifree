package examples
package example5

import derifree.*
import derifree.syntax.*
import derifree.fd.SpatialGrid

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

  val tgFactory = TimeGrid.Factory.almostEquidistant(YearFraction.oneDay)
  val sgFactory = SpatialGrid.Factory.logSinh(100, 0.01, spot)

  val solution = fd.feynmankac.blackScholes(
    forward,
    discount,
    vol,
    expiry,
    payout,
    refTime,
    fd.BoundaryCondition.Linear,
    fd.BoundaryCondition.Linear,
    tgFactory,
    sgFactory
  )

  println(solution)
