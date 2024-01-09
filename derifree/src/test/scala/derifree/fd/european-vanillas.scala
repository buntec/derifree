package derifree
package fd

import derifree.syntax.*
import cats.syntax.all.*
import cats.Show
import cats.derived.*

class EuropeanVanillaSuite extends munit.FunSuite:

  private case class Case(
      timeToExpiry: YearFraction,
      strike: Double,
      vol: Double,
      isCall: Boolean,
      discountRate: Double,
      borrowRate: Double
  ) derives Show

  private val genCase: Gen[Case] = for
    tte <- Gen.between(0.01, 3.0).map(YearFraction(_))
    isCall <- Gen.boolean
    strike <- Gen.between(70.0, 150.0)
    dRate <- Gen.between(-0.1, 0.1)
    bRate <- Gen.between(-0.1, 0.1)
    vol <- Gen.between(0.05, 0.8)
  yield Case(tte, strike, vol, isCall, dRate, bRate)

  test("should be close to closed-form price"):

    val refTime = YearFraction.zero
    val spot = 100.0
    val divs = Nil // Dividend(refTime.plusDays(180), 3.0, 0.02) :: Nil

    genCase.view.zipWithIndex
      .take(1000)
      .foreach: (c, i) =>
        val expiry = c.timeToExpiry
        val strike = c.strike
        val vol = c.vol
        val discount = YieldCurve.fromContinuouslyCompoundedRate(c.discountRate.rate, refTime)
        val borrow = YieldCurve.fromContinuouslyCompoundedRate(c.borrowRate.rate, refTime)
        val forward = Forward(spot, divs, discount, borrow)

        val tgFactory = TimeGrid.Factory.almostEquidistant(YearFraction.oneDay)
        val sgFactory = SpatialGrid.Factory.logSinh(300, 0.01, strike)

        val payoff =
          payoffs.EuropeanVanilla(
            strike,
            expiry,
            if c.isCall then payoffs.EuropeanVanilla.OptionType.Call
            else payoffs.EuropeanVanilla.OptionType.Put
          )

        val price = fd.feynmankac.blackScholes(
          payoff,
          forward,
          discount,
          vol,
          refTime,
          tgFactory,
          sgFactory,
          Settings.default
        )

        val timeToMat = refTime.yearFractionTo(expiry)
        val refPrice = black.price(
          if c.isCall then black.OptionType.Call else black.OptionType.Put,
          strike,
          timeToMat.toDouble,
          vol,
          forward(timeToMat),
          discount.discountFactor(timeToMat)
        )

        val clue = show"i=$i, price=$price, ref=$refPrice, case=$c"
        println(clue)

        assertEqualsDouble(price, refPrice, 0.01, clue)
