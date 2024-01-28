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

import cats.Show
import cats.derived.*
import cats.syntax.all.*
import derifree.fd.*
import derifree.syntax.*

class McVsFd extends munit.FunSuite:

  case class Case(
      timeToExpiry: YearFraction,
      strike: Double,
      vol: Double,
      isCall: Boolean,
      discountRate: Double,
      borrowRate: Double
  ) derives Show

  val genCase: Gen[Case] = for
    tte <- Gen.between(0.01, 1.0).map(YearFraction(_))
    isCall <- Gen.boolean
    strike <- Gen.between(70.0, 150.0)
    dRate <- Gen.between(-0.1, 0.1)
    bRate <- Gen.between(-0.05, 0.05)
    vol <- Gen.between(0.05, 0.8)
  yield Case(tte, strike, vol, isCall, dRate, bRate)

  private val genCaseFix: Gen[Case] = for
    tte <- Gen.constant(0.25).map(YearFraction(_))
    isCall <- Gen.constant(true)
    strike <- Gen.constant(100.0)
    dRate <- Gen.constant(0.0)
    bRate <- Gen.constant(0.0)
    vol <- Gen.constant(0.3)
  yield Case(tte, strike, vol, isCall, dRate, bRate)

  val refTime = YearFraction.zero

  val dirNums = Sobol.directionNumbers(1500).toTry.get

  val nSims = (1 << 15) - 1

  val udl = "ACME"
  val ccy = Ccy.USD

  // TODO: find reasonable tolerances for American MC
  test("Vanilla prices should be close".ignore):

    val spot = 100.0
    val divs = Nil // Dividend(refTime.plusDays(180), 3.0, 0.02) :: Nil

    genCaseFix.view.zipWithIndex
      .take(1)
      // .filter((_, i) => i == 0) // debug specific case
      .foreach: (c, i) =>
        val expiry = c.timeToExpiry
        val strike = c.strike
        val vol = c.vol
        val discount = YieldCurve.fromContinuouslyCompoundedRate(c.discountRate.rate, refTime)
        val borrow = YieldCurve.fromContinuouslyCompoundedRate(c.borrowRate.rate, refTime)
        val forward = Forward(spot, divs, discount, borrow)

        val tgFactory = TimeGrid.Factory.almostEquidistant(YearFraction.oneDay)
        val sgFactory = SpatialGrid.Factory.logSinh(300, 0.01, strike)

        val assets = models.blackscholes.Asset(udl, Ccy.USD, forward, vol.vol) :: Nil

        val sim = models.blackscholes.simulator(
          TimeGrid.Factory.almostEquidistant(YearFraction.oneDay),
          NormalGen.Factory.sobol(dirNums),
          refTime,
          assets,
          Map.empty,
          discount,
          Map.empty
        )

        val dsl = Dsl[YearFraction]

        val euVanilla =
          payoffs.EuropeanVanilla(
            udl,
            ccy,
            strike,
            expiry,
            if c.isCall then payoffs.EuropeanVanilla.OptionType.Call
            else payoffs.EuropeanVanilla.OptionType.Put
          )

        val amVanilla =
          payoffs.AmericanVanilla(
            udl,
            ccy,
            strike,
            expiry,
            if c.isCall then payoffs.AmericanVanilla.OptionType.Call
            else payoffs.AmericanVanilla.OptionType.Put
          )

        val euRefPrice = black.price(
          if c.isCall then black.OptionType.Call else black.OptionType.Put,
          strike,
          c.timeToExpiry.toDouble,
          vol,
          forward(expiry),
          discount.discountFactor(expiry)
        )

        val euPriceMc = euVanilla.contingentClaim(refTime, dsl).fairValue(sim, nSims).toTry.get

        val amPriceMcResult =
          amVanilla.contingentClaim(refTime, dsl).fairValueResult(sim, nSims).toTry.get

        val euPriceFd = fd.feynmankac.blackScholes(
          euVanilla,
          forward,
          discount,
          vol,
          refTime,
          tgFactory,
          sgFactory,
          Settings.default
        )

        val amPriceFd = fd.feynmankac.blackScholes(
          amVanilla,
          forward,
          discount,
          vol,
          refTime,
          tgFactory,
          sgFactory,
          Settings.default
        )

        println(
          show"""i=$i
          |case=$c
          |euRefPrice=$euRefPrice
          |euPriceMc=$euPriceMc
          |euPriceFd=$euPriceFd
          |amPriceMc=${amPriceMcResult.fairValue} (exercise probs=${amPriceMcResult.putProbabilities.toList
                 .sortBy(_(1))}, total=${amPriceMcResult.putProbabilities.toList.map(_(1)).sum})
          |amPriceFd=$amPriceFd""".stripMargin
        )
