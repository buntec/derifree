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

import cats.Show
import cats.derived.*
import cats.syntax.all.*
import derifree.syntax.*

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

    val statsBuilder = genCase.view.zipWithIndex
      .take(1000)
      .foldLeft(SummaryStatistics.builder):
        case (statsBuilder, (c, i)) =>
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
          // println(clue)

          val diff = math.abs(price - refPrice)
          assertEqualsDouble(price, refPrice, 0.01, clue)
          statsBuilder.add(diff)

    val stats = statsBuilder.build
    println(stats.summaryString)
