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

class DupireSuite extends munit.FunSuite:

  private case class Case(
      maxExpiry: YearFraction,
      vol: Double,
      discountRate: Double,
      borrowRate: Double
  ) derives Show

  private case class InnerCase(
      expiry: YearFraction,
      strike: Double
  ) derives Show

  private val genCase: Gen[Case] = for
    tte <- Gen.between(0.01, 1.0).map(YearFraction(_))
    dRate <- Gen.between(-0.1, 0.1)
    bRate <- Gen.between(-0.1, 0.1)
    vol <- Gen.between(0.05, 0.8)
  yield Case(tte, vol, dRate, bRate)

  private def genInnerCase(tMax: YearFraction): Gen[InnerCase] = for
    tte <- Gen.between(0.01, tMax.toDouble).map(YearFraction(_))
    strike <- Gen.between(70.0, 150.0)
  yield InnerCase(tte, strike)

  test("should be close to closed-form price"):
    val refTime = YearFraction.zero
    val spot = 100.0
    val divs = Nil // Dividend(refTime.plusDays(180), 3.0, 0.02) :: Nil

    genCase.view.zipWithIndex
      .take(100)
      .foreach:
        case (c, i) =>
          val maxExpiry = c.maxExpiry
          val vol = c.vol
          val discount = YieldCurve.fromContinuouslyCompoundedRate(c.discountRate.rate, refTime)
          val borrow = YieldCurve.fromContinuouslyCompoundedRate(c.borrowRate.rate, refTime)
          val forward = Forward(spot, divs, discount, borrow)

          val tgFactory = TimeGrid.Factory.powerRule(
            0.5,
            0.01,
            1.0 / 365 / 5,
            10.0 / 365
          )

          val callPriceSurface = dupire.blackScholesCallPrices(
            forward,
            discount,
            vol,
            refTime,
            maxExpiry,
            tgFactory,
            Settings(1e-5, 2)
          )

          genInnerCase(c.maxExpiry).view.zipWithIndex
            .take(100)
            .foreach: (ic, j) =>
              val strike = ic.strike
              val expiry = ic.expiry

              val refPrice = black.price(
                black.OptionType.Call,
                strike,
                expiry.toDouble,
                vol,
                forward(expiry),
                discount.discountFactor(expiry)
              )

              val priceVolUp = black.price(
                black.OptionType.Call,
                strike,
                expiry.toDouble,
                vol + 0.01,
                forward(expiry),
                discount.discountFactor(expiry)
              )

              val vega = priceVolUp - refPrice

              val price = callPriceSurface(strike, ic.expiry)

              val hint =
                s"i=$i, j=$j, price=$price, ref=$refPrice, vega=$vega, vol=$vol, T=$expiry, strike=$strike, Tmax=$maxExpiry"

              assertEqualsDouble(price, refPrice, 1e-2, hint)

              // check implied-vol only if vega is above threshold
              if (vega > 0.1) {
                val ivol = black
                  .impliedVol(
                    black.OptionType.Call,
                    strike,
                    expiry.toDouble,
                    forward(expiry),
                    discount.discountFactor(expiry),
                    price
                  )
                  .toTry
                  .get
                val hint =
                  s"i=$i, j=$j, price=$price, ref=$refPrice, vega=$vega, vol=$vol, ivol=$ivol, T=$expiry, strike=$strike, Tmax=$maxExpiry"
                assertEqualsDouble(ivol, vol, 0.0001, hint)
              }
