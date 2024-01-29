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
package etd
package options

import derifree.math.RootFinding

object ImpliedVol:

  /** In the presence of cash dividends this returns Buehler's "pure" vol. */
  def european[T: TimeLike](
      price: Double,
      strike: Double,
      expiry: T,
      settle: T,
      isCall: Boolean,
      refTime: T,
      forward: Forward[T],
      discount: YieldCurve[T]
  ): Either[Error, Double] =
    val pureStrike =
      buehler.strikeToPureStrike(strike, forward(expiry), forward.dividendFloor(expiry))
    val df = discount.discountFactor(settle)
    val purePrice = price / df / (forward(expiry) - forward.dividendFloor(expiry))
    black.impliedVol(
      if isCall then black.OptionType.Call else black.OptionType.Put,
      pureStrike,
      TimeLike[T].yearFractionBetween(refTime, expiry).toDouble,
      1.0,
      1.0,
      purePrice
    )

  /** In the presence of cash dividends this returns Buehler's "pure" vol. */
  def american[T: TimeLike](
      price: Double,
      strike: Double,
      expiry: T,
      isCall: Boolean,
      refTime: T,
      forward: Forward[T],
      discount: YieldCurve[T]
  ): Either[Error, Double] =

    val payoff = payoffs.AmericanVanilla(
      "ACME", // ignored for finite-difference method
      Ccy.USD, // ditto
      strike,
      expiry,
      if isCall then payoffs.AmericanVanilla.OptionType.Call
      else payoffs.AmericanVanilla.OptionType.Put
    )

    def priceForVol(vol: Double) = fd.feynmankac.blackScholes(
      payoff,
      forward,
      discount,
      vol,
      refTime,
      TimeGrid.Factory.almostEquidistant(YearFraction.oneDay),
      fd.SpatialGrid.Factory.logSinh(200, 1, strike),
      fd.Settings.default
    )

    // The corresponding European option price is equal to or lower than
    // the American price and therefore, for the same price, the implied
    // vol of the European option is an upper bound to the
    // implied vol of the American option. We use a generous fudge factor
    // to account for numerical inaccuracy.
    european(price, strike, expiry, expiry, isCall, refTime, forward, discount).flatMap(
      europeanVol =>
        RootFinding.brent(
          vol => priceForVol(vol) - price,
          0.001,
          1.1 * europeanVol,
          RootFinding.Settings(absAccuracy = 0.0001, maxIters = 20)
        )
    )
