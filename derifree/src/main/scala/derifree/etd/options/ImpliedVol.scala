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
      fd.SpatialGrid.Factory.logSinh(100, 1, strike),
      fd.Settings.default
    )

    RootFinding.brent(
      vol => priceForVol(vol) - price,
      0.0001,
      10.0,
      RootFinding.Settings(absAccuracy = 0.00001, maxIters = 20)
    )
