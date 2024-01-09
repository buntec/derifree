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

package examples
package example5

import derifree.*
import derifree.fd.*
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

  val tgFactory = TimeGrid.Factory.almostEquidistant(YearFraction.oneDay)
  val sgFactory = SpatialGrid.Factory.logSinh(100, 1, spot)

  val payoff = payoffs.EuropeanVanilla(spot, expiry, payoffs.EuropeanVanilla.OptionType.Call)

  val solution = fd.feynmankac.blackScholes(
    payoff,
    forward,
    discount,
    vol,
    refTime,
    tgFactory,
    sgFactory,
    Settings.default
  )

  println(solution)
