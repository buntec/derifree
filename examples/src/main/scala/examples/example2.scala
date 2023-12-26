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

package example2

import derifree.*
import derifree.syntax.*

val dsl = Dsl[YearFraction]

import dsl.*

val refTime = YearFraction.zero
val expiry = YearFraction.oneYear
val settle = expiry + YearFraction.oneDay * 2

val worstOfDip = for
  s1_0 <- spot("AAPL", refTime)
  s2_0 <- spot("META", refTime)
  s1 <- spot("AAPL", expiry)
  s2 <- spot("META", expiry)
  pHit <- hitProb(
    Barrier.Continuous(
      Barrier.Direction.Down,
      Map("AAPL" -> 0.8 * s1_0, "META" -> 0.8 * s2_0),
      from = refTime,
      to = expiry,
      Barrier.Policy.Or
    )
  )
  _ <- cashflow(pHit * max(0, 1 - min(s1 / s1_0, s2 / s2_0)), settle)
yield ()

@main def run: Unit =
  val dirNums = Sobol.directionNumbers(5000).toTry.get

  val spots = Map("AAPL" -> 195.0, "META" -> 330.0)
  val vols = Map("AAPL" -> 0.25.vol, "META" -> 0.3.vol)
  val correlations = Map(("AAPL", "META") -> 0.7)
  val rate = 0.04.rate

  val sim: Simulator[YearFraction] =
    Simulator.blackScholes[YearFraction](
      TimeGrid.Factory.almostEquidistant(YearFraction.oneDay),
      NormalGen.Factory.sobol(dirNums),
      refTime,
      spots,
      vols,
      correlations,
      rate
    )

  val nSims = (1 << 16) - 1

  (0 until 1000).foreach: _ =>
    val t1 = System.nanoTime()
    val price = worstOfDip.fairValue(sim, nSims)
    val t2 = System.nanoTime()
    println(f"price = $price, duration = ${(t2 - t1) * 1e-6}%.0f ms")
