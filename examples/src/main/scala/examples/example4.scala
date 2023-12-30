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
package example4

import derifree.*
import derifree.syntax.*

val dsl = Dsl[YearFraction]

import dsl.*

val udl = "ACME"

val refTime = YearFraction.zero
val expiry = YearFraction.oneYear

val europeanPut =
  for
    s0 <- spot(udl, refTime)
    s <- spot(udl, expiry)
    _ <- cashflow(max(1 - s / s0, 0), Ccy.USD, expiry)
  yield ()

val europeanCall =
  for
    s0 <- spot(udl, refTime)
    s <- spot(udl, expiry)
    _ <- cashflow(max(s / s0 - 1, 0), Ccy.USD, expiry)
  yield ()

@main def run: Unit =
  val dirNums = Sobol.directionNumbers(1000).toTry.get

  val spot = 100.0
  val vol = 0.30.vol
  val divs = Dividend(refTime.plusDays(180), 3.0, 0.02) :: Nil
  val discount = YieldCurve.fromContinuouslyCompoundedRate(0.05.rate, refTime)
  val borrow = YieldCurve.zero[YearFraction]
  val forwards = Map(udl -> Forward(spot, divs, discount, borrow))
  val vols = Map(udl -> vol)

  val sim: Simulator[YearFraction] =
    Simulator.blackScholes[YearFraction](
      TimeGrid.Factory.almostEquidistant(YearFraction.oneDay),
      NormalGen.Factory.sobol(dirNums),
      refTime,
      forwards,
      vols,
      Map.empty,
      discount
    )

  val nSims = (1 << 15) - 1

  def printResult(cc: ContingentClaim): Unit =
    val t1 = System.nanoTime()
    val result = cc.fairValueResult(sim, nSims).toTry.get
    val t2 = System.nanoTime()
    println(
      f"price = ${result.fairValue}, duration = ${(t2 - t1) * 1e-6}%.0f ms"
    )

  printResult(europeanPut)
  printResult(europeanCall)
