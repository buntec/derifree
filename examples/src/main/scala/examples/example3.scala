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
package example3

import cats.syntax.all.*
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

val americanPut =
  val m = 50
  for
    s0 <- spot(udl, refTime)
    s <- spot(udl, expiry)
    _ <- List
      .tabulate(m)(i => (expiry / m) * i)
      .drop(1)
      .traverse(t =>
        spot(udl, t).flatMap(s_t =>
          puttable(max(1 - s_t / s0, 0.0).some.filter(_ > 0), Ccy.USD, t)
        )
      )
    _ <- cashflow(max(1 - s / s0, 0), Ccy.USD, expiry)
  yield ()

@main def run: Unit =
  val dirNums = Sobol.directionNumbers(1000).toTry.get

  val spots = Map(udl -> 100.0)
  val vols = Map(udl -> 0.30.vol)
  val rate = 0.05.rate

  val sim = models.blackscholes.simulator(
    TimeGrid.Factory.almostEquidistant(YearFraction.oneDay),
    NormalGen.Factory.sobol(dirNums),
    refTime,
    spots,
    vols,
    Map.empty,
    rate
  )

  val nSims = (1 << 15) - 1

  def printResult(cc: ContingentClaim): Unit =
    val t1 = System.nanoTime()
    val result = cc.fairValueResult(sim, nSims).toTry.get
    val t2 = System.nanoTime()
    val exerciseProb = result.putProbabilities.values.sum
    println(
      f"price = ${result.fairValue}, exercise prob = $exerciseProb, duration = ${(t2 - t1) * 1e-6}%.0f ms"
    )

  (0 until 1000).foreach: _ =>
    printResult(europeanPut)
    printResult(americanPut)
