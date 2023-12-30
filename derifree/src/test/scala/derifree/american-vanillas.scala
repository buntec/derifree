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

import cats.syntax.all.*
import derifree.syntax.*

class AmericanVanillaSuite extends munit.FunSuite:

  val dsl = Dsl[YearFraction]
  import dsl.*

  val refTime = YearFraction.zero

  val dirNums = Sobol.directionNumbers(500).toTry.get

  test("American put should match tabulated values"):
    val udl = "ACME"

    case class Row(spot: Double, europeanPrice: Double, americanPrice: Double, tol: Double)

    // values taken from Barone, Adesi, Whaley 1987, Table IV
    val rows = List(
      Row(80.0, 18.09, 20.0, 0.1),
      Row(90.0, 9.05, 10.4, 0.5),
      Row(100.0, 3.04, 3.22, 0.05),
      Row(110.0, 0.64, 0.66, 0.05),
      Row(120.0, 0.09, 0.09, 0.05)
    )

    rows.foreach { case Row(s0, euPriceRef, amPriceRef, absTol) =>
      val spots = Map(udl -> s0)
      val vols = Map(udl -> 0.2.vol)
      val rate = 0.08.rate

      val sim: Simulator[YearFraction] =
        Simulator.blackScholes(
          TimeGrid.Factory.almostEquidistant(YearFraction.oneDay),
          NormalGen.Factory.sobol(dirNums),
          refTime,
          spots,
          vols,
          Map.empty,
          rate
        )

      val expiry = YearFraction.oneYear / 4
      val strike = 100.0

      val nSims = (1 << 15) - 1

      val europeanPut = for
        s <- spot(udl, expiry)
        _ <- cashflow(max(strike - s, 0.0), Ccy.USD, expiry)
      yield ()

      val americanPut =
        val m = 90
        for
          s <- spot(udl, expiry)
          _ <- List
            .tabulate(m)(i => (expiry / m) * i)
            .drop(1)
            .traverse(t =>
              spot(udl, t).flatMap(s_t =>
                puttable(max(strike - s_t, 0.0).some.filter(_ > 0), Ccy.USD, t)
              )
            )
          _ <- cashflow(max(strike - s, 0.0), Ccy.USD, expiry)
        yield ()

      val euPrice = europeanPut.fairValue(sim, nSims).toTry.get
      val amPrice = americanPut.fairValue(sim, nSims).toTry.get

      val hint =
        s"spot=$s0, euPrice=$euPrice, euPriceRef=$euPriceRef, amPrice=$amPrice, amPriceRef=$amPriceRef"

      // println(hint)
      assert(euPrice.toDouble <= amPrice.toDouble + absTol, hint)
      assert(math.abs(euPrice.toDouble - euPriceRef) < absTol, hint)
      assert(math.abs(amPrice.toDouble - amPriceRef) < absTol, hint)
    }
