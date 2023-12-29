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
import cats.Show
import cats.derived.*

class BlackSuite extends munit.FunSuite:

  private case class Case(
      oType: black.OptionType,
      forward: Double,
      strike: Double,
      val vol: Double,
      df: Double,
      timeToExpiry: Double
  ) derives Show

  test("implied vol should be close to input vol"):

    val gen: Gen[Case] = for
      ot <- Gen.choose(black.OptionType.Call, black.OptionType.Put)
      vol <- Gen.between(0.001, 1.0)
      f <- Gen.constant(100.0)
      k <- Gen.choose(0.5, 0.8, 0.9, 1.0, 1.1, 1.2, 1.5).map(_ * f)
      t <- Gen.between(0.0001, 3.0)
      df <- Gen.choose(-0.05, -0.02, 0.0, 0.02, 0.05, 0.1).map(r => math.exp(-r * t))
    yield Case(ot, f, k, vol, df, t)

    val tol = 1e-6

    gen.view
      .map: c =>
        val price = black.price(
          c.oType,
          c.strike,
          c.timeToExpiry,
          c.vol,
          c.forward,
          c.df
        )
        val intrinsic =
          math.max(0.0, c.df * c.oType.sign * (c.forward - c.strike))
        (c, price, intrinsic)
      .filter:
        case (c, price, intrinsic) => price - intrinsic > c.forward * 1e-8
      .take(10000)
      .zipWithIndex
      .foreach:
        case ((c, price, intrinsic), i) =>
          val ivol = black
            .impliedVol(
              c.oType,
              c.strike,
              c.timeToExpiry,
              c.forward,
              c.df,
              price
            )
            .toTry
            .get
          println(show"i=$i, case=$c, vol=${c.vol}, implied vol= $ivol")
          assert(math.abs(ivol - c.vol) <= tol)
