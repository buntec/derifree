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

import derifree.fd.*
import derifree.syntax.*

import scala.concurrent.duration.*
import scala.math.abs

class LsmSuite extends munit.FunSuite:

  test("Compare to tabulated values in Longstaff-Schwartz 2001 paper"):

    case class Row(
        S: Double,
        sigma: Double,
        T: Double,
        fdAm: Double,
        cfEu: Double,
        SimAm: Double
    )

    // corresponds to Table 1 in the paper
    val rows = List(
      Row(36, 0.2, 1, 4.478, 3.844, 4.472),
      Row(36, 0.2, 2, 4.840, 3.763, 4.821),
      Row(36, 0.4, 1, 7.101, 6.711, 7.091),
      Row(36, 0.4, 2, 8.508, 7.700, 8.488),
      Row(38, 0.2, 1, 3.250, 2.852, 3.244),
      Row(38, 0.2, 2, 3.745, 2.991, 3.735),
      Row(38, 0.4, 1, 6.148, 5.834, 6.139),
      Row(38, 0.4, 2, 7.670, 6.979, 7.669),
      Row(40, 0.2, 1, 2.314, 2.066, 2.313),
      Row(40, 0.2, 2, 2.885, 2.356, 2.879),
      Row(40, 0.4, 1, 5.312, 5.060, 5.308),
      Row(40, 0.4, 2, 6.920, 6.326, 6.921),
      Row(42, 0.2, 1, 1.617, 1.465, 1.617),
      Row(42, 0.2, 2, 2.212, 1.841, 2.206),
      Row(42, 0.4, 1, 4.582, 4.379, 4.588),
      Row(42, 0.4, 2, 6.248, 5.736, 6.243),
      Row(44, 0.2, 1, 1.110, 1.017, 1.118),
      Row(44, 0.2, 2, 1.690, 1.429, 1.675),
      Row(44, 0.4, 1, 3.948, 3.783, 3.957),
      Row(44, 0.4, 2, 5.647, 5.202, 5.622)
    )

    val refTime = YearFraction.zero
    val dirNums = Sobol.directionNumbers(1500).toTry.get

    val nSims = (1 << 16) - 1

    val udl = "ACME"
    val ccy = Ccy.USD
    val strike = 40.0
    val rate = 0.06
    val divs = Nil

    rows.foreach: row =>

      val expiry = YearFraction(row.T)
      val vol = row.sigma
      val spot = row.S

      val discount = YieldCurve.fromContinuouslyCompoundedRate(rate.rate, refTime)
      val borrow = YieldCurve.zero[YearFraction]
      val forward = Forward(spot, divs, discount, borrow)

      val tgFactory = TimeGrid.Factory.almostEquidistant(YearFraction.oneDay)
      val sgFactory = SpatialGrid.Factory.logSinh(300, 0.01, strike)

      val assets = models.blackscholes.Asset(udl, Ccy.USD, forward, vol.vol) :: Nil

      val sim = models.blackscholes.simulator(
        TimeGrid.Factory.almostEquidistant(YearFraction.oneDay),
        NormalGen.Factory.sobol(dirNums),
        refTime,
        assets,
        Map.empty,
        discount,
        Map.empty
      )

      // 50 exercise steps per year as in the paper
      val earlyExtimes = TimeLike[YearFraction].stepsBetween(refTime, expiry, 7.3.days)

      val bmVanilla =
        payoffs.BermudanVanilla(
          udl,
          ccy,
          strike,
          expiry,
          earlyExtimes,
          payoffs.BermudanVanilla.OptionType.Put
        )

      val euRefPrice = black.price(
        black.OptionType.Put,
        strike,
        expiry.toDouble,
        vol,
        forward(expiry),
        discount.discountFactor(expiry)
      )

      val bmPriceFd = fd.feynmankac.blackScholes(
        bmVanilla,
        forward,
        discount,
        vol,
        refTime,
        tgFactory,
        sgFactory,
        Settings.default
      )

      val dsl = Dsl[YearFraction]

      val bmPriceMcResult =
        bmVanilla.contingentClaim(refTime, dsl).fairValueResult(sim, nSims).toTry.get

      val bmPriceMc = bmPriceMcResult.fairValue

      val clue = f"""S=${row.S}, sigma=${row.sigma}, T=${row.T}
        |paper:    cfEu=${row.cfEu}%.3f, fdAm=${row.fdAm}%.3f, simAm=${row.SimAm}%.3f
        |derifree: cfEu=${euRefPrice}%.3f, fdAm=${bmPriceFd}%.3f, simAm=${bmPriceMc}%.3f
        """.stripMargin

      println(clue)

      assertEqualsDouble(euRefPrice, row.cfEu, 0.001, clue)
      assertEqualsDouble(bmPriceFd, row.fdAm, 0.01, clue)

      // pass if our MC price is close to either FD price or MC price from paper
      assert(
        abs(bmPriceMc.toDouble - row.SimAm) <= 0.01 || abs(
          bmPriceMc.toDouble - bmPriceFd
        ) <= 0.01,
        clue
      )

  test("speed of polynomial regression".ignore):
    val rows = List(
      (List(0.1, 2.1), 1.1),
      (List(0.3, 1.1), 0.3),
      (List(0.1, -0.3), 1.3),
      (List(3.1, 1.2), -3.1),
      (List(2.1, 4.2), 3.1),
      (List(3.4, -2.2), 3.7),
      (List(8.2, 1.5), 1.7),
      (List(3.1, -1.8), 2.9),
      (List(7.1, 3.2), 2.9)
    )
    var j = 0
    while (j < 1000) {
      val t1 = System.nanoTime()
      var i = 0
      var sum = 0.0
      while (i < 64000) {
        val lsm = Lsm.fromPoly(3)
        val est = lsm.continuationValueEstimator(rows).toOption.get
        sum += est(IndexedSeq(0.4, 0.2))
        i += 1
      }
      val t2 = System.nanoTime()
      println(s"took ${(t2 - t1) * 1e-6} ms")
      j += 1
    }
