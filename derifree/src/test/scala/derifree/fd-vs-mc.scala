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

import cats.Show
import cats.derived.*
import cats.syntax.all.*
import derifree.fd.*
import derifree.syntax.*

class McVsFd extends munit.FunSuite:

  private case class Case(
      timeToExpiry: YearFraction,
      strike: Double,
      vol: Double,
      isCall: Boolean,
      discountRate: Double,
      borrowRate: Double
  ) derives Show

  private val genCase: Gen[Case] = for
    tte <- Gen.between(0.01, 1.0).map(YearFraction(_))
    // isCall <- Gen.boolean
    isCall = false
    strike <- Gen.between(70.0, 150.0)
    dRate <- Gen.between(-0.1, 0.1)
    bRate <- Gen.between(-0.05, 0.05)
    vol <- Gen.between(0.05, 0.8)
  yield Case(tte, strike, vol, isCall, dRate, bRate)

  val refTime = YearFraction.zero

  val dirNums = Sobol.directionNumbers(1500).toTry.get

  val nSims = (1 << 15) - 1

  val udl = "ACME"

  test("Vanilla prices should be close"):

    val refTime = YearFraction.zero
    val spot = 100.0
    val divs = Nil // Dividend(refTime.plusDays(180), 3.0, 0.02) :: Nil

    genCase.view.zipWithIndex
      .take(1000)
      .foreach: (c, i) =>
        val expiry = c.timeToExpiry
        val strike = c.strike
        val vol = c.vol
        val discount = YieldCurve.fromContinuouslyCompoundedRate(c.discountRate.rate, refTime)
        val borrow = YieldCurve.fromContinuouslyCompoundedRate(c.borrowRate.rate, refTime)
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

        val dsl = Dsl[YearFraction]
        import dsl.*

        val europeanPut = for
          s <- dsl.spot(udl, expiry)
          _ <- cashflow(max(strike - s, 0.0), Ccy.USD, expiry)
        yield ()

        val americanPut =
          val m = 90
          for
            s <- dsl.spot(udl, expiry)
            _ <- List
              .tabulate(m)(i => (expiry / m) * i)
              .drop(1)
              .traverse(t =>
                dsl
                  .spot(udl, t)
                  .flatMap(s_t =>
                    puttable(max(strike - s_t, 0.0).some.filter(_ > 0), Ccy.USD, t)
                  )
              )
            _ <- cashflow(max(strike - s, 0.0), Ccy.USD, expiry)
          yield ()

        val euFdPayoff =
          payoffs.EuropeanVanilla(
            strike,
            expiry,
            if c.isCall then payoffs.EuropeanVanilla.OptionType.Call
            else payoffs.EuropeanVanilla.OptionType.Put
          )

        val amFdPayoff =
          payoffs.AmericanVanilla(
            strike,
            expiry,
            if c.isCall then payoffs.AmericanVanilla.OptionType.Call
            else payoffs.AmericanVanilla.OptionType.Put
          )

        val euPrice = europeanPut.fairValue(sim, nSims).toTry.get
        val amPrice = americanPut.fairValue(sim, nSims).toTry.get
        val euPriceFd = fd.feynmankac.blackScholes(
          euFdPayoff,
          forward,
          discount,
          vol,
          refTime,
          tgFactory,
          sgFactory,
          Settings.default
        )
        val amPriceFd = fd.feynmankac.blackScholes(
          amFdPayoff,
          forward,
          discount,
          vol,
          refTime,
          tgFactory,
          sgFactory,
          Settings.default
        )

        println(
          s"i=$i, euPrice=$euPrice, euPriceFd=$euPriceFd, amPrice=$amPrice, amPriceFd=$amPriceFd"
        )
