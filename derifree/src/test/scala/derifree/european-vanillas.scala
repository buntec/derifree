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

import derifree.literals.*
import derifree.syntax.*

import scala.math.exp
import scala.concurrent.duration.*

class VanillaSuite extends munit.FunSuite:

  override def munitTimeout: Duration = 3.minutes

  val dsl = Dsl[java.time.Instant]
  import dsl.*

  val refTime = i"2023-01-01T18:00:00Z"

  val spots = Map("ACME" -> 100.0, "EMCA" -> 50.0)
  val vols = Map("ACME" -> 0.3.vol, "EMCA" -> 0.35.vol)
  val correlations = Map(("ACME", "EMCA") -> 0.7)
  val rate = 0.05.rate

  val dirNums = Sobol.directionNumbers(500).toTry.get

  val sim = models.blackscholes.simulator(
    TimeGrid.Factory.almostEquidistant(YearFraction.oneDay),
    NormalGen.Factory.sobol(dirNums),
    refTime,
    Ccy.USD,
    spots,
    vols,
    correlations,
    rate
  )

  test("Black-Scholes Monte Carlo price is close to closed-form price."):
    val nSims = (1 << 16) - 1
    for
      days <- List(10, 30, 90, 180, 365)
      strike <- List(0.5, 0.75, 1.0, 1.25, 1.5)
      isCall <- List(true, false)
      udl <- List("ACME", "EMCA")
    yield
      val hint = s"days=$days, strike=$strike, isCall=$isCall, udl=$udl"
      val expiry = refTime.plusDays(days)
      val settle = expiry.plusDays(2)
      val omega = if isCall then 1 else -1

      val europeanVanilla = for
        s0 <- spot(udl, refTime)
        s <- spot(udl, expiry)
        _ <- cashflow(max(omega * (s / s0 - strike), 0), Ccy.USD, settle)
      yield ()

      val t = refTime.yearFractionTo(expiry)
      val forward = spots(udl) * exp(rate * t)
      val ts = refTime.yearFractionTo(settle)
      val discount = exp(-rate * ts)
      val optionType = if isCall then black.OptionType.Call else black.OptionType.Put

      val refPrice = black.price(
        optionType,
        spots(udl) * strike,
        t.toDouble,
        vols(udl).toDouble,
        forward,
        discount
      ) / spots(udl)

      val price = europeanVanilla.fairValue(sim, nSims).toTry.get

      // println(s"price=$price, ref=$refPrice, hint=$hint")
      assertEqualsDouble(price.toDouble, refPrice, 1e-4, hint)
