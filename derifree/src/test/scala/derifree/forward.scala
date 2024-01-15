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

class ForwardSuite extends munit.FunSuite:

  val refTime = YearFraction.zero

  private case class Case(
      spot: Double,
      divs: List[Dividend[YearFraction]],
      discount: YieldCurve[YearFraction],
      borrow: YieldCurve[YearFraction]
  )

  private val genYieldCurve: Gen[YieldCurve[YearFraction]] =
    Gen.between(-0.1, 0.1).map(r => YieldCurve.fromContinuouslyCompoundedRate(r.rate, refTime))

  private def genDiv(spot: Double): Gen[Dividend[YearFraction]] =
    for
      t <- Gen.between(0.01, 3.0).map(YearFraction(_))
      cash <- Gen.between(0.001, 0.05).map(_ * spot)
      prop <- Gen.between(0.001, 0.05)
    yield Dividend(t, cash, prop)

  private val caseGen: Gen[Case] =
    for
      spot <- Gen.between(1.0, 1000.0)
      nDivs <- Gen.between(0, 20)
      divs <- genDiv(spot).replicateA(nDivs).map(_.sortBy(_.exDiv))
      discount <- genYieldCurve
      borrow <- genYieldCurve
    yield Case(spot, divs, discount, borrow)

  test("should match ref implementation"):
    caseGen.view.zipWithIndex
      .take(1000)
      .foreach: (c, i) =>
        val clue = s"i = $i, case = $c"
        val f1 = Forward(c.spot, c.divs, c.discount, c.borrow)
        val f2 = Forward.referenceImpl(c.spot, c.divs, c.discount, c.borrow)
        val ts = List(0, 1, 7, 30, 90, 365, 730).map(refTime.plusDays)
        ts.foreach: t =>
          assertEqualsDouble(f1(t), f2(t), 1e-10 * c.spot, clue)
