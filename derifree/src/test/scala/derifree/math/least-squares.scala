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

package derifree.math

import cats.syntax.all.*
import derifree.Gen

class LeastSquaresSuite extends munit.FunSuite:

  private case class Case(
      a: IndexedSeq[IndexedSeq[Double]], // design matrix
      y: IndexedSeq[Double]
  )

  private val genCase: Gen[Case] = for
    m <- Gen.between(3, 20)
    n <- Gen.between(m + 1, 4000)
    intercept <- Gen.boolean
    a <- Gen.normal
      .replicateA(m)
      .map(row => if intercept then (1.0 :: row) else row)
      .replicateA(n)
    y <- Gen.normal.replicateA(n)
  yield Case(a.map(_.toIndexedSeq).toIndexedSeq, y.toIndexedSeq)

  test("different implementations should give identical results"):
    genCase.view
      .take(500)
      .zipWithIndex
      .foreach: (c, i) =>
        val r1 = LeastSquares.ejml.ols(c.a, c.y).toTry.get
        val r2 = LeastSquares.commonsmath.ols(c.a, c.y).toTry.get
        val coeffs1 = r1.coefficients
        val coeffs2 = r2.coefficients
        (coeffs1 zip coeffs2).foreach: (b1, b2) =>
          val clue = s"i=$i, b1=$b1, b2=$b2"
          // println(clue)
          assertEqualsDouble(b1, b2, 1e-10, clue)
