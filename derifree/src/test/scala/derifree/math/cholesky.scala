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
import org.apache.commons.math3.linear.Array2DRowRealMatrix

class CholeskySuite extends munit.FunSuite:

  private case class Case(m: IndexedSeq[IndexedSeq[Double]])

  private val genCase: Gen[Case] = for
    n <- Gen.between(2, 10)
    b0 <- Gen.normal.replicateA(n).replicateA(n)
  yield
    val b = Array2DRowRealMatrix(b0.map(_.toArray).toArray)
    val m = b.multiply(b.transpose()).getData.map(_.toIndexedSeq).toIndexedSeq
    Case(m)

  test("different backends should give identical results"):
    genCase.view.zipWithIndex
      .take(1000)
      .foreach: (c, i) =>
        val l1 = Cholesky.ejml.decomp(c.m).toTry.get
        val l2 = Cholesky.commonsmath.decomp(c.m).toTry.get
        (l1 zip l2).foreach: (row1, row2) =>
          (row1 zip row2).foreach: (a1, a2) =>
            val clue = s"i=$i, a1=$a1, a2=$a2"
            // println(clue)
            assertEqualsDouble(a1, a2, 1e-12, clue)
