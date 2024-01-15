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

import derifree.math.LeastSquares

import collection.immutable.ArraySeq
import Lsm.*

private[derifree] trait Lsm:

  def continuationValueEstimator(
      rows: Seq[(List[Double], Double)]
  ): Either[derifree.Error, Estimator]

private[derifree] object Lsm:

  enum Error(message: String) extends derifree.Error(message):
    case BadNumericsException(message: String) extends Error(message)
    case BadInputs(message: String) extends Error(message)

  trait Estimator:

    def apply(factors: IndexedSeq[Double]): Double

    def stdDev: Double

  def fromPoly(maxDegree: Int): Lsm = new Lsm:

    def basisFunctions(factors: IndexedSeq[Double]): IndexedSeq[Double] =
      monomials(factors, maxDegree)

    def continuationValueEstimator(
        rows: Seq[(List[Double], Double)]
    ): Either[derifree.Error, Estimator] =
      val m = rows.length
      val a = rows.map((factors, _) => basisFunctions(factors.toIndexedSeq)).toIndexedSeq
      val b = rows.map(_(1)).toIndexedSeq
      LeastSquares.apply
        .ols(a, b)
        .map: result =>
          val coeffs = result.coefficients
          val condStd = result.normOfResiduals / m
          new Estimator:
            def stdDev: Double = condStd
            def apply(factors: IndexedSeq[Double]): Double =
              (basisFunctions(factors) zip coeffs).map(_ * _).sum

  private def monomials(covariates: IndexedSeq[Double], maxDegree: Int): IndexedSeq[Double] =
    def go(start: Int, maxDegree: Int): Array[Double] =
      val b = Array.newBuilder[Double]
      var i = 0
      val x0 = covariates(start)
      var xp = 1.0
      if (start < covariates.length - 1) {
        while (i <= maxDegree) {
          b.addAll(go(start + 1, maxDegree - i).map(_ * xp))
          xp *= x0
          i += 1
        }
      } else {
        while (i <= maxDegree) {
          b.addOne(xp)
          xp *= x0
          i += 1
        }
      }
      b.result()

    ArraySeq.unsafeWrapArray(go(0, maxDegree))
