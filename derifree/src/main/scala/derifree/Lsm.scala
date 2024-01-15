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
import derifree.prettyprint.*
import org.ejml.data.DMatrixRMaj
import org.ejml.dense.row.CommonOps_DDRM
import org.ejml.dense.row.NormOps_DDRM
import org.ejml.dense.row.factory.LinearSolverFactory_DDRM

import collection.immutable.ArraySeq
import Lsm.*

private[derifree] trait Lsm:

  def continuationValueEstimator(rows: Seq[(List[Double], Double)]): Either[Error, Estimator]

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
    ): Either[Error, Estimator] =
      for
        _ <- Either.raiseWhen(rows.isEmpty)(Error.BadInputs("empty rows"))
        factors0 = rows.head(0)
        m = rows.length
        nCoeffs = basisFunctions(factors0.toIndexedSeq).length
        data = rows.flatMap((factors, _) => basisFunctions(factors.toIndexedSeq))
        a <- Either
          .catchNonFatal(
            new DMatrixRMaj(
              m,
              nCoeffs,
              true,
              data*
            )
          )
          .leftMap(t => Error.BadInputs(t.getMessage))
        b <- Either
          .catchNonFatal(new DMatrixRMaj(m, 1, true, rows.map(_(1))*))
          .leftMap(t => Error.BadInputs(t.getMessage))
        x <- Either
          .catchNonFatal(new DMatrixRMaj(nCoeffs, 1, true, Array.ofDim(nCoeffs)*))
          .leftMap(t => Error.BadInputs(t.getMessage))
        solver <- Either
          .catchNonFatal(LinearSolverFactory_DDRM.leastSquaresQrPivot(true, false))
          .leftMap(t => Error.BadNumericsException(t.getMessage))
        success = solver.setA(a)
        _ <- Either.raiseUnless(success)(Error.BadNumericsException("solver rejected matrix A"))
        _ <- Either
          .catchNonFatal(solver.solve(b, x))
          .leftMap(t => Error.BadNumericsException(t.getMessage))
      yield
        val coeffs = x.data.toIndexedSeq
        // println(s"rows=$m, coeffs: $coeffs")
        val condStd =
          NormOps_DDRM.normP2(
            CommonOps_DDRM.subtract(CommonOps_DDRM.mult(a, x, null), b, null)
          ) / m

        // println(s"b = ${b.data.prettyPrint(2)}")
        // println(s"cond std = $condStd, m=$m")

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
