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
import org.ejml.data.DMatrixRMaj
import org.ejml.dense.row.CommonOps_DDRM
import org.ejml.dense.row.NormOps_DDRM
import org.ejml.dense.row.factory.LinearSolverFactory_DDRM

import LeastSquares.*

trait LeastSquares:

  /** @param a
    *   m x n design matrix
    * @param y
    *   m x 1 response vector
    */
  def ols(a: Seq[Seq[Double]], y: Seq[Double]): Either[Error, Result]

object LeastSquares:

  final case class Result(
      coefficients: IndexedSeq[Double],
      residuals: IndexedSeq[Double],
      normOfResiduals: Double
  )

  enum Error(message: String) extends derifree.Error(message):
    case BadNumericsException(message: String) extends Error(message)
    case BadInputs(message: String) extends Error(message)

  /** Implementation based on ejml. */
  def ejml: LeastSquares = new LeastSquares:
    def ols(a: Seq[Seq[Double]], y: Seq[Double]): Either[Error, Result] =
      for
        _ <- Either.raiseWhen(a.isEmpty)(Error.BadInputs("empty rows"))
        a0 <- Either
          .catchNonFatal(new DMatrixRMaj(a.map(_.toArray).toArray))
          .leftMap(t => Error.BadInputs(t.getMessage))
        y0 <- Either
          .catchNonFatal(new DMatrixRMaj(y.length, 1, true, y*))
          .leftMap(t => Error.BadInputs(t.getMessage))
        beta <- Either
          .catchNonFatal(new DMatrixRMaj(a0.getNumCols, 1, true, Array.ofDim(a0.getNumCols)*))
          .leftMap(t => Error.BadInputs(t.getMessage))
        solver <- Either
          .catchNonFatal(LinearSolverFactory_DDRM.leastSquaresQrPivot(true, false))
          .leftMap(t => Error.BadNumericsException(t.getMessage))
        success = solver.setA(a0)
        _ <- Either.raiseUnless(success)(Error.BadNumericsException("solver rejected matrix A"))
        _ <- Either
          .catchNonFatal(solver.solve(y0, beta))
          .leftMap(t => Error.BadNumericsException(t.getMessage))
      yield
        val residuals =
          CommonOps_DDRM.subtract(CommonOps_DDRM.mult(a0, beta, null), y0, null)
        val norm = NormOps_DDRM.normP2(residuals)
        Result(
          beta.data.toIndexedSeq,
          residuals.data.toIndexedSeq,
          norm
        )
