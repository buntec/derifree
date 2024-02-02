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
import org.apache.commons.math3.linear.LUDecomposition
import org.apache.commons.math3.linear.MatrixUtils
import org.apache.commons.math3.stat.regression.OLSMultipleLinearRegression
import org.ejml.data.DMatrixRMaj
import org.ejml.dense.row.CommonOps_DDRM
import org.ejml.dense.row.NormOps_DDRM
import org.ejml.dense.row.factory.LinearSolverFactory_DDRM

import scala.{math => smath}

import LeastSquares.*

trait LeastSquares:

  /** @param a
    *   m x n design matrix
    * @param y
    *   m x 1 response vector
    */
  def ols(a: Seq[Seq[Double]], y: Seq[Double]): Either[Error, Result]

  def ridge(a: Seq[Seq[Double]], y: Seq[Double], lambda: Double): Either[Error, Result]

object LeastSquares:

  final case class Result(
      coefficients: IndexedSeq[Double],
      residuals: IndexedSeq[Double],
      normOfResiduals: Double
  )

  enum Error(message: String) extends derifree.Error(message):
    case BadNumericsException(message: String) extends Error(message)
    case BadInputs(message: String) extends Error(message)

  def apply: LeastSquares = ejml

  /** Implementation based on Apache commons math. */
  private[math] def commonsmath: LeastSquares = new LeastSquares:

    def ridge(
        a: Seq[Seq[Double]],
        y: Seq[Double],
        lambda: Double
    ): Either[Error, Result] =
      for
        _ <- Either.raiseWhen(a.isEmpty)(Error.BadInputs("empty rows"))
        _ <- Either.raiseWhen(lambda < 0)(Error.BadInputs("lambda must be non-negative"))
        result <- Either
          .catchNonFatal:
            val X = MatrixUtils.createRealMatrix(a.map(_.toArray).toArray)
            val y0 = MatrixUtils.createRealVector(y.toArray)
            val XtX = X.transpose.multiply(X)
            val lambdaI =
              MatrixUtils.createRealIdentityMatrix(XtX.getRowDimension).scalarMultiply(lambda)
            val c = XtX.add(lambdaI)
            val cInverse = new LUDecomposition(c).getSolver.getInverse()
            val d = X.preMultiply(y0)
            val beta = cInverse.transpose.preMultiply(d)
            val residuals = X.transpose.preMultiply(beta).subtract(y0)
            Result(
              beta.toArray.toIndexedSeq,
              residuals.toArray.toIndexedSeq,
              residuals.getNorm
            )
          .leftMap(t => Error.BadInputs(t.getMessage))
      yield result

    def ols(a: Seq[Seq[Double]], y: Seq[Double]): Either[Error, Result] =
      val lr = new OLSMultipleLinearRegression
      lr.setNoIntercept(true)
      for
        _ <- Either.raiseWhen(a.isEmpty)(Error.BadInputs("empty rows"))
        _ <- Either
          .catchNonFatal(lr.newSampleData(y.toArray, a.map(_.toArray).toArray))
          .leftMap(t => Error.BadInputs(t.getMessage))
        beta <- Either
          .catchNonFatal(lr.estimateRegressionParameters())
          .leftMap(t => Error.BadInputs(t.getMessage))
        residuals <- Either
          .catchNonFatal(lr.estimateResiduals.toIndexedSeq)
          .leftMap(t => Error.BadInputs(t.getMessage))
        residualSumOfSquares <- Either
          .catchNonFatal(lr.calculateResidualSumOfSquares())
          .leftMap(t => Error.BadInputs(t.getMessage))
      yield Result(
        beta.toIndexedSeq,
        residuals.toIndexedSeq,
        smath.sqrt(residualSumOfSquares)
      )

  /** Implementation based on ejml. */
  private[math] def ejml: LeastSquares = new LeastSquares:

    def ridge(a: Seq[Seq[Double]], y: Seq[Double], lambda: Double): Either[Error, Result] =
      for
        _ <- Either.raiseWhen(a.isEmpty)(Error.BadInputs("empty rows"))
        _ <- Either.raiseWhen(lambda < 0)(Error.BadInputs("lambda must be non-negative"))
        result <- Either
          .catchNonFatal:
            val X = new DMatrixRMaj(a.map(_.toArray).toArray)
            val y0 = new DMatrixRMaj(y.length, 1, true, y*)
            val XtX = CommonOps_DDRM.multTransA(X, X, null)
            val lambdaI = CommonOps_DDRM.identity(XtX.getNumCols)
            CommonOps_DDRM.scale(lambda, lambdaI)
            val c = CommonOps_DDRM.add(XtX, lambdaI, null)
            CommonOps_DDRM.invert(c)
            val beta = CommonOps_DDRM.mult(c, CommonOps_DDRM.multTransA(X, y0, null), null)
            val residuals =
              CommonOps_DDRM.subtract(CommonOps_DDRM.mult(X, beta, null), y0, null)
            val norm = NormOps_DDRM.normP2(residuals)
            Result(
              beta.data.toIndexedSeq,
              residuals.data.toIndexedSeq,
              norm
            )
          .leftMap(t => Error.BadInputs(t.toString))
      yield result

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
