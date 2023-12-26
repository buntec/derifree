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
import org.apache.commons.math3.util.{FastMath => math}
import org.ejml.data.DMatrixRMaj
import org.ejml.dense.row.factory.LinearSolverFactory_DDRM

import collection.mutable
import Lsm.*

private[derifree] trait Lsm:

  def toBasisFunctions(factors: IndexedSeq[Double]): IndexedSeq[Double]

  def toContValueEstimator(rows: Seq[(List[Double], Double)]): Either[Error, Estimator]

private[derifree] object Lsm:

  enum Error extends derifree.Error:
    case BadNumericsException(override val getMessage: String)
    case BadInputs(override val getMessage: String)

  trait Estimator:

    def apply(factors: IndexedSeq[Double]): Double

  def fromPoly(maxDegree: Int): Lsm = new Lsm:

    def toBasisFunctions(factors: IndexedSeq[Double]): IndexedSeq[Double] =
      monomials(factors, maxDegree)

    def toContValueEstimator(rows: Seq[(List[Double], Double)]): Either[Error, Estimator] =
      for
        _ <- Either.raiseWhen(rows.isEmpty)(Error.BadInputs("empty rows"))
        factors0 = rows.head(0)
        m = rows.length
        nCoeffs = toBasisFunctions(factors0.toIndexedSeq).length
        data = rows.flatMap((factors, _) => toBasisFunctions(factors.toIndexedSeq))
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
        // _ = println( s"rows = ${a.numRows}, cols = ${a.numCols}, data.length = ${data.length}, data = ${a.data.mkString(", ")}")
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
        // println(s"coeffs: $coeffs")
        new Estimator:
          def apply(factors: IndexedSeq[Double]): Double =
            (toBasisFunctions(factors) zip coeffs).map(_ * _).sum

  // TODO: replace with more efficient implementation
  private def monomials(covariates: IndexedSeq[Double], maxDegree: Int): IndexedSeq[Double] =
    def powers(indices: List[Int]): mutable.Map[Int, Int] =
      val res = mutable.Map.empty[Int, Int]
      indices.foreach(i =>
        res.updateWith(i):
          case Some(n) => Some(n + 1)
          case None    => Some(1)
      )
      res

    (0 to maxDegree)
      .flatMap(degree => covariates.indices.toList.replicateA(degree))
      .map(powers)
      .distinct
      .map(_.map((i, n) => math.pow(covariates(i), n)).product)
