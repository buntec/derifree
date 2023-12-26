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

import collection.mutable

import Lsm.*
import org.apache.commons.math3.util.{FastMath => math}
import org.ejml.dense.row.factory.LinearSolverFactory_DDRM
import org.ejml.data.DMatrixRMaj

trait Lsm:

  def toBasisFunctions(factors: IndexedSeq[Double]): IndexedSeq[Double]

  def toContValueEstimator(rows: Seq[(List[Double], Double)]): Either[Error, Estimator]

object Lsm:

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

  @main def lsmPerf: Unit =
    val rows = List(
      (List(0.1, 2.1), 1.1),
      (List(0.3, 1.1), 0.3),
      (List(0.1, -0.3), 1.3),
      (List(3.1, 1.2), -3.1),
      (List(2.1, 4.2), 3.1),
      (List(3.4, -2.2), 3.7),
      (List(8.2, 1.5), 1.7),
      (List(3.1, -1.8), 2.9),
      (List(7.1, 3.2), 2.9)
    )
    var j = 0
    while (j < 1000) {
      val t1 = System.nanoTime()
      var i = 0
      var sum = 0.0
      while (i < 64000) {
        val lsm = fromPoly(2)
        val est = lsm.toContValueEstimator(rows).toOption.get
        sum += est(IndexedSeq(0.4, 0.2))
        i += 1
      }
      val t2 = System.nanoTime()
      println(s"took ${(t2 - t1) * 1e-6} ms")
      j += 1
    }
