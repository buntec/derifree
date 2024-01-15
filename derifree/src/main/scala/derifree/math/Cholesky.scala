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
package math

import cats.syntax.all.*
import org.apache.commons.math3.linear.Array2DRowRealMatrix
import org.apache.commons.math3.linear.CholeskyDecomposition
import org.ejml.data.DMatrixRMaj
import org.ejml.dense.row.factory.DecompositionFactory_DDRM

import scala.collection.immutable.ArraySeq

trait Cholesky:

  /** For any PSD m, returns lower triangular L s.t. m = LL^T */
  def decomp(
      m: IndexedSeq[IndexedSeq[Double]]
  ): Either[derifree.Error, IndexedSeq[IndexedSeq[Double]]]

object Cholesky:

  def apply: Cholesky = ejml

  private[math] def ejml: Cholesky = new Cholesky:
    def decomp(
        m: IndexedSeq[IndexedSeq[Double]]
    ): Either[Error, IndexedSeq[IndexedSeq[Double]]] =
      for
        m0 <- Either
          .catchNonFatal(new DMatrixRMaj(m.map(_.toArray).toArray))
          .leftMap(t => derifree.Error.BadInputs(t.getMessage))
        n = m0.getNumRows
        decomp = DecompositionFactory_DDRM.chol(true)
        success = decomp.decompose(m0)
        _ <- Either.raiseUnless(success)(
          derifree.Error.BadNumerics("matrix is not PSD")
        )
      yield
        val lower = decomp.getT(null)
        val res = Array.ofDim[Double](n, n)
        var i = 0
        while (i < n) {
          var j = 0
          while (j < n) {
            res(i)(j) = lower.get(i, j)
            j += 1
          }
          i += 1
        }
        ArraySeq.unsafeWrapArray(res.map(ArraySeq.unsafeWrapArray))

  private[math] def commonsmath: Cholesky = new Cholesky:
    def decomp(
        m: IndexedSeq[IndexedSeq[Double]]
    ): Either[Error, IndexedSeq[IndexedSeq[Double]]] =
      Either
        .catchNonFatal:
          val m0 = new Array2DRowRealMatrix(m.map(_.toArray).toArray)
          val decomp = new CholeskyDecomposition(m0)
          decomp.getL().getData.map(_.toIndexedSeq).toIndexedSeq
        .leftMap(t => derifree.Error.BadNumerics(t.getMessage))
