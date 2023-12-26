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
import org.ejml.data.DMatrixRMaj
import org.ejml.dense.row.factory.DecompositionFactory_DDRM

import scala.collection.immutable.ArraySeq

private[derifree] object utils:

  def cholesky(
      corrs: Map[(String, String), Double],
      udls: List[String]
  ): Either[derifree.Error, IndexedSeq[IndexedSeq[Double]]] =
    val n = udls.length
    val dataMaybe = (for {
      udl1 <- udls
      udl2 <- udls
    } yield
      if (udl1 == udl2) then 1.0.some
      else corrs.get((udl1, udl2)).orElse(corrs.get((udl2, udl1)))).sequence

    for
      data <- Either.fromOption(
        dataMaybe,
        derifree.Error.MissingData("Missing correlation pair.")
      )
      m = new DMatrixRMaj(n, n, true, data*)
      decomp = DecompositionFactory_DDRM.chol(true)
      success = decomp.decompose(m)
      _ <- Either.raiseUnless(success)(
        derifree.Error.BadNumerics("Correlation matrix is not PSD.")
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
