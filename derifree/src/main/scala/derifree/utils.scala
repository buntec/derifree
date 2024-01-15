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

object utils:

  def correlationMatrixCholeskyDecomp(
      corrs: Map[(String, String), Double],
      udls: List[String]
  ): Either[derifree.Error, IndexedSeq[IndexedSeq[Double]]] =
    val matrixMaybe = udls
      .map(udl1 =>
        udls
          .map(udl2 =>
            if (udl1 == udl2) then 1.0.some
            else corrs.get((udl1, udl2)).orElse(corrs.get((udl2, udl1)))
          )
          .sequence
      )
      .sequence
    Either
      .fromOption(matrixMaybe, derifree.Error.MissingData("Missing correlation pair."))
      .flatMap(data => math.Cholesky.apply.decomp(data.map(_.toIndexedSeq).toIndexedSeq))
