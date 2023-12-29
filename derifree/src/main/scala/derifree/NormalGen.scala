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

import cats.data.{State => cState}

import scala.collection.View
import scala.collection.immutable.ArraySeq

sealed trait NormalGen:

  type State

  def init(offset: Int): State

  def dimensions: (Int, Int)

  /** Nested `IndexedSeq` dimensions equal `dimensions`. */
  def next: cState[State, IndexedSeq[IndexedSeq[Double]]]

  def skipTo(pos: Int): cState[State, Unit]

  def view(offset: Int): View[IndexedSeq[IndexedSeq[Double]]] =
    View.Unfold(init(offset)): s =>
      val (nextState, xs) = next.run(s).value
      Some((xs, nextState))

object NormalGen:

  trait Factory:
    def apply(m: Int, n: Int): Either[derifree.Error, NormalGen]

  object Factory:

    def sobol(dirs: Sobol.DirectionNumbers): Factory = new Factory:
      def apply(m: Int, n: Int): Either[Error, NormalGen] =
        fromSobol(m, n, dirs)

  def fromSobol(
      m: Int,
      n: Int,
      dirs: Sobol.DirectionNumbers
  ): Either[Sobol.Error, NormalGen] =
    Sobol(m * n, dirs).map: sobol =>
      val bbt = BrownianBridge.transform(m * n)
      new NormalGen:
        type State = Sobol.State

        def dimensions: (Int, Int) = (m, n)

        def init(offset: Int): State = sobol.initialState(offset)

        def skipTo(pos: Int): cState[State, Unit] = sobol.skipTo(pos)

        def next: cState[State, IndexedSeq[IndexedSeq[Double]]] =
          sobol.next.map: sobolNumbers =>
            val zs0 = sobolNumbers.toArray
            var i = 0
            while (i < m * n) {
              zs0(i) = normal.inverseCdf(zs0(i))
              i += 1
            }
            bbt(zs0)
            val zs = Array.ofDim[Double](m, n)

            i = 0
            while (i < m) {
              var j = 0
              while (j < n) {
                zs(i)(j) = zs0(i * n + j)
                j += 1
              }
              i += 1
            }

            ArraySeq.unsafeWrapArray(zs.map(ArraySeq.unsafeWrapArray))
