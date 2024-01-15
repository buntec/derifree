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

import cats.data.State
import org.apache.commons.math3.random.MersenneTwister

import scala.collection.AbstractView
import scala.collection.View
import scala.collection.immutable.ArraySeq

/** Produces uncorrelated standard normal variates, one (m x n) matrix at a time. */
sealed trait NormalGen:

  /** (m x n) */
  def dimensions: (Int, Int)

  /** Nested `IndexedSeq` dimensions equal `dimensions`. */
  def view(offset: Int): View[IndexedSeq[IndexedSeq[Double]]]

object NormalGen:

  trait Factory:
    def apply(m: Int, n: Int): Either[derifree.Error, NormalGen]

  object Factory:

    def mersenneTwister: Factory = new Factory:
      def apply(m: Int, n: Int): Either[Error, NormalGen] = fromMersenneTwister(m, n)

    def sobol(dirs: Sobol.DirectionNumbers): Factory = new Factory:
      def apply(m: Int, n: Int): Either[Error, NormalGen] =
        fromSobol(m, n, dirs)

  def fromMersenneTwister(m: Int, n: Int): Either[derifree.Error, NormalGen] =
    Right(
      new NormalGen:
        def dimensions: (Int, Int) = (m, n)
        def view(offset: Int): View[IndexedSeq[IndexedSeq[Double]]] =
          new AbstractView[IndexedSeq[IndexedSeq[Double]]]:
            override def iterator: Iterator[IndexedSeq[IndexedSeq[Double]]] =
              val mt = MersenneTwister(0)

              for (_ <- 0 until offset)
              do mt.nextDouble()

              Iterator.continually:
                ArraySeq.unsafeWrapArray(
                  Array.fill(m)(
                    ArraySeq.unsafeWrapArray(Array.fill(n)(normal.inverseCdf(mt.nextDouble())))
                  )
                )
    )

  def fromSobol(
      m: Int,
      n: Int,
      dirs: Sobol.DirectionNumbers
  ): Either[Sobol.Error, NormalGen] =
    Sobol(m * n, dirs).map: sobol =>
      val bbt = BrownianBridge.transform(m * n)
      new NormalGen:
        type S = Sobol.S

        def dimensions: (Int, Int) = (m, n)

        def init(offset: Int): S = sobol.initialState(offset)

        def skipTo(pos: Int): State[S, Unit] = sobol.skipTo(pos)

        def next: State[S, IndexedSeq[IndexedSeq[Double]]] =
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

        def view(offset: Int): View[IndexedSeq[IndexedSeq[Double]]] =
          View.Unfold(init(offset)): s =>
            val (nextState, xs) = next.run(s).value
            Some((xs, nextState))
