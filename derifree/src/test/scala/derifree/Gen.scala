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

import cats.Monad
import cats.data.State
import cats.syntax.all.*

import scala.collection.View

private val stateMonadInstance: Monad[State[Gen.S, _]] = Monad[State[Gen.S, _]]

opaque type Gen[A] = State[Gen.S, A]

object Gen:

  opaque type S = Long

  extension [A](g: Gen[A])
    def view: View[A] =
      View.Unfold(init): s =>
        val (sNext, a) = g.run(s).value
        Some((a, sNext))

  given Monad[Gen] = stateMonadInstance

  val init: S = 0L

  val reset: Gen[Unit] = State.set(0L)

  def setSeed(seed: Long): Gen[Unit] = State.set(seed)

  def constant[A](a: A): Gen[A] = State.pure(a)

  def pure[A](a: A): Gen[A] = State.pure(a)

  def normal: Gen[Double] = State: s =>
    val r = scala.util.Random(s)
    (r.nextLong(), r.nextGaussian())

  def between(minInclusive: Double, maxExclusive: Double): Gen[Double] = State: s =>
    val r = scala.util.Random(s)
    (r.nextLong(), r.between(minInclusive, maxExclusive))

  def between(minInclusive: Int, maxExclusive: Int): Gen[Int] = State: s =>
    val r = scala.util.Random(s)
    (r.nextLong(), r.between(minInclusive, maxExclusive))

  val int: Gen[Int] = State: s =>
    val r = scala.util.Random(s)
    (r.nextLong(), r.nextInt())

  val boolean: Gen[Boolean] = State: s =>
    val r = scala.util.Random(s)
    (r.nextLong(), r.nextBoolean())

  def choose[A](as: A*): Gen[A] = State: s =>
    val r = scala.util.Random(s)
    val i = r.nextInt(as.length)
    (r.nextLong(), as(i))

  def weightedChoose[A](as: (A, Int)*): Gen[A] = State: s =>
    val r = scala.util.Random(s)
    val sumOfWeights = as.map(_(1)).sum
    val i = r.nextInt(sumOfWeights)
    val cum = as.flatMap((a, w) => Seq.fill(w)(a))
    (r.nextLong(), cum(i))

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = g.replicateA(n)
