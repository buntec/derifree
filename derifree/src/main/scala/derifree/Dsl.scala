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
import cats.Monoid
import cats.free.Free
import cats.free.Free.*

trait Dsl[T]:
  self =>

  import Barrier.*
  enum Barrier:
    case Continuous(
        direction: Direction,
        levels: Map[String, Double],
        from: T,
        to: T,
        policy: Policy = Policy.Or
    )
    case Discrete(
        direction: Direction,
        levels: Map[String, List[(T, Double)]],
        policy: Policy = Policy.Or
    )

  object Barrier:
    enum Direction:
      case Down, Up

    enum Policy:
      case And, Or

  sealed trait RVA[A]
  case class Spot(ticker: String, time: T) extends RVA[Double]
  case class Cashflow(amount: Double, time: T) extends RVA[PV]
  case class HitProb(barrier: Barrier) extends RVA[Double]
  case class Puttable(amount: Option[Double], time: T) extends RVA[Unit]
  case class Callable(amount: Option[Double], time: T) extends RVA[Unit]

  /** A random variable measurable with respect to a simulation. */
  type RV[A] = Free[RVA, A]

  type ContingentClaim = RV[Unit]

  def pure[A](a: A): RV[A] = Monad[RV].pure(a)

  def spot(ticker: String, time: T): RV[Double] =
    liftF[RVA, Double](Spot(ticker, time))

  def cashflow(amount: Double, time: T): RV[PV] =
    liftF[RVA, PV](Cashflow(amount, time))

  def hitProb(barrier: Barrier): RV[Double] =
    liftF[RVA, Double](HitProb(barrier))

  def survivalProb(barrier: Barrier): RV[Double] =
    liftF[RVA, Double](HitProb(barrier)).map(1 - _)

  def puttable(amount: Option[Double], time: T): RV[Unit] =
    liftF[RVA, Unit](Puttable(amount, time))

  /** Alias for `puttable`. */
  def exercisable(amount: Option[Double], time: T): RV[Unit] =
    liftF[RVA, Unit](Puttable(amount, time))

  def callable(amount: Option[Double], time: T): RV[Unit] =
    liftF[RVA, Unit](Callable(amount, time))

  // def min[A: Ordering](as: A*): A = as.min
  def min[A: Ordering](a1: A, a2: A): A = if Ordering[A].lteq(a1, a2) then a1 else a2

  def min[A: Ordering](a1: A, a2: A, a3: A): A =
    if Ordering[A].lteq(a1, a2) then min(a1, a3) else min(a2, a3)

  def min[A: Ordering](a1: A, a2: A, a3: A, a4: A): A =
    if Ordering[A].lteq(a1, a2) then min(a1, a3, a4) else min(a2, a3, a4)

  // def max[A: Ordering](as: A*): A = as.max
  def max[A: Ordering](a1: A, a2: A): A = if Ordering[A].lteq(a2, a1) then a1 else a2

  def max[A: Ordering](a1: A, a2: A, a3: A): A =
    if Ordering[A].lteq(a2, a1) then max(a1, a3) else max(a2, a3)

  def max[A: Ordering](a1: A, a2: A, a3: A, a4: A): A =
    if Ordering[A].lteq(a2, a1) then max(a1, a3, a4) else max(a2, a3, a4)

  extension [A](rva: RV[A])
    def mean(
        simulator: Simulator[T],
        nSims: Int
    )(using TimeLike[T], Fractional[A], Monoid[A]): Either[derifree.Error, A] =
      Compiler[T].mean(self, simulator, nSims, rva)

  extension (cc: ContingentClaim)
    def fairValue(
        simulator: Simulator[T],
        nSims: Int
    )(using TimeLike[T]): Either[derifree.Error, PV] =
      Compiler[T].fairValue(self, simulator, nSims, cc).map(_.fairValue)

    def fairValueResult(
        simulator: Simulator[T],
        nSims: Int
    )(using TimeLike[T]): Either[derifree.Error, FairValueResult[T]] =
      Compiler[T].fairValue(self, simulator, nSims, cc)

    def callProbabilities(
        simulator: Simulator[T],
        nSims: Int
    )(using TimeLike[T]): Either[derifree.Error, Map[T, Double]] =
      Compiler[T].fairValue(self, simulator, nSims, cc).map(_.callProbabilities)

    def putProbabilities(
        simulator: Simulator[T],
        nSims: Int
    )(using TimeLike[T]): Either[derifree.Error, Map[T, Double]] =
      Compiler[T].fairValue(self, simulator, nSims, cc).map(_.putProbabilities)

object Dsl:

  def apply[T]: Dsl[T] = new Dsl[T] {}
