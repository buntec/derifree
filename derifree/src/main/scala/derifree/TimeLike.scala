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

import cats.kernel.Order

import java.time.Instant

trait TimeLike[T] extends Order[T]:

  def yearFractionBetween(x: T, y: T): YearFraction

  def dailyStepsBetween(start: T, stop: T): List[T]

  def addDays(t: T, n: Int): T

object TimeLike:

  def apply[T](using ev: TimeLike[T]): TimeLike[T] = ev

  given TimeLike[YearFraction] = new TimeLike[YearFraction]:

    def compare(x: YearFraction, y: YearFraction): Int =
      if x == y then 0 else if x < y then -1 else 1

    def yearFractionBetween(x: YearFraction, y: YearFraction): YearFraction =
      y - x

    def dailyStepsBetween(
        start: YearFraction,
        stop: YearFraction
    ): List[YearFraction] =
      val dt = YearFraction.oneDay
      List.unfold(start + dt)(s => if s < stop then Some((s, s + dt)) else None)

    def addDays(t: YearFraction, n: Int): YearFraction = t + YearFraction.oneDay * n

  given TimeLike[java.time.Instant] =
    new TimeLike[java.time.Instant]:

      def compare(x: Instant, y: Instant): Int =
        if x.equals(y) then 0
        else if x.isBefore(y) then -1
        else 1

      def yearFractionBetween(x: Instant, y: Instant): YearFraction =
        YearFraction(
          java.time.Duration
            .between(x, y)
            .toMillis / (1000.0 * 60 * 60 * 24 * 365)
        )

      def dailyStepsBetween(start: Instant, stop: Instant): List[Instant] =
        val dt = java.time.Duration.ofDays(1)
        List.unfold(start.plus(dt))(s =>
          if s.isBefore(stop) then Some((s, s.plus(dt))) else None
        )

      def addDays(t: Instant, n: Int): Instant = t.plus(java.time.Duration.ofDays(n))

trait TimeLikeSyntax:

  given orderingForTimeLike[T: TimeLike]: Ordering[T] = Order[T].toOrdering

  extension [T: TimeLike](t: T)
    def yearFractionTo(t2: T): YearFraction = TimeLike[T].yearFractionBetween(t, t2)
    def dailyStepsTo(t2: T): List[T] = TimeLike[T].dailyStepsBetween(t, t2)
    def plusDays(n: Int): T = TimeLike[T].addDays(t, n)
