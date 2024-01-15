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
import scala.concurrent.duration.FiniteDuration

trait TimeLike[T] extends Order[T]:

  def yearFractionBetween(x: T, y: T): YearFraction

  def addMillis(t: T, millis: Long): T

  final def addSeconds(t: T, seconds: Long): T = addMillis(t, seconds * 1000)

  final def addMinutes(t: T, minutes: Long): T = addSeconds(t, minutes * 60L)

  final def addHours(t: T, hours: Long): T = addMinutes(t, hours * 60L)

  final def addDays(t: T, days: Int): T = addHours(t, days * 24L)

  final def dailyStepsBetween(start: T, stop: T): List[T] =
    List.unfold(addDays(start, 1))(s => if lt(s, stop) then Some((s, addDays(s, 1))) else None)

  final def stepsBetween(start: T, stop: T, step: FiniteDuration): List[T] =
    val stepMillis = step.toMillis
    List.unfold(addMillis(start, stepMillis))(s =>
      if lt(s, stop) then Some((s, addMillis(s, stepMillis))) else None
    )

object TimeLike:

  def apply[T](using ev: TimeLike[T]): TimeLike[T] = ev

  given TimeLike[YearFraction] = new TimeLike[YearFraction]:

    def compare(x: YearFraction, y: YearFraction): Int =
      if x == y then 0 else if x < y then -1 else 1

    def yearFractionBetween(x: YearFraction, y: YearFraction): YearFraction =
      y - x

    def addMillis(t: YearFraction, millis: Long): YearFraction =
      t + YearFraction.oneMilli * millis

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

      def addMillis(t: Instant, millis: Long): Instant =
        t.plus(java.time.Duration.ofMillis(millis))

  trait Syntax:

    given orderingForTimeLike[T: TimeLike]: Ordering[T] = Order[T].toOrdering

    extension [T: TimeLike](t: T)
      def yearFractionTo(t2: T): YearFraction = TimeLike[T].yearFractionBetween(t, t2)
      def dailyStepsTo(t2: T): List[T] = TimeLike[T].dailyStepsBetween(t, t2)
      def plusDays(n: Int): T = TimeLike[T].addDays(t, n)
      def plusSeconds(n: Long): T = TimeLike[T].addSeconds(t, n)
      def plusMillis(n: Long): T = TimeLike[T].addMillis(t, n)
      def plusYearFraction(yf: YearFraction): T =
        TimeLike[T].addMillis(t, math.round(yf.toDouble * 365 * 24 * 60 * 60 * 1000))
