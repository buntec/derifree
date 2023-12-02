package derifree

import cats.kernel.Order
import java.time.Instant

trait TimeOrder[T] extends Order[T]:

  def yearFractionBetween(x: T, y: T): YearFraction

  def dailyStepsBetween(start: T, stop: T): List[T]

object TimeOrder:

  def forYearFraction: TimeOrder[YearFraction] = new TimeOrder[YearFraction]:

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

  def forInstant: TimeOrder[java.time.Instant] =
    new TimeOrder[java.time.Instant]:

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
