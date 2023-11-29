package derifree

import cats.kernel.Order
import java.time.Instant

trait TimeOrder[T] extends Order[T]:

  def yearFractionBetween(x: T, y: T): YearFraction

object TimeOrder:

  def forYearFraction: TimeOrder[YearFraction] = new TimeOrder[YearFraction]:

    def compare(x: YearFraction, y: YearFraction): Int =
      if x == y then 0 else if x < y then -1 else 1

    def yearFractionBetween(x: YearFraction, y: YearFraction): YearFraction =
      y - x

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
