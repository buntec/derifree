package derifree
package etd
package options

import derifree.syntax.*
import derifree.syntax.given

import cats.syntax.all.*
import scala.math.Ordering.Implicits.*
import scala.math.{abs, exp}

trait BorrowFitter[T]:

  def fromSnapshot(
      snapshot: Snapshot,
      forward: Forward[T],
      discount: YieldCurve[T],
      refTime: T
  ): Either[Error, YieldCurve[T]]

object BorrowFitter:

  case class Settings(nStrikes: Int = 5, useTermStructure: Boolean = true)

  def apply[T: TimeLike](settings: Settings): BorrowFitter[T] = new BorrowFitter[T]:
    def fromSnapshot(
        snapshot: Snapshot,
        forward: Forward[T],
        discount: YieldCurve[T],
        refTime: T
    ): Either[Error, YieldCurve[T]] =
      val t0 = snapshot.timestamp.toInstant
      val quotesByExpiry = snapshot.quotes
        .groupBy(_.expiry)
        .filter: (expiry, quotes) =>
          val t = expiry.atTime(16, 0).atZone(snapshot.expiryZone).toInstant
          t0.yearFractionTo(t) > YearFraction.oneDay * 30

      quotesByExpiry.toList
        .sortBy(_(0))
        .foldLeft(List.empty[(T, Double)]):
          case (acc, (expiry, quotes)) =>
            val t = expiry.atTime(16, 0).atZone(snapshot.expiryZone).toInstant
            val yf = t0.yearFractionTo(t)
            val expiryT = refTime.plusYearFraction(yf)
            val fwd = forward(expiryT)

            val quotesbyStrike = quotes
              .groupBy(_.strike)
              .toList
              .sortBy((strike, _) => abs(strike.toDouble - fwd))
              .take(settings.nStrikes)

            def borrow(r: Double) =
              val p = expiryT -> exp(-yf.toDouble * r)
              YieldCurve.linearRTFromDiscountFactors[T](
                if settings.useTermStructure then acc :+ p
                else List(p),
                refTime
              )

            def iv(o: OptionQuote, borrow: YieldCurve[T]) =
              derifree.etd.options.ImpliedVol.american(
                o.mid.toDouble,
                o.strike.toDouble,
                expiryT,
                o.isCall,
                refTime,
                forward.withBorrow(borrow),
                discount
              )

            def pcDisparity(borrow: YieldCurve[T]) = quotesbyStrike.collectFoldSome:
              (strike, quotes) =>
                for
                  call <- quotes.find(_.isCall)
                  ivCall <- iv(call, borrow).toOption
                  put <- quotes.find(_.isPut)
                  ivPut <- iv(put, borrow).toOption
                yield abs(ivCall - ivPut)

            ???

      ???
