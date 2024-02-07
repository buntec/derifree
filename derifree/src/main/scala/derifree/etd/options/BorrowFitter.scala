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
package etd
package options

import cats.syntax.all.*
import derifree.dtos.etd.options.*
import derifree.syntax.*

import scala.math.abs
import scala.math.exp

/** Fits borrow curves based on put-call parity of option implied vols. */
trait BorrowFitter[T]:

  def fromSnapshot(
      snapshot: Snapshot,
      forward: Forward[T],
      discount: YieldCurve[T],
      refTime: T
  ): Either[Error, YieldCurve[T]]

object BorrowFitter:

  case class Settings(
      /** The number of strikes around ATMF to consider for put-call parity. */
      nStrikes: Int = 5,

      /** Whether to use the term-structure of the borrow curve in the bootstrap. */
      useTermStructure: Boolean = true,

      /** The short end is noisy so we skip this many days in the fit. */
      minDaysToExpiry: Int = 30,

      /** Bounds the absolute value of the borrow spot rate at any expiry. */
      maxRate: Double = 0.05,

      /** Whether to use the mid option price to compute the mid vol. If `false` then we take
        * the average of the bid and ask vols, which is twice as expensive.
        */
      midVolFromMidPrice: Boolean = true
  )

  def apply[T: TimeLike](settings: Settings): BorrowFitter[T] = new BorrowFitter[T]:

    private def daysBetween(d1: java.time.LocalDate, d2: java.time.LocalDate): Long =
      java.time.temporal.ChronoUnit.DAYS.between(d1, d2)

    def fromSnapshot(
        snapshot: Snapshot,
        forward: Forward[T],
        discount: YieldCurve[T],
        refTime: T
    ): Either[Error, YieldCurve[T]] =
      val t0 = snapshot.timestamp.toInstant

      // Group option quotes by expiry and
      // filter short-end expiries.
      val quotesByExpiry = snapshot.quotes
        .groupBy(_.expiry)
        .filter((expiry, _) =>
          daysBetween(
            snapshot.timestamp.atZoneSameInstant(snapshot.expiryZone).toLocalDate,
            expiry
          ) > settings.minDaysToExpiry
        )

      // Bootstrap discount factors of the borrow curve starting from the first expiry.
      val fittedDiscountsE = quotesByExpiry.toList
        .sortBy(_(0)) // sort by expiry
        .foldLeftM(List.empty[(T, Double)]):
          case (acc, (expiry, quotes)) =>
            // compute year fraction to expiry
            // TODO: we imprecisely assume expiry at 4pm in the exchange's time zone
            val yf =
              t0.yearFractionTo(expiry.atTime(16, 0).atZone(snapshot.expiryZone).toInstant)
            val expiryT = refTime.plusYearFraction(yf)

            // pick strikes closest to the forward
            val fwd = forward(expiryT)
            val quotesbyStrike = quotes
              .groupBy(_.strike)
              .toList
              .sortBy((strike, _) => abs(strike.toDouble - fwd))
              .take(settings.nStrikes)

            // The next borrow curve as a function of the spot rate.
            // If the setting is to take term-structure into account,
            // then we take all previously fitted discounts and tack
            // this discount factor onto the end. Otherwise we use a flat curve.
            def borrow(r: Double) =
              if settings.useTermStructure then
                YieldCurve.linearRTFromDiscountFactors[T](
                  acc :+ (expiryT -> exp(-yf.toDouble * r)),
                  refTime
                )
              else YieldCurve.fromContinuouslyCompoundedRate[T](r.rate, refTime)

            def impliedVol(o: OptionQuote, borrow: YieldCurve[T]) =
              require(o.expiry == expiry) // invariant
              val f = forward.withBorrow(borrow)
              if settings.midVolFromMidPrice then
                snapshot.exerciseStyle match
                  case ExerciseStyle.American =>
                    derifree.etd.options.ImpliedVol.american(
                      o.mid.toDouble,
                      o.strike.toDouble,
                      expiryT,
                      o.isCall,
                      refTime,
                      f,
                      discount
                    )
                  case ExerciseStyle.European =>
                    derifree.etd.options.ImpliedVol.european(
                      o.mid.toDouble,
                      o.strike.toDouble,
                      expiryT,
                      expiryT,
                      o.isCall,
                      refTime,
                      f,
                      discount
                    )
              else
                val bidVol = snapshot.exerciseStyle match
                  case ExerciseStyle.American =>
                    derifree.etd.options.ImpliedVol.american(
                      o.bid.toDouble,
                      o.strike.toDouble,
                      expiryT,
                      o.isCall,
                      refTime,
                      f,
                      discount
                    )
                  case ExerciseStyle.European =>
                    derifree.etd.options.ImpliedVol.european(
                      o.bid.toDouble,
                      o.strike.toDouble,
                      expiryT,
                      expiryT,
                      o.isCall,
                      refTime,
                      f,
                      discount
                    )
                val askVol = snapshot.exerciseStyle match
                  case ExerciseStyle.American =>
                    derifree.etd.options.ImpliedVol.american(
                      o.ask.toDouble,
                      o.strike.toDouble,
                      expiryT,
                      o.isCall,
                      refTime,
                      f,
                      discount
                    )
                  case ExerciseStyle.European =>
                    derifree.etd.options.ImpliedVol.european(
                      o.ask.toDouble,
                      o.strike.toDouble,
                      expiryT,
                      expiryT,
                      o.isCall,
                      refTime,
                      f,
                      discount
                    )
                (bidVol, askVol).mapN((bid, ask) => (bid + ask) / 2)

            // At this point for every given strike there should be exactly one
            // put and one call. We compute the absolute difference
            // between their (mid) implied vols and sum over all strikes.
            // The number of strikes is limited by the settings.
            def putCallDisparity(borrow: YieldCurve[T]) = quotesbyStrike.collectFoldSome:
              (_, quotes) =>
                for
                  call <- quotes.find(_.isCall)
                  ivCall <- impliedVol(call, borrow).toOption
                  put <- quotes.find(_.isPut)
                  ivPut <- impliedVol(put, borrow).toOption
                yield abs(ivCall - ivPut)

            val objectiveFun = (r: Double) => putCallDisparity(borrow(r))

            // Find the spot borrow rate that minimizes put-call disparity.
            derifree.math.UnivariateMinimizing
              .brent(
                objectiveFun,
                -settings.maxRate,
                settings.maxRate,
                0.0,
                1e-4,
                1e-4
              )
              .map(result => acc :+ (expiryT -> exp(-yf.toDouble * result.point)))

      fittedDiscountsE.map: dfs =>
        YieldCurve.linearRTFromDiscountFactors(dfs, refTime)
