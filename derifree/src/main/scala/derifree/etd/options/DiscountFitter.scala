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

import scala.math.abs
import scala.math.log

/** Fit discount curves from snapshots of European-style option prices using the obervation that
  * for a given expiry T and strike K, by put-call parity:
  * ```
  * C - P = D (F - K)
  * ```
  * where D is the discount factor, F the forward. Taking strikes K_1 != K_2,
  * ```
  * D = ((C_1 - P_1) - (C_2 - P_2)) / (K_2 - K_1)
  * ```
  */
trait DiscountFitter[T]:

  def fromSnapshot(snapshot: Snapshot, refTime: T): Either[Error, YieldCurve[T]]

object DiscountFitter:

  case class Settings(
      /** The number of strikes around ATMF we consider for extracting discount factors (and
        * forwards).
        */
      nStrikes: Int = 11,

      /** Discount factors extracted from very short expiries are too noisy. This sets the
        * shortest expiry (in days) we want to fit
        */
      nCutoffDays: Int = 30
  )

  def apply[T: TimeLike](settings: Settings) = new DiscountFitter[T]:

    private def daysBetween(d1: java.time.LocalDate, d2: java.time.LocalDate): Long =
      java.time.temporal.ChronoUnit.DAYS.between(d1, d2)

    private val lambdaGrid =
      List(0.027, 0.082, 0.16, 0.25, 0.5, 0.75, 1.0, 1.5, 2.0, 3.0)

    private val benchmarkYfs =
      List(0.027, 0.082, 0.16, 0.25, 0.5, 0.75, 1.0, 1.5, 2.0, 3.0, 5.0)
        .map(derifree.YearFraction(_))

    override def fromSnapshot(snapshot: Snapshot, refTime: T): Either[Error, YieldCurve[T]] =
      require(snapshot.exerciseStyle == ExerciseStyle.European, "exercise must be European")

      val refDate = snapshot.timestamp.atZoneSameInstant(snapshot.expiryZone).toLocalDate
      val spot = (
        snapshot.underlying.bid.filter(_ > 0),
        snapshot.underlying.ask.filter(_ > 0)
      )
        .mapN((bid, ask) => (bid + ask) / 2)
        .getOrElse(snapshot.underlying.last)

      require(spot > 0, "invalid spot price")

      val quotesByExpiry = snapshot.quotes
        .groupBy(_.expiry)
        .filter((d, _) => daysBetween(refDate, d) > settings.nCutoffDays)

      def maxSpotRateChange(
          p1: derifree.rates.nelsonsiegel.Params,
          p2: derifree.rates.nelsonsiegel.Params
      ): Double =
        val y = derifree.YearFraction.oneYear
        benchmarkYfs.map(t => abs(p1.spotRate(t) * y - p2.spotRate(t) * y)).max

      def fitForwards(
          params: derifree.rates.nelsonsiegel.Params,
          prevForwards: Option[Map[java.time.LocalDate, Double]]
      ): Map[java.time.LocalDate, Double] =
        val yc = derifree.YieldCurve.nelsonSiegel[derifree.YearFraction](
          params,
          derifree.YearFraction.zero
        )
        quotesByExpiry.map: (expiry, options) =>
          // if we don't have a first estimate of the forward yet, we fall back on spot
          val prevForward =
            prevForwards.flatMap(_.get(expiry)).getOrElse(spot.toDouble)

          val callsByStrike =
            options.filter(_.isCall).groupByNel(_.strike).map(_ -> _.head)

          val putsByStrike =
            options.filter(_.isPut).groupByNel(_.strike).map(_ -> _.head)

          // pick strikes closest to the (estimated) forward
          val strikes = options
            .map(_.strike)
            .toSet
            .toList
            .sortBy(k => abs((k - prevForward).toDouble))

          val t = derifree.YearFraction(daysBetween(refDate, expiry) / 365.0)

          val fwds = strikes
            .map { strike =>
              for
                p <- putsByStrike.get(strike)
                c <- callsByStrike.get(strike)
                df = yc.discountFactor(t)
                f = (c.mid - p.mid) / df + strike
              yield f
            }
            .collect:
              // discard obvious outliers
              case Some(f) if f > 0.1 * spot && f < 10.0 * spot => f
            .take(settings.nStrikes)
            .sorted
            .toIndexedSeq

          val f = fwds(fwds.length / 2)
          expiry -> f.toDouble

      def fitParams(
          forwards: Option[Map[java.time.LocalDate, Double]]
      ): Either[derifree.Error, derifree.rates.nelsonsiegel.Params] =
        val dfsByExpiry = quotesByExpiry.map: (expiry, options) =>
          // if we don't have a first estimate of the forward yet, we fall back on spot
          val forward = forwards.flatMap(_.get(expiry)).getOrElse(spot.toDouble)

          val callsByStrike =
            options.filter(_.isCall).groupByNel(_.strike).map(_ -> _.head)

          val putsByStrike =
            options.filter(_.isPut).groupByNel(_.strike).map(_ -> _.head)

          // pick strikes closest to the (estimated) forward
          val strikes = options
            .map(_.strike)
            .toSet
            .toList
            .sortBy(k => abs((k - forward).toDouble))

          // TODO: what's the best way to pick pairs of strikes?
          val dfs = strikes
            .zip(strikes.tail)
            .map { (k1, k2) =>
              for
                p1 <- putsByStrike.get(k1)
                c1 <- callsByStrike.get(k1)
                p2 <- putsByStrike.get(k2)
                c2 <- callsByStrike.get(k2)
              yield ((c1.mid - p1.mid) - (c2.mid - p2.mid)).toDouble / (k2 - k1).toDouble
            }
            .collect {
              // discard obvious outliers
              case Some(df) if df > 0.01 && df < 10.0 => df
            }
            .take(settings.nStrikes)
            .sorted
            .toIndexedSeq

          val df = dfs(dfs.length / 2)
          expiry -> df

        val dfs = dfsByExpiry.toList.sortBy(_(0))
        val spotRates = dfs.map: (expiry, df) =>
          val yf = daysBetween(refDate, expiry) / 365.0
          val r = -log(df) / yf
          derifree.YearFraction(yf) -> derifree.Rate(r)

        derifree.rates.nelsonsiegel.fitByOlsOnLambdaGrid(spotRates, lambdaGrid)

      def go(
          params1: Option[derifree.rates.nelsonsiegel.Params],
          params2: Option[derifree.rates.nelsonsiegel.Params],
          forwards: Option[Map[java.time.LocalDate, Double]],
          iters: Int
      ) =
        Either.raiseWhen(iters > 10)(derifree.Error.BadNumerics("max iters exceeded")) *>
          fitParams(forwards)
            .map(p => (params2, p.some, fitForwards(p, forwards).some, iters + 1))

      // Fixed point iteration:
      // To estimate the discount factors we want to pick strikes
      // close to the forward (for liquidity).
      // This means we also have to estimate the forward,
      // for which we need the discount:
      // F = (C - P) / D + K
      // For our first estimate of the discount
      // we assume forward = spot. We then use
      // the estimated discount to back out
      // estimated forwards, which we then use to improve
      // our estimate of the discount, etc.
      // We stop when the spot rates change
      // less than a given threshold.
      val resultE = (
        none[derifree.rates.nelsonsiegel.Params],
        none[derifree.rates.nelsonsiegel.Params],
        none[Map[java.time.LocalDate, Double]],
        0
      ).iterateUntilM(go)((p1, p2, _, _) =>
        (p1, p2).tupled.exists(maxSpotRateChange(_, _) < 0.0001)
      )

      // resultE.foreach((_, _, fwds, iters) =>
      //   println(s"iters=$iters, forwards: ${fwds.get.toList.sortBy(_(0)).mkString("\n")}")
      // )

      val paramsE = resultE.map((_, p, _, _) => p.get) // `get` is safe here
      paramsE.map(YieldCurve.nelsonSiegel[T](_, refTime))
