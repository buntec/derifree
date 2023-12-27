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

import cats.Monoid
import cats.syntax.all.*

object Simulation:

  case class Spec[T](
      spotObs: Map[String, Set[T]],
      discountObs: Set[T],
      putTimes: Set[T],
      callTimes: Set[T]
  )

  object Spec:

    def make[T](
        spotObs: Map[String, Set[T]] = Map.empty[String, Set[T]],
        discountObs: Set[T] = Set.empty[T],
        putTimes: Set[T] = Set.empty[T],
        callTimes: Set[T] = Set.empty[T]
    ): Spec[T] = Spec(spotObs, discountObs, putTimes, callTimes)

    def spotObs[T](ticker: String, times: Set[T]) =
      make[T](spotObs = Map(ticker -> times))

    def discountObs[T](time: T) = make(discountObs = Set(time))

    def putTime[T](time: T) = make(putTimes = Set(time))

    def callTime[T](time: T) = make(callTimes = Set(time))

    given [T]: Monoid[Spec[T]] = new Monoid[Spec[T]]:
      def empty: Spec[T] = make()
      def combine(x: Spec[T], y: Spec[T]): Spec[T] =
        // we cannot simply combine spotObs b/c the
        // default monoid instance for Map will not combine values
        val spotObs = (x.spotObs.toList ++ y.spotObs.toList)
          .groupBy((udl, _) => udl)
          .map((udl, l) => udl -> l.map(_(1)).combineAll)
        make(
          spotObs,
          x.discountObs |+| y.discountObs,
          x.putTimes |+| y.putTimes,
          x.callTimes |+| y.callTimes
        )

  /** Relization of a piecewise diffusion. */
  case class Realization[T](
      timeIndex: Map[T, Int],
      ts: IndexedSeq[YearFraction], // t_i
      deltaTs: IndexedSeq[YearFraction], // t_{i+1} - t_i
      spots: Map[String, IndexedSeq[Double]],
      logSpots: Map[String, IndexedSeq[Double]],
      jumps: Map[String, IndexedSeq[Double]], // Jump(t_i) = S(t_i) - S(t_i-)
      vols: Map[String, IndexedSeq[Vol]],
      discounts: IndexedSeq[Double] // discount factors
  )
