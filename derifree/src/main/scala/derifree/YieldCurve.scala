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

import cats.syntax.all.*
import derifree.syntax.*

import scala.math.exp
import scala.math.log

trait YieldCurve[T]:
  def discountFactor(t: T): Double
  def discountFactor(t1: T, t2: T): Double = discountFactor(t2) / discountFactor(t1)
  def spotRate(t: T): Rate

object YieldCurve:

  def linearRTFromDiscountFactors[T: TimeLike](
      dfs: Seq[(T, Double)],
      refTime: T
  ): YieldCurve[T] =
    val ts = dfs.map(_(0))
    require(dfs.forall((t, _) => t > refTime), "pillars must be strictly after refTime")
    require((ts zip ts.tail).forall(_ < _), "pillars must be strictly increasing")
    require(dfs.forall((_, df) => df > 0), "discount factors must be positive")

    val yfs =
      0.0 +: ts.map(t => TimeLike[T].yearFractionBetween(refTime, t).toDouble).toIndexedSeq
    val rts = 0.0 +: dfs.map((_, df) => -log(df)).toIndexedSeq
    val interp =
      math.LinearInterpolation.withLinearExtrapolation(yfs, rts, 0.0, rts.last / yfs.last)
    new YieldCurve[T]:
      def spotRate(t: T): Rate =
        val t0 = TimeLike[T].yearFractionBetween(refTime, t).toDouble
        val rt0 = interp(t0)
        (rt0 / t0).rate

      def discountFactor(t: T): Double =
        val rt = interp(TimeLike[T].yearFractionBetween(refTime, t).toDouble)
        exp(-rt)

  def fromContinuouslyCompoundedRate[T: TimeLike](r: Rate, refTime: T): YieldCurve[T] =
    new YieldCurve[T]:
      def discountFactor(t: T): Double = discountFactor(refTime, t)
      override def discountFactor(t1: T, t2: T): Double =
        val dt = TimeLike[T].yearFractionBetween(t1, t2)
        exp(-r * dt)
      def spotRate(t: T): Rate = r

  def zero[T]: YieldCurve[T] = new YieldCurve[T]:
    def discountFactor(at: T): Double = 1.0
    override def discountFactor(t1: T, t2: T): Double = 1.0
    def spotRate(t: T): Rate = Rate(0.0)
