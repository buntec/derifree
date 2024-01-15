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

import scala.math.exp

trait YieldCurve[T]:
  def discountFactor(t: T): Double
  def discountFactor(t1: T, t2: T): Double
  def spotRate(t: T): Rate

object YieldCurve:

  def fromContinuouslyCompoundedRate[T: TimeLike](r: Rate, refTime: T): YieldCurve[T] =
    new YieldCurve[T]:
      def discountFactor(t: T): Double = discountFactor(refTime, t)
      def discountFactor(t1: T, t2: T): Double =
        val dt = TimeLike[T].yearFractionBetween(t1, t2)
        exp(-r * dt)
      def spotRate(t: T): Rate = r

  def zero[T]: YieldCurve[T] = new YieldCurve[T]:
    def discountFactor(at: T): Double = 1.0
    def discountFactor(t1: T, t2: T): Double = 1.0
    def spotRate(t: T): Rate = Rate(0.0)
