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

trait SummaryStatistics:

  def N: Int

  def max: Double

  def min: Double

  def mean: Double

  def variance: Double

  def stdDev: Double

  def percentile(p: Double): Double

  final def median: Double = percentile(0.5)

  final def summaryString: String =
    s"""--------------------------------
    | N = $N
    | min = $min
    | max = $max
    | median = $median
    | mean = $mean
    | std = $stdDev
    |--------------------------------
  """.stripMargin

object SummaryStatistics:

  trait Builder:

    def add(d: Double): Builder

    def build: SummaryStatistics

  private case class BuilderImpl(data: List[Double]) extends Builder:
    def add(d: Double): Builder = BuilderImpl(d :: data)

    def build: SummaryStatistics =
      val sum = data.sum
      val sorted = data.sorted.toIndexedSeq
      val n = sorted.length
      val mean0 = sum / n
      val max0 = data.max
      val min0 = data.min
      val variance0 = sorted.map(a => (a - mean0) * (a - mean0)).sum / n
      val stdDev0 = math.sqrt(variance0)

      new SummaryStatistics:
        def N: Int = n
        def stdDev: Double = stdDev0

        // crude but good enough for us
        def percentile(p: Double): Double =
          require(p >= 0 && p <= 1, "p must be in [0, 1]")
          sorted((p * (n - 1)).toInt)

        def variance: Double = variance0
        def max: Double = max0
        def min: Double = min0
        def mean: Double = mean0

  def builder: Builder = BuilderImpl(Nil)
