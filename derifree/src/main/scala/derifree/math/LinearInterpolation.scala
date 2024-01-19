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

package derifree.math

object LinearInterpolation:

  /** O(log n) linear interpolation on a non-uniform grid; extrapolation is flat. */
  def withFlatExtrapolation(
      xs: IndexedSeq[Double],
      ys: IndexedSeq[Double]
  ): (Double => Double) =
    require(xs.length == ys.length && xs.length >= 1, "dimension mismatch or empty input")
    if xs.length == 1 then _ => ys(0)
    else
      x =>
        if x <= xs(0) then ys(0)
        else if x >= xs(xs.length - 1) then ys(ys.length - 1)
        else
          val i = xs.search(x).insertionPoint
          val x1 = xs(i - 1)
          val x2 = xs(i)
          val y1 = ys(i - 1)
          val y2 = ys(i)
          y1 + (y2 - y1) / (x2 - x1) * (x - x1)

  /** O(log n) linear interpolation on a non-uniform grid; extrapolation is linear with user
    * provided slopes.
    */
  def withLinearExtrapolation(
      xs: IndexedSeq[Double],
      ys: IndexedSeq[Double],
      leftSlope: Double,
      rightSlope: Double
  ): (Double => Double) =
    val n = xs.length
    require(n >= 2, "need at least two knots")
    require(n == ys.length, "dimension mismatch")
    x =>
      if x <= xs(0) then ys(0) + (x - xs(0)) * leftSlope
      else if x >= xs(n - 1) then ys(n - 1) + (x - xs(n - 1)) * rightSlope
      else
        val i = xs.search(x).insertionPoint
        val x1 = xs(i - 1)
        val x2 = xs(i)
        val y1 = ys(i - 1)
        val y2 = ys(i)
        y1 + (y2 - y1) / (x2 - x1) * (x - x1)

  /** O(1) linear interpolation on an equispaced grid; extrapolation is flat. */
  def onEquidistantGridWithFlatExtrapolation(
      xMin: Double,
      xMax: Double,
      ys: IndexedSeq[Double]
  ): (Double => Double) =
    val n = ys.length
    require(n >= 2, "need at least 2 knots")
    require(xMin < xMax, "must have xMin < xMax")
    val dx = (xMax - xMin) / (n - 1)
    x =>
      if x <= xMin then ys(0)
      else if x >= xMax then ys(n - 1)
      else
        val xr = x - xMin
        val k = (xr / dx).toInt // flooring
        val r = (xr / dx) - k
        (1.0 - r) * ys(k) + r * ys(k + 1)
