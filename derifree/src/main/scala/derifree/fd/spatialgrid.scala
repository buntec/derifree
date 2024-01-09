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
package fd

import org.apache.commons.math3.util.{FastMath => math}

object SpatialGrid:
  self =>

  trait Factory:

    def apply(min: Double, max: Double): IArray[Double]

  object Factory:

    def logSinh(n: Int, concentration: Double, x0: Double): Factory = new Factory:
      def apply(min: Double, max: Double): IArray[Double] =
        if min < x0 && x0 < max then
          fitAntipoint(x0, self.logSinh(min, max, n, concentration, x0))
        else self.log(min, max, n)

  enum SpatialDimension:
    case Spot, LogSpot

  enum SpatialPoint:
    case Spot(spot: Double) extends SpatialPoint
    case LogSpot(logspot: Double) extends SpatialPoint

  def toDim(point: SpatialPoint, targetDim: SpatialDimension): SpatialPoint =
    import SpatialPoint.*
    (point, targetDim) match {
      case (Spot(spot), SpatialDimension.Spot)          => Spot(spot)
      case (Spot(spot), SpatialDimension.LogSpot)       => LogSpot(math.log(spot))
      case (LogSpot(logspot), SpatialDimension.Spot)    => Spot(math.exp(logspot))
      case (LogSpot(logspot), SpatialDimension.LogSpot) => LogSpot(logspot)
    }

  def lognormalPercentile(
      vol: Double,
      expiry: Double,
      drift: Double,
      p: Double
  ): Double =
    math.exp(
      expiry * (drift - 0.5 * vol * vol) + math.sqrt(expiry) * vol * normal.inverseCdf(p)
    )

  private def sinhTransform(a: Double, x0: Double): (Double => Double) =
    val c = math.asinh(-x0 / a)
    val b = math.asinh((1.0 - x0) / a) - c
    x => x0 + a * math.sinh(b * x + c)

  def logSinh(
      min: Double,
      max: Double,
      n: Int,
      concentration: Double,
      x0: Double
  ): IArray[Double] =
    require(max > x0 && x0 > min && min > 0.0, "must have min < x0 < max")
    val a = math.log(max / min)
    IArray.unsafeFromArray(
      Array
        .tabulate(n)(i => i.toDouble / (n - 1))
        .map(sinhTransform(concentration, math.log(x0 / min) / a))
        .map(u => min * math.exp(u * a))
    )

  def log(
      min: Double,
      max: Double,
      n: Int
  ): IArray[Double] =
    require(max > min && min > 0.0, "must have 0 < min < max")
    val a = math.log(max / min)
    IArray.unsafeFromArray(
      Array
        .tabulate(n)(i => i.toDouble / (n - 1))
        .map(u => min * math.exp(u * a))
    )

  def fitAntipoint(antipoint: Double, grid: IArray[Double]): IArray[Double] =
    require(
      antipoint >= grid.min && antipoint <= grid.max,
      "antipoint outside end points"
    )
    val i = {
      val j = grid.search(antipoint).insertionPoint
      if (j >= 0) j else -(j + 1)
    }
    val x1 = grid(i - 1)
    val x2 = grid(i)
    val alpha = antipoint - (x1 + x2) / 2.0
    grid.map(_ + alpha)

  def addBoundaryValues(
      grid: IArray[Double],
      interiorValues: IArray[Double],
      lowerBoundary: BoundaryCondition,
      upperBoundary: BoundaryCondition
  ): IArray[Double] =
    val n = grid.length - 2 // number of interior points
    require(interiorValues.length == n, "dimension mismatch")
    val vLeft = lowerBoundary match
      case BoundaryCondition.Linear =>
        val dxUp = grid(2) - grid(1)
        val dxDown = grid(1) - grid(0)
        (dxUp + dxDown) / dxUp * interiorValues(0) - dxDown / dxUp * interiorValues(1)
      case BoundaryCondition.Dirichlet(_, value) => value
    val vRight = upperBoundary match
      case BoundaryCondition.Linear =>
        val dxUp = grid(n + 1) - grid(n)
        val dxDown = grid(n) - grid(n - 1)
        (dxUp + dxDown) / dxDown * interiorValues(n - 1) - dxUp / dxDown * interiorValues(
          n - 2
        )
      case BoundaryCondition.Dirichlet(_, value) =>
        value
    vLeft +: interiorValues :+ vRight
