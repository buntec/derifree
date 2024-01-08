package derifree
package fd

import org.apache.commons.math3.util.{FastMath => math}
import scala.collection.immutable.ArraySeq

object SpatialGrid:
  self =>

  trait Factory:

    def apply(min: Double, max: Double): ArraySeq[Double]

  object Factory:

    def logSinh(n: Int, concentration: Double, x0: Double): Factory = new Factory:
      def apply(min: Double, max: Double): ArraySeq[Double] =
        self.logSinh(min, max, n, concentration, x0)

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
  ): ArraySeq[Double] =
    require(max > x0 && x0 > min && min > 0.0, "must have min < x0 < max")
    val a = math.log(max / min)
    ArraySeq.unsafeWrapArray(
      Array
        .tabulate(n)(i => i.toDouble / (n - 1))
        .map(sinhTransform(concentration, math.log(x0 / min) / a))
        .map(u => min * math.exp(u * a))
    )

  def fitAntipoint(antipoint: Double, grid: ArraySeq[Double]): ArraySeq[Double] =
    require(
      antipoint >= grid.min && antipoint <= grid.max,
      "antipoint outside end points"
    )
    val i = {
      val j = java.util.Arrays.binarySearch(grid.toArray, antipoint)
      if (j >= 0) j else -(j + 1)
    }
    val x1 = grid(i - 1)
    val x2 = grid(i)
    val alpha = antipoint - (x1 + x2) / 2.0
    grid.map(_ + alpha)
