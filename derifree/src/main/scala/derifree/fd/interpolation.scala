package derifree
package fd

object interpolation:

  def naturalCubicSpline(xs: IndexedSeq[Double], ys: IndexedSeq[Double]): Double => Double =
    val spline = CubicSpline.natural(xs, ys)
    (strike: Double) => spline(strike)
