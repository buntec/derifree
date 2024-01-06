package derifree
package fd

import org.apache.commons.math3.analysis.interpolation.SplineInterpolator

object interpolation:

  def naturalCubicSpline(xs: IndexedSeq[Double], ys: IndexedSeq[Double]): Double => Double =
    val interpolation =
      (new SplineInterpolator()).interpolate(xs.toArray, ys.toArray)
    (strike: Double) => interpolation.value(strike)
