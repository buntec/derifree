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

package derifree.normal

import org.apache.commons.math3.distribution.NormalDistribution
import org.apache.commons.math3.util.{FastMath => math}

def cdf(x: Double): Double = Hill.normalCdf(x)

def pdf(x: Double): Double = normalDistribution.density(x)

def inverseCdf(p: Double): Double = Acklam.inverseNormalCdf(p)

private val normalDistribution = new NormalDistribution()

private object Hill:

  // David Hill, Algorithm AS 66, Applied Statistics, Volume 22, Number 3, 1973, pages 424--427
  def normalCdf(x: Double): Double =
    val a1 = 5.75885480458
    val a2 = 2.62433121679
    val a3 = 5.92885724438
    val b1 = -29.8213557807
    val b2 = 48.6959930692
    val c1 = -0.000000038052
    val c2 = 0.000398064794
    val c3 = -0.151679116635
    val c4 = 4.8385912808
    val c5 = 0.742380924027
    val c6 = 3.99019417011
    val con = 1.28
    val d1 = 1.00000615302
    val d2 = 1.98615381364
    val d3 = 5.29330324926
    val d4 = -15.1508972451
    val d5 = 30.789933034
    val ltone = 7.0
    val p = 0.398942280444
    val q = 0.39990348504
    val r = 0.398942280385
    val utzero = 18.66

    var value = 0.0
    var y = 0.0
    var z = 0.0
    var up = false
    z = x

    if (z < 0.0) {
      up = !up
      z = -z
    }

    if (ltone < z && ((!up) || utzero < z)) {
      if (up) {
        value = 0.0
      } else {
        value = 1.0
      }
      return value
    }

    y = 0.5 * z * z

    if (z <= con) {
      value = 0.5 - z * (p - q * y / (y + a1 + b1 / (y + a2 + b2 / (y + a3))))
    } else {
      value = r * math.exp(
        -y
      ) / (z + c1 + d1 / (z + c2 + d2 / (z + c3 + d3 / (z + c4 + d4 / (z + c5 + d5 / (z + c6))))))
    }

    if (!up) {
      value = 1.0 - value
    }

    return value

private object Acklam:

  def inverseNormalCdf(p: Double): Double =
    require(p > 0.0 & p < 1.0, "p=$p is not in (0, 1)")

    val a0 = -3.969683028665376e01
    val a1 = 2.209460984245205e02
    val a2 = -2.759285104469687e02
    val a3 = 1.383577518672690e02
    val a4 = -3.066479806614716e01
    val a5 = 2.506628277459239e00
    val b0 = -5.447609879822406e01
    val b1 = 1.615858368580409e02
    val b2 = -1.556989798598866e02
    val b3 = 6.680131188771972e01
    val b4 = -1.328068155288572e01
    val c0 = -7.784894002430293e-03
    val c1 = -3.223964580411365e-01
    val c2 = -2.400758277161838e00
    val c3 = -2.549732539343734e00
    val c4 = 4.374664141464968e00
    val c5 = 2.938163982698783e00
    val d0 = 7.784695709041462e-03
    val d1 = 3.224671290700398e-01
    val d2 = 2.445134137142996e00
    val d3 = 3.754408661907416e00
    val low = 0.02425
    val high = 0.97575
    if (p < low) {
      val q = math.sqrt(-2.0 * math.log(p))
      (((((c0 * q + c1) * q + c2) * q + c3) * q + c4) * q + c5) / ((((d0 * q + d1) * q + d2) * q + d3) * q + 1.0)
    } else if (p > high) {
      val q = math.sqrt(-2.0 * math.log(1.0 - p))
      -(((((c0 * q + c1) * q + c2) * q + c3) * q + c4) * q + c5) / ((((d0 * q + d1) * q + d2) * q + d3) * q + 1.0)
    } else {
      val q = p - 0.5
      val r = q * q
      ((((((a0 * r + a1) * r + a2) * r + a3) * r + a4) * r + a5) * q / (((((b0 * r + b1) * r + b2) * r + b3) * r + b4) * r + 1.0))
    }
