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

import cats.*

import scala.annotation.targetName

private val doubleOrdering: Ordering[Double] = Ordering[Double]
private val doubleFractional = Fractional[Double]
private val doubleMonoid = Monoid[Double]
private val doubleShow: Show[Double] = Show[Double]
private val doubleEq: Eq[Double] = Eq[Double]
private val longOrdering: Ordering[Long] = Ordering[Long]

/** Present value */
opaque type PV = Double

object PV {

  def apply(pv: Double): PV = pv

  given Fractional[PV] = doubleFractional

  given Monoid[PV] = doubleMonoid

  given Show[PV] = doubleShow

  given Eq[PV] = doubleEq

  extension (pv: PV)
    def toDouble: Double = pv
    def *(a: Double): PV = a * pv
    def +(rhs: PV): PV = pv + rhs
    def /(n: Int): PV = pv / n

}

/** Annualized interest rate */
opaque type Rate = Double

object Rate:

  def apply(r: Double): Rate = r

  extension (r: Rate)
    def toDouble: Double = r
    def *(t: YearFraction): Double = r * t
    def unary_- : Rate = -r

opaque type YearFraction = Double

object YearFraction:

  def apply(t: Double): YearFraction = t

  val zero = YearFraction(0.0)
  val oneMilli = YearFraction(1.0 / 365 / 24 / 60 / 60 / 1000)
  val oneSecond = YearFraction(1.0 / 365 / 24 / 60 / 60)
  val oneMinute = YearFraction(1.0 / 365 / 24 / 60)
  val oneHour = YearFraction(1.0 / 365 / 24)
  val oneDay: YearFraction = 1.0 / 365
  val oneYear: YearFraction = 1.0

  given Ordering[YearFraction] = doubleOrdering

  given Show[YearFraction] = doubleShow

  extension (t: YearFraction)
    def toDouble: Double = t
    def -(rhs: YearFraction): YearFraction = t - rhs
    def <(rhs: YearFraction): Boolean = t < rhs
    def +(rhs: YearFraction): YearFraction = t + rhs
    def *(a: Double): YearFraction = t * a
    def *(n: Int): YearFraction = t * n
    def *(n: Long): YearFraction = t * n
    @targetName("multByRate") def *(r: Rate): Double = t * r
    def /(n: Int): YearFraction = t / n
    def /(yf: YearFraction): Double = t / yf

opaque type Tick = Long

object Tick:

  def apply(n: Long): Tick = n

  extension (t: Tick)
    def toLong: Long = t
    def +(rhs: Tick): Tick = t + rhs
    def -(rhs: Tick): Tick = t - rhs

  given Ordering[Tick] = longOrdering

/** Annualized variance */
opaque type Var = Double

object Var:

  def apply(v: Double): Var = v

  extension (v: Var)
    def toDouble: Double = v
    def +(rhs: Var): Var = v + rhs
    def *(rhs: YearFraction): Double = v * rhs

/** Annualized volatility */
opaque type Vol = Double

object Vol:

  def apply(v: Double): Vol = v

  extension (v: Vol)
    def toDouble: Double = v
    def +(rhs: Vol): Vol = v + rhs
    def *(rhs: Vol): Var = v * rhs

trait QuantitiesSyntax:

  extension (x: Double)
    def pv: PV = PV(x)
    def vol: Vol = Vol(x)
    def `var`: Var = Var(x)
    def rate: Rate = Rate(x)
    def yf: YearFraction = YearFraction(x)
