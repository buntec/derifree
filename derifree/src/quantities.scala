package derifree

import cats.*

import scala.annotation.targetName

private val doubleOrdering: Ordering[Double] = Ordering[Double]
private val doubleFractional = Fractional[Double]
private val doubleMonoid = Monoid[Double]
private val longOrdering: Ordering[Long] = Ordering[Long]

/** Present value */
opaque type PV = Double

object PV {

  def apply(pv: Double): PV = pv

  given Fractional[PV] = doubleFractional

  given Monoid[PV] = doubleMonoid

  extension (pv: PV)
    private[derifree] def toDouble: Double = pv
    def *(a: Double): PV = a * pv
    def +(rhs: PV): PV = pv + rhs
    def /(n: Int): PV = pv / n

}

/** Annualized interest rate */
opaque type Rate = Double

object Rate:

  def apply(r: Double): Rate = r

  extension (r: Rate)
    private[derifree] def toDouble: Double = r
    def *(t: YearFraction): Double = r * t
    def unary_- : Rate = -r

opaque type YearFraction = Double

object YearFraction:

  def apply(t: Double): YearFraction = t

  val zero = YearFraction(0.0)
  val oneSecond = YearFraction(1.0 / 365 / 24 / 60 / 60)
  val oneMinute = YearFraction(1.0 / 365 / 24 / 60)
  val oneHour = YearFraction(1.0 / 365 / 24)
  val oneDay: YearFraction = 1.0 / 365
  val oneYear: YearFraction = 1.0

  given Ordering[YearFraction] = doubleOrdering

  extension (t: YearFraction)
    private[derifree] def toDouble: Double = t
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
    private[derifree] def toDouble: Double = v
    def +(rhs: Var): Var = v + rhs
    def *(rhs: YearFraction): Double = v * rhs

/** Annualized volatility */
opaque type Vol = Double

object Vol:

  def apply(v: Double): Vol = v

  extension (v: Vol)
    private[derifree] def toDouble: Double = v
    def +(rhs: Vol): Vol = v + rhs
    def *(rhs: Vol): Var = v * rhs

trait QuantitiesSyntax:

  extension (x: Double)
    def pv: PV = PV(x)
    def vol: Vol = Vol(x)
    def `var`: Var = Var(x)
    def rate: Rate = Rate(x)
    def yf: YearFraction = YearFraction(x)
