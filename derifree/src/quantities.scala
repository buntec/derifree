package derifree

/** Present value */
opaque type PV = Double

object PV {

  def apply(pv: Double): PV = pv

  extension (pv: PV)
    def *(a: Double): PV = a * pv
    def +(rhs: PV): PV = pv + rhs

}

opaque type YearFraction = Double

private val doubleOrdering: Ordering[Double] = summon[Ordering[Double]]

object YearFraction:

  def apply(t: Double): YearFraction = t

  val oneSecond = YearFraction(1.0 / 365 / 24 / 60 / 60)
  val oneMinute = YearFraction(1.0 / 365 / 24 / 60)
  val oneHour = YearFraction(1.0 / 365 / 24)
  val oneDay: YearFraction = 1.0 / 365
  val oneYear: YearFraction = 1.0

  given Ordering[YearFraction] = doubleOrdering

  extension (t: YearFraction)
    def toDouble: Double = t
    def -(rhs: YearFraction): YearFraction = t - rhs
    def <(rhs: YearFraction): Boolean = t < rhs
    def add(rhs: YearFraction): YearFraction = t + rhs
    def +(rhs: YearFraction): YearFraction = t + rhs
    def *(a: Double): YearFraction = t * a
    def *(n: Int): YearFraction = t * n
    def *(n: Long): YearFraction = t * n
    def ratio(rhs: YearFraction): Double = t / rhs
    def /(n: Int): YearFraction = t / n

opaque type Tick = Long

private val longOrdering: Ordering[Long] = summon[Ordering[Long]]

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
    def +(rhs: Var): Var = v + rhs
    def *(rhs: YearFraction): Double = v * rhs

/** Annualized volatility */
opaque type Vol = Double

object Vol:

  def apply(v: Double): Vol = v

  extension (v: Vol)
    def +(rhs: Vol): Vol = v + rhs
    def *(rhs: Vol): Var = v * rhs
