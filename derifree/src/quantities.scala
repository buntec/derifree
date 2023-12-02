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

object YearFraction:

  def apply(t: Double): YearFraction = t

  val oneDay: YearFraction = 1.0 / 365

  extension (t: YearFraction)
    def -(rhs: YearFraction): YearFraction = t - rhs
    def <(rhs: YearFraction): Boolean = t < rhs
    def add(rhs: YearFraction): YearFraction = t + rhs
    def +(rhs: YearFraction): YearFraction = t + rhs
    def *(v: Var): Double = t * v

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
