package derifree

opaque type YearFraction = Double

object YearFraction:

  def apply(t: Double): YearFraction = t

  val oneDay: YearFraction = 1.0 / 365

  extension (t: YearFraction)

    def -(rhs: YearFraction): YearFraction = t - rhs

    def <(rhs: YearFraction): Boolean = t < rhs

    def add(rhs: YearFraction): YearFraction = t + rhs

    def +(rhs: YearFraction): YearFraction = t + rhs
