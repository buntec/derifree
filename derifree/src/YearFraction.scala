package derifree

opaque type YearFraction = Double

object YearFraction:

  def apply(t: Double): YearFraction = t

  extension (yf: YearFraction)

    def -(rhs: YearFraction): YearFraction = yf - rhs
    def <(rhs: YearFraction): Boolean = yf < rhs
