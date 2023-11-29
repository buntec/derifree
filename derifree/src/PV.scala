package derifree

/** Present Value */
opaque type PV = Double

object PV {

  def apply(pv: Double): PV = pv

  extension (pv: PV)

    def scaleBy(a: Double): PV = a * pv
    def *(a: Double): PV = a * pv

    def add(other: PV): PV = pv + other
    def +(other: PV): PV = pv + other

}
