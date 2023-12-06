package derifree

import cats.data.{State => cState}

import scala.collection.immutable.ArraySeq

trait NormalGen:

  type State

  def init: State

  def dimensions: (Int, Int)

  /** Nested `IndexedSeq` dimensions equal `dimensions`. */
  def next: cState[State, IndexedSeq[IndexedSeq[Double]]]

object NormalGen:

  def fromSobol(m: Int, n: Int, dirs: Sobol.DirectionNumbers): Either[Sobol.Error, NormalGen] =
    Sobol(m * n, dirs).map: sobol =>
      val bbt = BrownianBridge.transform(m * n)
      new NormalGen:
        type State = Sobol.State

        def dimensions: (Int, Int) = (m, n)

        def init: State = sobol.initialState

        def next: cState[State, IndexedSeq[IndexedSeq[Double]]] =
          sobol.next.map: sobolNumbers =>
            val zs0 = sobolNumbers.toArray
            var i = 0
            while (i < m * n) {
              zs0(i) = normal.inverseCdf(zs0(i))
              i += 1
            }
            bbt(zs0)
            val zs = Array.ofDim[Double](m, n)

            i = 0
            while (i < m) {
              var j = 0
              while (j < n) {
                zs(i)(j) = zs0(i * n + j)
                j += 1
              }
              i += 1
            }

            ArraySeq.unsafeWrapArray(zs.map(ArraySeq.unsafeWrapArray))
