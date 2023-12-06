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
    Sobol(m * n, dirs).map(sobol =>
      new NormalGen:
        type State = Sobol.State
        def dimensions: (Int, Int) = (m, n)
        def init: State = sobol.initialState
        def next: cState[State, IndexedSeq[IndexedSeq[Double]]] =
          sobol.next.map { sobolNumbers =>
            val zs = Array.ofDim[Double](m, n)
            var i = 0
            while (i < m) {
              var j = 0
              while (j < n) {
                zs(i)(j) = normal.inverseCdf(sobolNumbers(i * n + j))
                j += 1
              }
              i += 1
            }
            ArraySeq.unsafeWrapArray(zs.map(ArraySeq.unsafeWrapArray))
          }
    )
