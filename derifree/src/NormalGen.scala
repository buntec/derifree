package derifree

import cats.data.{State => cState}

trait NormalGen:

  type State

  def init: State

  def next: cState[State, IndexedSeq[IndexedSeq[Double]]]

object NormalGen:

  def fromSobol(sobol: Sobol): NormalGen = new NormalGen:
    type State = Sobol.State
    def init: State = sobol.initialState
    def next: cState[State, IndexedSeq[IndexedSeq[Double]]] =
      sobol.next.map(sobolNumbers => IndexedSeq(IndexedSeq(1.0)))
