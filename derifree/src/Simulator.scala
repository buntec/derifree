package derifree

import derifree.Simulation.Realization
import derifree.Simulation.Spec

trait Simulator[T]:

  def apply(spec: Simulation.Spec[T]): Either[String, Seq[Simulation.Realization[T]]]

object Simulator:

  def blackScholes[T: TimeOrder](s: Double, sigma: Vol, r: Double): Simulator[T] = new Simulator[T]:
    def apply(spec: Spec[T]): Either[String, Seq[Realization[T]]] = ???
