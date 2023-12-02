package derifree

import derifree.Simulation.Realization
import derifree.Simulation.Spec
import cats.syntax.all.*
import cats.kernel.Order

trait Simulator[T]:

  def apply(spec: Simulation.Spec[T]): Either[String, Seq[Simulation.Realization[T]]]

object Simulator:

  def blackScholes[T: TimeLike](ref: T, s: Double, sigma: Vol, r: Double): Simulator[T] = new Simulator[T]:
    def apply(spec: Spec[T]): Either[String, Seq[Realization[T]]] =
      val obsTimes = (spec.spotObs.values.reduce(_ union _) union spec.discountObs).toList.sorted(Order[T].toOrdering)
      val yearFractions = obsTimes.map(t => TimeLike[T].yearFractionBetween(ref, t))
      val timeGrid = TimeGrid.equidistant(100, yearFractions.max, yearFractions.toSet)
      val timeIndexMaybe =
        (obsTimes zip yearFractions).traverse((t, yf) => timeGrid.indexOf(yf).tupleLeft(t)).map(_.toMap)
      Either
        .fromOption(timeIndexMaybe, "failed to compute time index")
        .map(timeIndex =>
          LazyList.unfold(0): state =>
            if state < 10000 then
              val spots = ???
              val jumps = ???
              val vols = ???
              val discounts = ???
              Some(Simulation.Realization[T](timeIndex, timeGrid.deltas, spots, jumps, vols, discounts), state + 1)
            else None
        )
