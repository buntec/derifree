package derifree

import cats.syntax.all.*
import cats.Monoid

object Simulation:

  case class Spec[T](spotObs: Map[String, Set[T]], discountObs: Set[T])

  object Spec:
    def spotObs[T](ticker: String, times: Set[T]) =
      Spec[T](Map(ticker -> times), Set.empty)

    def discountObs[T](time: T) = Spec[T](Map.empty, Set(time))

    given monoid[T]: Monoid[Spec[T]] = new Monoid[Spec[T]]:
      def empty: Spec[T] = Spec(Map.empty, Set.empty)
      def combine(x: Spec[T], y: Spec[T]): Spec[T] =
        Spec(x.spotObs <+> y.spotObs, x.discountObs <+> y.discountObs)

  /** Relization of a piecewise diffusion. */
  case class Realization[T](
      timeIndex: Map[T, Int],
      deltaTs: IndexedSeq[YearFraction], // t_{i+1} - t_i
      spots: Map[String, IndexedSeq[Double]],
      jumps: Map[String, IndexedSeq[Double]], // Jump(t_i) = S(t_i) - S(t_i-)
      vols: Map[String, IndexedSeq[Vol]],
      discounts: IArray[Double] // discount factors
  )