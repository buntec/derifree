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
        // we cannot simply do <+> on spotObs b/c the
        // default monoid instance for Map will not combine values
        val spotObs = (x.spotObs.toList ++ y.spotObs.toList)
          .groupBy((udl, _) => udl)
          .map((udl, l) => udl -> l.map(_(1)).combineAll)
        Spec(spotObs, x.discountObs <+> y.discountObs)

  /** Relization of a piecewise diffusion. */
  case class Realization[T](
      timeIndex: Map[T, Int],
      ts: IndexedSeq[YearFraction], // t_i
      deltaTs: IndexedSeq[YearFraction], // t_{i+1} - t_i
      spots: Map[String, IndexedSeq[Double]],
      jumps: Map[String, IndexedSeq[Double]], // Jump(t_i) = S(t_i) - S(t_i-)
      vols: Map[String, IndexedSeq[Vol]],
      discounts: IndexedSeq[Double] // discount factors
  )
