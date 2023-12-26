package derifree

import cats.Monoid
import cats.syntax.all.*

opaque type Counter[K, V] = Map[K, V]

object Counter:

  def apply[K, V](kvs: (K, V)*): Counter[K, V] = Map(kvs*)

  def empty[K, V]: Counter[K, V] = Map.empty[K, V]

  extension [K, V](counter: Counter[K, V]) def toMap: Map[K, V] = counter

  given [K, V: Monoid]: Monoid[Counter[K, V]] = new Monoid[Counter[K, V]]:
    def empty: Counter[K, V] = Map.empty[K, V]
    def combine(x: Counter[K, V], y: Counter[K, V]): Counter[K, V] =
      (x.toList |+| y.toList).groupMapReduce(_(0))(_(1))(_ |+| _)
