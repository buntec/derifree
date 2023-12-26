/*
 * Copyright 2023 buntec
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

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
