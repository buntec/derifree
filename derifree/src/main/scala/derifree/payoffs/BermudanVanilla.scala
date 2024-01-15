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
package payoffs

import cats.syntax.all.*
import derifree.fd.*

import scala.{math => smath}

case class BermudanVanilla[T](
    underlying: String,
    ccy: Ccy,
    strike: Double,
    expiry: T,
    exerciseTimes: List[T],
    optionType: BermudanVanilla.OptionType
)

object BermudanVanilla:

  enum OptionType:
    case Call, Put

  given [T: TimeLike]: MC[BermudanVanilla[T], T] =
    new MC[BermudanVanilla[T], T]:
      def contingentClaim(a: BermudanVanilla[T], refTime: T, dsl: Dsl[T]): dsl.ContingentClaim =
        val omega = a.optionType match
          case OptionType.Call => 1
          case OptionType.Put  => -1
        import dsl.*
        for
          s <- spot(a.underlying, a.expiry)
          _ <- a.exerciseTimes
            .traverse(t =>
              dsl
                .spot(a.underlying, t)
                .flatMap(s_t =>
                  puttable(max(omega * (s_t - a.strike), 0.0).some.filter(_ > 0), Ccy.USD, t)
                )
            )
          _ <- cashflow(max(omega * (s - a.strike), 0.0), a.ccy, a.expiry)
        yield ()

  given [T: TimeLike]: FD[BermudanVanilla[T], T] =
    new FD[BermudanVanilla[T], T]:
      def lowerBoundary(a: BermudanVanilla[T]): BoundaryCondition = BoundaryCondition.Linear

      def upperBoundary(a: BermudanVanilla[T]): BoundaryCondition = BoundaryCondition.Linear

      def terminalPayoff(a: BermudanVanilla[T]): (T, Double => Double) =
        val omega = a.optionType match
          case OptionType.Call => 1
          case OptionType.Put  => -1
        (a.expiry, s => smath.max(omega * (s - a.strike), 0.0))

      def valueTransforms(a: BermudanVanilla[T]): List[(T, (Double, Double) => Double)] =
        val omega = a.optionType match
          case OptionType.Call => 1
          case OptionType.Put  => -1
        a.exerciseTimes.map: t =>
          (t, (spot, value) => smath.max(value, smath.max(omega * (spot - a.strike), 0.0)))

      def americanExerciseValue(a: BermudanVanilla[T]): Option[T => Double => Double] = None
