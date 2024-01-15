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

import scala.math.max

case class AmericanVanilla[T](
    underlying: String,
    ccy: Ccy,
    strike: Double,
    expiry: T,
    optionType: AmericanVanilla.OptionType
)

object AmericanVanilla:

  enum OptionType:
    case Call, Put

  given [T: TimeLike]: MC[AmericanVanilla[T], T] =
    new MC[AmericanVanilla[T], T]:
      def contingentClaim(a: AmericanVanilla[T], refTime: T, dsl: Dsl[T]): dsl.ContingentClaim =
        val omega = a.optionType match
          case OptionType.Call => 1
          case OptionType.Put  => -1

        // daily exercise
        val earlyExTimes = TimeLike[T].dailyStepsBetween(refTime, a.expiry)

        import dsl.*
        for
          s <- spot(a.underlying, a.expiry)
          _ <- earlyExTimes
            .traverse(t =>
              dsl
                .spot(a.underlying, t)
                .flatMap(s_t =>
                  puttable(
                    dsl.max(omega * (s_t - a.strike), 0.0).some.filter(_ > 0),
                    Ccy.USD,
                    t
                  )
                )
            )
          _ <- cashflow(dsl.max(omega * (s - a.strike), 0.0), a.ccy, a.expiry)
        yield ()

  given [T: TimeLike]: FD[AmericanVanilla[T], T] =
    new FD[AmericanVanilla[T], T]:
      def lowerBoundary(a: AmericanVanilla[T]): BoundaryCondition = BoundaryCondition.Linear

      def upperBoundary(a: AmericanVanilla[T]): BoundaryCondition = BoundaryCondition.Linear

      def terminalPayoff(a: AmericanVanilla[T]): (T, Double => Double) =
        val omega = a.optionType match
          case OptionType.Call => 1
          case OptionType.Put  => -1
        (a.expiry, s => max(omega * (s - a.strike), 0.0))

      def valueTransforms(a: AmericanVanilla[T]): List[(T, (Double, Double) => Double)] = Nil

      def americanExerciseValue(a: AmericanVanilla[T]): Option[T => Double => Double] =
        val omega = a.optionType match
          case OptionType.Call => 1
          case OptionType.Put  => -1
        Some(t => s => max(omega * (s - a.strike), 0.0))