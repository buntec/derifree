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

import derifree.fd.*

case class EuropeanVanilla[T](
    strike: Double,
    expiry: T,
    optionType: EuropeanVanilla.OptionType
)

object EuropeanVanilla:

  enum OptionType:
    case Call, Put

  given priceable[T: TimeLike]: FD[EuropeanVanilla[T], T] =
    new FD[EuropeanVanilla[T], T]:
      def lowerBoundary(a: EuropeanVanilla[T]): BoundaryCondition = BoundaryCondition.Linear

      def upperBoundary(a: EuropeanVanilla[T]): BoundaryCondition = BoundaryCondition.Linear

      def terminalPayoff(a: EuropeanVanilla[T]): (T, Double => Double) =
        val omega = a.optionType match
          case OptionType.Call => 1
          case OptionType.Put  => -1
        (a.expiry, s => math.max(omega * (s - a.strike), 0.0))

      def valueTransforms(a: EuropeanVanilla[T]): List[(T, (Double, Double) => Double)] = Nil

      def americanExerciseValue(a: EuropeanVanilla[T]) = None
