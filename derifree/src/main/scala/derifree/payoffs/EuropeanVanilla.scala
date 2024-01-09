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
