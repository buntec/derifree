package derifree
package rates

import cats.syntax.all.*

import scala.math.exp

object nelsonsiegel:

  case class Params(beta0: Double, beta1: Double, beta2: Double, lambda: Double):
    def spotRate(t: YearFraction): Rate =
      val a = t.toDouble / lambda
      val b = exp(-a)
      val c = (1 - b) / a
      Rate(beta0 + beta1 * c + beta2 * (c - b))

  private def f1(t: YearFraction, lambda: Double): Double =
    val a = t.toDouble / lambda
    val b = exp(-a)
    (1 - b) / a

  private def f2(t: YearFraction, lambda: Double): Double =
    val a = t.toDouble / lambda
    val b = exp(-a)
    (1 - b) / a - b

  private val defaultLambdaGrid = List(0.027, 0.082, 0.16, 0.25, 0.5, 0.75, 1.0, 1.5, 2.0)

  def fitByOlsOnLambdaGrid(
      spotRates: List[(YearFraction, Rate)],
      lambdas: List[Double] = defaultLambdaGrid
  ): Either[Error, Params] =
    Either.raiseUnless(lambdas.nonEmpty)(Error.BadInputs("lambdas must not be empty")) *>
      lambdas
        .traverse: lambda =>
          val ols = math.LeastSquares.apply
          val a = spotRates.map: (t, _) =>
            List(1.0, f1(t, lambda), f2(t, lambda))
          val y = spotRates.map(_(1).toDouble)
          ols.ols(a, y).tupleLeft(lambda)
        .map: results =>
          val (lambda, result) = results.minBy(_(1).normOfResiduals)
          val coeffs = result.coefficients
          Params(coeffs(0), coeffs(1), coeffs(2), lambda)
