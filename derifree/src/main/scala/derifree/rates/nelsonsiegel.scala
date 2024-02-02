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
      lambdas: List[Double] = defaultLambdaGrid,
      ridgePenalty: Double = 0
  ): Either[Error, Params] =
    Either.raiseUnless(lambdas.nonEmpty)(Error.BadInputs("lambdas must not be empty")) *>
      lambdas
        .traverse: lambda =>
          val ls = math.LeastSquares.apply
          val a = spotRates.map: (t, _) =>
            List(1.0, f1(t, lambda), f2(t, lambda))
          val y = spotRates.map(_(1).toDouble)
          (if ridgePenalty > 0 then ls.ridge(a, y, ridgePenalty) else ls.ols(a, y)).tupleLeft(
            lambda
          )
        .map: results =>
          val (lambda, result) = results.minBy(_(1).normOfResiduals)
          val coeffs = result.coefficients
          Params(coeffs(0), coeffs(1), coeffs(2), lambda)
