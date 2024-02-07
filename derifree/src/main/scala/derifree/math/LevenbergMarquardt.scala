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
package math

import cats.syntax.all.*
import org.apache.commons.math3.analysis._
import org.apache.commons.math3.fitting.leastsquares._
import org.apache.commons.math3.linear._
import org.apache.commons.math3.optim._
import org.apache.commons.math3.util.FastMath._

object LevenbergMarquardt:

  final case class Result(
      optimum: IndexedSeq[Double],
      evals: Int,
      iters: Int,
      residuals: IndexedSeq[Double]
  )

  def optimize(
      fun: IndexedSeq[Double] => IndexedSeq[Double],
      inputDim: Int,
      outputDim: Int,
      target: IndexedSeq[Double],
      lowerBounds: IndexedSeq[Double],
      upperBounds: IndexedSeq[Double],
      guess: IndexedSeq[Double],
      weights: IndexedSeq[Double],
      absBumpSize: Double,
      absTol: Double,
      maxIters: Int = 100,
      maxEvals: Int = 100
  ): Either[Error, Result] =
    (
      validateInput(target.length == outputDim, "dimension mismatch"),
      validateInput(
        lowerBounds.length == inputDim,
        s"lower bounds don't match input dimensions ($inputDim): $lowerBounds"
      ),
      validateInput(
        upperBounds.length == inputDim,
        s"upper bounds don't match input dimensions ($inputDim): $upperBounds"
      ),
      validateInput(guess.length == inputDim, "dimension mismatch"),
      validateInput(weights.length == outputDim, "dimension mismatch")
    ).tupled.void.flatMap: _ =>

      val boxContraints =
        (lowerBounds zip upperBounds zip guess).map:
          case ((lb, ub), x0) =>
            BoxConstraints(lb, ub, x0)

      val valueFun = new MultivariateVectorFunction:
        def value(point: Array[Double]): Array[Double] =
          fun(point.toIndexedSeq.zip(boxContraints).map((x, bc) => bc.toBounded(x))).toArray

      val jacobianFun = new MultivariateMatrixFunction {
        def value(point: Array[Double]): Array[Array[Double]] = {
          val m = Array.ofDim[Double](outputDim, inputDim)
          var i = 0
          val y0 = valueFun.value(point)
          while (i < inputDim) {
            val xi = point(i)
            point(i) += absBumpSize
            val y = valueFun.value(point)
            var j = 0
            while (j < outputDim) {
              m(j)(i) = (y(j) - y0(j)) / absBumpSize
              j += 1
            }
            point(i) = xi
            i += 1
          }
          m
        }
      }

      val model = LeastSquaresFactory.model(valueFun, jacobianFun)
      val observed = new ArrayRealVector(target.toArray)

      // by construction of the box constraints, `toBounded` maps (0,...,0) to `guess`,
      // so we simply start from the all-zero vector.
      val start = new ArrayRealVector(inputDim)

      val checker = new ConvergenceChecker[LeastSquaresProblem.Evaluation] {
        def converged(
            iteration: Int,
            previous: LeastSquaresProblem.Evaluation,
            current: LeastSquaresProblem.Evaluation
        ): Boolean = {
          current.getRMS() < absTol
        }
      }

      val problem =
        LeastSquaresFactory.weightDiagonal(
          LeastSquaresFactory.create(model, observed, start, checker, maxEvals, maxIters),
          new ArrayRealVector(weights.toArray)
        )

      val optimizer = new LevenbergMarquardtOptimizer()

      Either
        .catchNonFatal(optimizer.optimize(problem))
        .map(optimum =>
          Result(
            optimum.getPoint.toArray.toIndexedSeq
              .zip(boxContraints)
              .map((x, bc) => bc.toBounded(x)),
            optimum.getEvaluations,
            optimum.getIterations,
            optimum.getResiduals.toArray.toIndexedSeq
          )
        )
        .leftMap(t => Error.BadNumerics(t.getMessage))

  private case class BoxConstraints(lowerBound: Double, upperBound: Double, center: Double):

    private val kappa = atanh(
      2.0 * (center - lowerBound) / (upperBound - lowerBound) - 1.0
    )
    private val coshKappa = cosh(kappa)
    private val alpha = 2.0 / (upperBound - lowerBound) * coshKappa * coshKappa
    private val c = -kappa / alpha

    def toBounded(x: Double): Double =
      0.5 * (upperBound - lowerBound) * (tanh(
        alpha * (x - c)
      ) + 1.0) + lowerBound

    def toUnbounded(y: Double): Double =
      atanh(
        2.0 * (y - lowerBound) / (upperBound - lowerBound) - 1.0
      ) / alpha + c
