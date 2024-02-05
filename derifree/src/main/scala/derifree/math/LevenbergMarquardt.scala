package derifree.math

import org.apache.commons.math3.analysis._
import org.apache.commons.math3.linear._
import org.apache.commons.math3.fitting.leastsquares._
import org.apache.commons.math3.optim._
import org.apache.commons.math3.util.FastMath._

object LevenbergMarquardt {

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
      absTol: Double
  ): Result = {

    require(target.length == outputDim)
    require(lowerBounds.length == inputDim)
    require(upperBounds.length == inputDim)
    require(guess.length == inputDim)
    require(weights.length == outputDim)

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
        LeastSquaresFactory.create(model, observed, start, checker, 100, 100),
        new ArrayRealVector(weights.toArray)
      )

    val optimizer = new LevenbergMarquardtOptimizer()
    val optimum = optimizer.optimize(problem)

    Result(
      optimum.getPoint.toArray.toIndexedSeq.zip(boxContraints).map((x, bc) => bc.toBounded(x)),
      optimum.getEvaluations,
      optimum.getIterations,
      optimum.getResiduals.toArray.toIndexedSeq
    )

  }

}
