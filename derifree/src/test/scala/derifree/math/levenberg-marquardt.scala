package derifree.math

import cats.syntax.all.*
import derifree.Gen

import scala.math.sqrt

class LevenbergMarquardtSuite extends munit.FunSuite:

  private case class Case(
      n: Int,
      m: Int,
      xs: IndexedSeq[IndexedSeq[Double]],
      beta: IndexedSeq[Double]
  )

  def model(x: IndexedSeq[IndexedSeq[Double]], beta: IndexedSeq[Double]): IndexedSeq[Double] =
    x.map(_.zip(beta).map((x_i, beta_i) => (x_i - beta_i) * (x_i - beta_i)).sum)

  private val genCase: Gen[Case] = for
    n <- Gen.between(2, 10)
    m <- Gen.between(n, 10 * n)
    xs <- Gen.normal.replicateA(n).replicateA(m)
    beta <- Gen.normal.replicateA(n)
  yield Case(n, m, xs.map(_.toIndexedSeq).toIndexedSeq, beta.toIndexedSeq)

  // TODO: understand better when and why this fails
  test("should recover beta to within tolerance"):
    genCase.view.zipWithIndex
      .take(100)
      .foreach: (c, i) =>

        def f(beta: IndexedSeq[Double]): IndexedSeq[Double] =
          model(c.xs, beta)

        val lbs = IndexedSeq.fill(c.n)(-10.0)
        val ubs = IndexedSeq.fill(c.n)(10.0)
        val guess = IndexedSeq.fill(c.n)(0.0)
        val weights = IndexedSeq.fill(c.m)(1.0)

        val ys = model(c.xs, c.beta)

        val result =
          LevenbergMarquardt.optimize(
            f,
            c.n,
            c.m,
            ys,
            lbs,
            ubs,
            guess,
            weights,
            0.0001,
            0.0001
          )

        val residuals = result.residuals
        val rmse = sqrt(residuals.map(r => r * r).sum / residuals.length)
        val clue =
          s"i=$i, m=${c.m}, n=${c.n}, rmse=$rmse, beta=${c.beta}, beta_hat=${result.optimum}, iters=${result.iters}"
        assert(rmse < 0.001, clue)
