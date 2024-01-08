package derifree

import cats.syntax.all.*

class NaturalCubicSplineSuite extends munit.FunSuite:

  private val N = 1000

  private case class Case(xs: List[Double], ys: List[Double])

  private val genCase: Gen[Case] =
    for
      n <- Gen.between(3, 100)
      x0 <- Gen.normal
      xs <- Gen.normal.replicateA(n).map(_.map(z => math.abs(z) + 0.01).scanLeft(x0)(_ + _))
      y0 <- Gen.normal
      ys <- Gen.normal.replicateA(n).map(_.scan(y0)(_ + _))
    yield Case(xs, ys)

  test("should match function values at knots"):
    genCase.view
      .take(N)
      .zipWithIndex
      .foreach: (c, i) =>
        val clue = s"i=$i"
        val spline = CubicSpline.natural(c.xs.toIndexedSeq, c.ys.toIndexedSeq)
        (c.xs zip c.ys).foreach((x, y) => assertEqualsDouble(y, spline(x), 1e-9, clue))

  test("should be continuous in value, first and second derivative at knots"):
    genCase.view
      .take(N)
      .zipWithIndex
      .foreach: (c, i) =>
        val spline = CubicSpline.natural(c.xs.toIndexedSeq, c.ys.toIndexedSeq)
        val xRange = c.xs.max - c.xs.min
        val yRange = c.ys.max - c.ys.min
        val dRange = c.xs.map(spline.fstDerivative).max - c.xs.map(spline.fstDerivative).min
        val d2Range = c.xs.map(spline.sndDerivative).max - c.xs.map(spline.sndDerivative).min
        // println(s"xRange=$xRange, yRange=$yRange, dRange=$dRange, d2Range=$d2Range")

        val eps0 = 1e-4 * xRange
        val epsMin = 1e-11 * xRange
        (c.xs zip c.ys).zipWithIndex.foreach:
          case ((x, y), j) =>
            val passValue = Iterator
              .unfold(eps0): eps =>
                val yLeft = spline(x - eps)
                val yRight = spline(x + eps)
                val delta = math.max(math.abs(yLeft - y), math.abs(yRight - y))
                if eps < epsMin then None else Some(delta, eps / 2)
              .exists(_ < 1e-5 * yRange)

            val passFstDeriv = Iterator
              .unfold(eps0): eps =>
                val d = spline.fstDerivative(x)
                val dLeft = spline.fstDerivative(x - eps)
                val dRight = spline.fstDerivative(x + eps)
                val delta = math.max(math.abs(dLeft - d), math.abs(dRight - d))
                if eps < epsMin then None else Some(delta, eps / 2)
              .exists(_ < 1e-5 * dRange)

            val passSndDeriv = Iterator
              .unfold(eps0): eps =>
                val d2 = spline.sndDerivative(x)
                val d2Left = spline.sndDerivative(x - eps)
                val d2Right = spline.sndDerivative(x + eps)
                val delta = math.max(math.abs(d2Left - d2), math.abs(d2Right - d2))
                if eps < epsMin then None else Some(delta, eps / 2)
              .exists(_ < 1e-5 * d2Range)

            assert(passValue, s"i=$i, value")
            assert(passFstDeriv, s"i=$i, 1st deriv, dRange=$dRange")
            assert(passSndDeriv, s"i=$i, 2nd deriv, d2Range=$dRange")

  test("should be linear in extrapolation region"):
    genCase.view
      .take(N)
      .zipWithIndex
      .foreach: (c, i) =>
        val clue = s"i=$i"
        val spline = CubicSpline.natural(c.xs.toIndexedSeq, c.ys.toIndexedSeq)
        val xMin = c.xs.min
        val xMax = c.xs.max
        val xRange = xMax - xMin
        val step = 0.1 * xRange
        val x1 = xMin - step
        val x2 = xMin - 2 * step
        val x3 = xMax + step
        val x4 = xMax + 2 * step
        val d1 = spline.fstDerivative(x1)
        val d2 = spline.fstDerivative(x2)
        assertEquals(d1, d2, clue)
        assertEquals(spline.sndDerivative(x1), 0.0, clue)
        val d3 = spline.fstDerivative(x3)
        val d4 = spline.fstDerivative(x4)
        assertEquals(d3, d4, clue)
        assertEquals(spline.sndDerivative(x3), 0.0, clue)
