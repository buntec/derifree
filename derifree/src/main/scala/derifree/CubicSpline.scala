package derifree

import scala.collection.immutable.ArraySeq

trait CubicSpline:

  def apply(x: Double): Double

  def fstDerivative(x: Double): Double

  def sndDerivative(x: Double): Double

object CubicSpline:

  def natural(xs: IndexedSeq[Double], ys: IndexedSeq[Double]): CubicSpline =

    require(xs.zip(xs.tail).forall(_ < _), "x values must be stricly increasing")
    require(xs.length == ys.length, "xs and ys must have same length")

    val m = xs.length
    val n = m - 1

    val (a0, b0, c0, d0) = solveNaturalCubicSpline(xs, ys)

    // append extrapolation coeffs to both ends
    val a = ys(0) +: a0 :+ ys(n)
    val b = b0(0) +: b0 :+ b0(n - 1) + c0(n - 1) * (xs(n) - xs(n - 1))
    val c = 0.0 +: c0 :+ 0.0
    val d = 0.0 +: d0 :+ 0.0

    new CubicSpline:

      def apply(x: Double): Double =
        val i = xs.search(x).insertionPoint
        val dx = x - xs(math.max(0, i - 1))
        ((d(i) * dx + c(i)) * dx + b(i)) * dx + a(i)

      def fstDerivative(x: Double): Double =
        val i = xs.search(x).insertionPoint
        val dx = x - xs(math.max(0, i - 1))
        (3 * d(i) * dx + 2 * c(i)) * dx + b(i)

      def sndDerivative(x: Double): Double =
        val i = xs.search(x).insertionPoint
        val dx = x - xs(math.max(0, i - 1))
        6 * d(i) * dx + 2 * c(i)

  private def SolveTridiagSystem(
      a: ArraySeq[Double], // lower diag
      b: ArraySeq[Double], // diag
      c: ArraySeq[Double], // upper diag
      d: ArraySeq[Double] // RHS
  ): ArraySeq[Double] =

    val n = b.length

    require(a.length == n - 1, "unexpected length of lower diagonal")
    require(b.length == n, "unexpected length of diagonal")
    require(c.length == n - 1, "unexpected length of upper diagonal")
    require(d.length == n, "unexpected length of RHS")

    val x = Array.ofDim[Double](n)
    val b0 = b.toArray[Double]
    val d0 = d.toArray[Double]

    var i = 1
    while (i < n) {
      val w = a(i - 1) / b0(i - 1)
      b0(i) = b0(i) - w * c(i - 1)
      d0(i) = d0(i) - w * d0(i - 1)
      i += 1
    }

    x(n - 1) = d0(n - 1) / b0(n - 1)

    i = n - 2
    while (i > -1) {
      x(i) = (d0(i) - c(i) * x(i + 1)) / b0(i)
      i -= 1
    }

    ArraySeq.unsafeWrapArray(x)

  private def solveNaturalCubicSpline(
      x: IndexedSeq[Double], // knots
      y: IndexedSeq[Double] // function values
  ) =

    val m = x.length
    require(m > 2, "cannot fit fewer than 3 knots")
    require(y.length == m, "x and y don't have same length")

    val n = m - 1 // there are n + 1 points

    val a = y // obviously a_i = y_i
    val b = Array.ofDim[Double](n)
    val c = Array.ofDim[Double](n + 1) // we need c(n) for intermediate calculations
    val d = Array.ofDim[Double](n)

    // we have to solve a tridiagonal system of size n - 1

    // build the RHS of the system
    val h = (0 until n - 1).map(i =>
      3 * ((y(i + 2) - y(i + 1)) / (x(i + 2) - x(i + 1)) - (y(i + 1) - y(i)) / (x(i + 1) - x(
        i
      )))
    )

    val diag = (0 until n - 1).map(i => 2 * (x(i + 2) - x(i)))
    val ldiag = (0 until n - 2).map(i => x(i + 2) - x(i + 1))
    val udiag = (0 until n - 2).map(i => x(i + 2) - x(i + 1))

    // solve for [c_1, ..., c_{n-1}]
    val c_interior = SolveTridiagSystem(
      ArraySeq.unsafeWrapArray(ldiag.toArray),
      ArraySeq.unsafeWrapArray(diag.toArray),
      ArraySeq.unsafeWrapArray(udiag.toArray),
      ArraySeq.unsafeWrapArray(h.toArray)
    )

    // c[0] = 0 and c[n] = 0 is implied by the boundary conditions
    for (i <- 0 until n - 1) {
      c(i + 1) = c_interior(i)
    }

    for (i <- 0 until n) {
      b(i) =
        (y(i + 1) - y(i)) / (x(i + 1) - x(i)) - (2 * c(i) + c(i + 1)) * (x(i + 1) - x(i)) / 3
    }

    for (i <- 0 until n) {
      d(i) = (c(i + 1) - c(i)) / (x(i + 1) - x(i)) / 3
    }

    (a, b, c.slice(0, c.length - 1), d)
