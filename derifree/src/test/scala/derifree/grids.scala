package derifree

import cats.syntax.all.*
import derifree.fd.SpatialGrid

import scala.math.exp
import scala.math.max
import scala.math.min

class GridsSuite extends munit.FunSuite:

  test("antipoint fitter"):

    case class Case(
        lb: Double,
        ub: Double,
        antipoint: Double
    )

    val gen: Gen[Case] = for
      z1 <- Gen.normal
      z2 <- Gen.normal
      u <- Gen.between(0.1, 0.9)
    yield
      val lb = exp(min(z1, z2))
      val ub = exp(max(z1, z2))
      val antipoint = exp(z1 + u * (z2 - z1))
      Case(lb, ub, antipoint)

    gen.view
      .take(10000)
      .foreach: c =>
        val antipoint = c.antipoint
        val factory = SpatialGrid.Factory.logSinh(100, 0.1, antipoint)
        val grid = factory(c.lb, c.ub)
        val i = grid.search(antipoint).insertionPoint
        val x1 = grid(i - 1)
        val x2 = grid(i)
        val midPoint = 0.5 * (x1 + x2)
        assertEqualsDouble(midPoint, antipoint, antipoint * 1e-6)
