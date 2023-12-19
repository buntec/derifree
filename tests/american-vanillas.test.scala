package derifree

import cats.syntax.all.*
import derifree.syntax.*

class AmericanVanillaSuite extends munit.FunSuite:

  val dsl = Dsl[YearFraction]
  import dsl.*

  val refTime = YearFraction.zero

  val spots = Map("ACME" -> 100.0)
  val vols = Map("ACME" -> 0.2.vol)
  val rate = 0.08.rate

  val dirNums = Sobol.directionNumbers(500).toTry.get

  val sim: Simulator[YearFraction] =
    Simulator.blackScholes(
      TimeGrid.Factory.almostEquidistant(YearFraction.oneDay),
      NormalGen.Factory.sobol(dirNums),
      refTime,
      spots,
      vols,
      Map.empty,
      rate
    )

  test("American put should match tabulated values"):
    val udl = "ACME"
    val expiry = YearFraction.oneYear / 4
    val strike = 100.0

    val nSims = (1 << 16) - 1

    val europeanPut = for
      s <- spot(udl, expiry)
      _ <- cashflow(max(strike - s, 0), expiry)
    yield ()

    val americanPut =
      val m = 30
      for
        s <- spot(udl, expiry)
        _ <- List
          .tabulate(m)(i => (expiry / m) * i)
          .drop(1)
          .traverse(t =>
            spot(udl, t).flatMap(s_t => puttable(max(0, strike - s_t).some.filter(_ > 0), t))
          )
        _ <- cashflow(max(strike - s, 0), expiry)
      yield ()

    val euPrice = europeanPut.fairValue(sim, nSims).toTry.get
    val amPrice = americanPut.fairValue(sim, nSims).toTry.get
    // val exercisePs = americanPut.earlyTerminationProbabilities(sim, nSims).toTry.get

    println(
      s"expiry=$expiry, strike=$strike, euPrice=$euPrice, amPrice=$amPrice"
    )
