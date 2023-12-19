package derifree

import cats.syntax.all.*
import derifree.syntax.*

class AmericanVanillaSuite extends munit.FunSuite:

  val dsl = Dsl[YearFraction]
  import dsl.*

  val refTime = YearFraction.zero

  val dirNums = Sobol.directionNumbers(500).toTry.get

  test("American put should match tabulated values"):
    val udl = "ACME"

    // values taken from Barone, Adesi, Whaley 1987
    (
      List(80.0, 90.0, 100.0, 110.0, 120.0),
      List(18.09, 9.05, 3.04, 0.64, 0.09), // European prices
      List(20.0, 10.4, 3.22, 0.66, 0.09) // American prices (finite-difference)
    ).parMapN { (s0, euPriceRef, amPriceRef) =>

      val spots = Map(udl -> s0)
      val vols = Map(udl -> 0.2.vol)
      val rate = 0.08.rate

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

      val expiry = YearFraction.oneYear / 4
      val strike = 100.0

      val nSims = (1 << 15) - 1

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

      val hint =
        s"euPrice=$euPrice, euPriceRef=$euPriceRef, amPrice=$amPrice, amPriceRef=$amPriceRef"

      println(hint)

      assert(
        math.abs(euPrice.toDouble - euPriceRef) < 0.01 || math.abs(
          euPrice.toDouble - euPriceRef
        ) / euPriceRef < 0.01
      )
      assert(
        math.abs(amPrice.toDouble - amPriceRef) < 0.01 || math.abs(
          amPrice.toDouble - amPriceRef
        ) / amPriceRef < 0.01
      )
    }
