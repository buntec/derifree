package example2

import derifree.*
import derifree.syntax.*

val dsl = Dsl[YearFraction]

import dsl.*

val refTime = YearFraction.zero
val expiry = YearFraction.oneYear
val settle = expiry + YearFraction.oneDay * 2

val worstOfDip: RV[PV] = for
  s1_0 <- spot("AAPL", refTime)
  s2_0 <- spot("META", refTime)
  s1 <- spot("AAPL", expiry)
  s2 <- spot("META", expiry)
  pHit <- hitProb(
    Barrier.Continuous(
      Barrier.Direction.Down,
      Map("AAPL" -> 0.8 * s1_0, "META" -> 0.8 * s2_0),
      from = refTime,
      to = expiry,
      Barrier.Policy.Or
    )
  )
  payout <- cashflow(max(0, 1 - min(s1 / s1_0, s2 / s2_0)), settle)
yield payout * pHit

import cats.effect.*

object Main extends IOApp.Simple:

  def run: IO[Unit] = Sobol
    .directionNumbersFromResource[IO](5000)
    .flatMap: dirNums =>

      val spots = Map("AAPL" -> 195.0, "META" -> 330.0)
      val vols = Map("AAPL" -> 0.25.vol, "META" -> 0.3.vol)
      val correlations = Map(("AAPL", "META") -> 0.7)
      val rate = 0.04.rate

      val nSims = (1 << 16) - 1

      val sim: Simulator[YearFraction] =
        Simulator.blackScholes[YearFraction](
          TimeGrid.Factory.almostEquidistant(YearFraction.oneDay),
          NormalGen.Factory.sobol(dirNums),
          nSims,
          refTime,
          spots,
          vols,
          correlations,
          rate
        )

      val priceAndPrint = IO.delay:
        val t1 = System.nanoTime()
        val price = worstOfDip.mean(sim)
        val t2 = System.nanoTime()
        println(f"price = $price, duration = ${(t2 - t1) * 1e-6}%.0f ms")

      priceAndPrint.replicateA_(100)