package app

import cats.effect.*
import cats.syntax.all.*
import derifree.*
import derifree.prettyprint.given

object Main extends IOApp.Simple:

  val printNormals: IO[Unit] = Sobol
    .directionNumbersFromResource[IO](100)
    .flatMap: dirNums =>
      IO.fromEither(NormalGen.fromSobol(10, 3, dirNums))
    .flatMap: normalGen =>
      val seq = normalGen.next.replicateA(10).runA(normalGen.init).value
      IO.println(seq.map(_.show).mkString("\n\n"))

  val payoffExample: IO[Unit] =
    Sobol
      .directionNumbersFromResource[IO](1000)
      .flatMap: dirNums =>

        val spots = Map("AAPL" -> 250.0, "META" -> 100.0)
        val vols = Map("AAPL" -> Vol(0.3), "META" -> Vol(0.4))
        val correlations = Map(("AAPL", "META") -> 0.5)
        val rate = Rate(0.05)
        val refTime = YearFraction.zero

        val nSims = 65535 // 32767

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

        val dsl = Dsl[YearFraction]

        import dsl.*
        import math.{min, max}

        val strike = 1.0
        val expiry = YearFraction(1.0)
        val settlement = expiry + YearFraction.oneDay * 2

        val worstOfDip: RV[PV] = for {
          s1_0 <- spot("AAPL", refTime)
          s2_0 <- spot("META", refTime)
          s1 <- spot("AAPL", expiry)
          s2 <- spot("META", expiry)
          pHit <- hitProb(
            Barrier.Continuous(
              Barrier.Direction.Down,
              Map("AAPL" -> 200.0, "META" -> 80.0),
              refTime,
              expiry,
              Barrier.Policy.Or
            )
          )
          payout <- cashflow(max(0.0, strike - min(s1 / s1_0, s2 / s2_0)), settlement)
        } yield payout * pHit

        val priceAndPrint = IO.delay:
          val t1 = System.nanoTime()
          val price = worstOfDip.mean(sim)
          val t2 = System.nanoTime()
          println(f"price = $price, duration = ${(t2 - t1) * 1e-6}%.0f ms")

        priceAndPrint.replicateA_(100)

  def run: IO[Unit] = payoffExample
