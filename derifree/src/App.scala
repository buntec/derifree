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
    Sobol.directionNumbersFromResource[IO](1000).flatMap { dirNums =>

      val spots = Map("AAPL" -> 250.0, "META" -> 100.0)
      val vols = Map("AAPL" -> Vol(0.3), "META" -> Vol(0.4))
      val correlations = Map(("AAPL", "META") -> 0.5)
      val rate = Rate(0.05)
      val refTime = YearFraction.zero

      val sim: Simulator[YearFraction] =
        Simulator
          .blackScholes[YearFraction](10000, dirNums)(refTime, spots, vols, correlations, rate)

      val dsl = Dsl[YearFraction]

      import dsl.*

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
            Barrier.Policy.Or,
            Map("AAPL" -> 200.0, "META" -> 80.0),
            refTime,
            expiry
          )
        )
        payout <- cashflow(math.max(0.0, strike - math.min(s1 / s1_0, s2 / s2_0)), settlement)
      } yield payout * pHit

      val price =
        IO.defer(IO.fromEither(Compiler.run(dsl, sim)(worstOfDip.map(_.toDouble)))).timed

      price
        .flatMap((d, p) => IO.println(s"price = $p, duration = ${d.toMillis} ms"))
        .replicateA_(100)

    }

  def run: IO[Unit] = payoffExample
