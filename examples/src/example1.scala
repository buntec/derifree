package example1

import derifree.*
import derifree.syntax.*
import derifree.literals.*

val dsl = Dsl[java.time.Instant]
import dsl.*

val refTime = i"2023-01-01T18:00:00Z"
val expiry = i"2024-01-01T18:00:00Z"
val settle = i"2024-01-03T18:00:00Z"

val forward =
  for
    s0 <- spot("AAPL", refTime)
    s <- spot("AAPL", expiry)
  yield s / s0

val europeanVanillaCall = for
  s0 <- spot("AAPL", refTime)
  s <- spot("AAPL", expiry)
  c <- cashflow(max(s / s0 - 1, 0), settle)
yield c

val europeanVanillaPut = for
  s0 <- spot("AAPL", refTime)
  s <- spot("AAPL", expiry)
  c <- cashflow(max(1 - s / s0, 0), settle)
yield c

val europeanUpAndOutCall = for
  s0 <- spot("AAPL", refTime)
  s <- spot("AAPL", expiry)
  p <- survivalProb(
    Barrier.Discrete(
      Barrier.Direction.Up,
      Map("AAPL" -> List((expiry, 1.2 * s0)))
    )
  )
  c <- cashflow(max(s / s0 - 1, 0), settle)
yield c * p

val worstOfDownAndInPut = for
  s1_0 <- spot("AAPL", refTime)
  s2_0 <- spot("MSFT", refTime)
  s3_0 <- spot("GOOG", refTime)
  s1 <- spot("AAPL", expiry)
  s2 <- spot("MSFT", expiry)
  s3 <- spot("GOOG", expiry)
  p <- survivalProb(
    Barrier.Continuous(
      Barrier.Direction.Down,
      Map("AAPL" -> 0.8 * s1_0, "MSFT" -> 0.8 * s2_0, "GOOG" -> 0.8 * s3_0),
      from = refTime,
      to = expiry
    )
  )
  c <- cashflow(max(0, min(s1 / s1_0, s2 / s2_0, s3 / s3_0) - 1), settle)
yield c * p

import cats.effect.*
import cats.*

object Main extends IOApp.Simple:
  def run: IO[Unit] =
    Sobol
      .directionNumbersFromResource[IO](5000)
      .flatMap: dirNums =>
        val spots = Map("AAPL" -> 195.0, "MSFT" -> 370.0, "GOOG" -> 135.0)
        val vols = Map("AAPL" -> 0.2.vol, "MSFT" -> 0.25.vol, "GOOG" -> 0.3.vol)
        val correlations =
          Map(("AAPL", "MSFT") -> 0.7, ("AAPL", "GOOG") -> 0.6, ("MSFT", "GOOG") -> 0.65)
        val rate = 0.05.rate

        val nSims = (1 << 16) - 1

        val sim: Simulator[java.time.Instant] =
          Simulator.blackScholes(
            TimeGrid.Factory.almostEquidistant(YearFraction.oneDay),
            NormalGen.Factory.sobol(dirNums),
            nSims,
            refTime,
            spots,
            vols,
            correlations,
            rate
          )

        def printPrice[A: Fractional: Monoid](rv: RV[A]) = IO:
          val t1 = System.nanoTime()
          val price = rv.mean(sim)
          val t2 = System.nanoTime()
          println(f"price = $price, duration = ${(t2 - t1) * 1e-6}%.0f ms")

        for
          _ <- printPrice(forward)
          _ <- printPrice(europeanVanillaCall)
          _ <- printPrice(europeanVanillaPut)
          _ <- printPrice(europeanUpAndOutCall)
          _ <- printPrice(worstOfDownAndInPut)
        yield ()
