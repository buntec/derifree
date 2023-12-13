package example1

import cats.syntax.all.*

import derifree.*
import derifree.syntax.*
import derifree.literals.*

val dsl = Dsl[java.time.Instant]
import dsl.*

val refTime = i"2023-01-01T18:00:00Z"
val expiry = i"2024-01-01T18:00:00Z"
val settle = i"2024-01-03T18:00:00Z"

val forward: Payoff =
  for
    s0 <- spot("AAPL", refTime)
    s <- spot("AAPL", expiry)
    _ <- cashflow(s / s0, expiry)
  yield ()

val europeanVanillaCall: Payoff = for
  s0 <- spot("AAPL", refTime)
  s <- spot("AAPL", expiry)
  _ <- cashflow(max(s / s0 - 1, 0), settle)
yield ()

val europeanVanillaPut: Payoff = for
  s0 <- spot("AAPL", refTime)
  s <- spot("AAPL", expiry)
  _ <- cashflow(max(1 - s / s0, 0), settle)
yield ()

val europeanUpAndOutCall: Payoff = for
  s0 <- spot("AAPL", refTime)
  s <- spot("AAPL", expiry)
  p <- survivalProb(
    Barrier.Discrete(
      Barrier.Direction.Up,
      Map("AAPL" -> List((expiry, 1.2 * s0)))
    )
  )
  _ <- cashflow(p * max(s / s0 - 1, 0), settle)
yield ()

val worstOfDownAndInPut: Payoff = for
  s1_0 <- spot("AAPL", refTime)
  s2_0 <- spot("MSFT", refTime)
  s3_0 <- spot("GOOG", refTime)
  s1 <- spot("AAPL", expiry)
  s2 <- spot("MSFT", expiry)
  s3 <- spot("GOOG", expiry)
  p <- hitProb(
    Barrier.Continuous(
      Barrier.Direction.Down,
      Map("AAPL" -> 0.8 * s1_0, "MSFT" -> 0.8 * s2_0, "GOOG" -> 0.8 * s3_0),
      from = refTime,
      to = expiry
    )
  )
  _ <- cashflow(p * max(0, 1 - min(s1 / s1_0, s2 / s2_0, s3 / s3_0)), settle)
yield ()

val barrierReverseConvertible: Payoff =
  val relBarrier = 0.7
  val callTimes =
    List(91, 181, 271).map(days => refTime.plus(java.time.Duration.ofDays(90)))
  val couponTimes =
    List(90, 180, 270).map(days => refTime.plus(java.time.Duration.ofDays(90))) :+ expiry
  for
    s1_0 <- spot("AAPL", refTime)
    s2_0 <- spot("MSFT", refTime)
    s3_0 <- spot("GOOG", refTime)
    s1 <- spot("AAPL", expiry)
    s2 <- spot("MSFT", expiry)
    s3 <- spot("GOOG", expiry)
    p <- hitProb(
      Barrier.Continuous(
        Barrier.Direction.Down,
        Map(
          "AAPL" -> relBarrier * s1_0,
          "MSFT" -> relBarrier * s2_0,
          "GOOG" -> relBarrier * s3_0
        ),
        from = refTime,
        to = expiry
      )
    )
    _ <- callTimes.traverse_(t => callable(100, t))
    _ <- couponTimes.traverse_(t => cashflow(5.0, t))
    _ <- cashflow(100 * (1 - p * max(0, 1 - min(s1 / s1_0, s2 / s2_0, s3 / s3_0))), settle)
  yield ()

import cats.effect.*
import cats.*

object Main extends IOApp.Simple:
  def run: IO[Unit] =
    Sobol
      .directionNumbersFromResource[IO](5000)
      .flatMap: dirNums =>
        val spots = Map("AAPL" -> 195.0, "MSFT" -> 370.0, "GOOG" -> 135.0)
        val vols = Map("AAPL" -> 0.3.vol, "MSFT" -> 0.33.vol, "GOOG" -> 0.35.vol)
        val correlations =
          Map(("AAPL", "MSFT") -> 0.7, ("AAPL", "GOOG") -> 0.6, ("MSFT", "GOOG") -> 0.65)
        val rate = 0.01.rate

        val sim: Simulator[java.time.Instant] =
          Simulator.blackScholes(
            TimeGrid.Factory.almostEquidistant(YearFraction.oneDay),
            NormalGen.Factory.sobol(dirNums),
            refTime,
            spots,
            vols,
            correlations,
            rate
          )

        val nSims = (1 << 16) - 1

        def printPrice(payoff: Payoff) = IO:
          val t1 = System.nanoTime()
          val price = payoff.fairValue(sim, nSims)
          val t2 = System.nanoTime()
          println(f"price = $price, duration = ${(t2 - t1) * 1e-6}%.0f ms")

        for
          // _ <- printPrice(forward)
          // _ <- printPrice(europeanVanillaCall)
          // _ <- printPrice(europeanVanillaPut)
          // _ <- printPrice(europeanUpAndOutCall)
          // _ <- printPrice(worstOfDownAndInPut)
          _ <- printPrice(barrierReverseConvertible).replicateA_(10)
        yield ()
