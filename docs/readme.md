# Derifree

Derivative pricing for the ~~lactose~~ side-effect intolerant.

This is an experimental library exploring the use of free monads to
implement a contract definition language for equity derivatives.

## Examples

```scala mdoc:silent
import cats.*
import cats.syntax.all.*
import derifree.*
import derifree.literals.*
import derifree.syntax.*

val dsl = Dsl[java.time.Instant]
import dsl.*

val refTime = i"2023-01-01T18:00:00Z"
val expiry = refTime.plusDays(365)
val settle = expiry.plusDays(2)

val europeanCall = for
  s0 <- spot("AAPL", refTime)
  s <- spot("AAPL", expiry)
  _ <- cashflow(max(s / s0 - 1, 0), settle)
yield ()

val europeanPut = for
  s0 <- spot("AAPL", refTime)
  s <- spot("AAPL", expiry)
  _ <- cashflow(max(1 - s / s0, 0), settle)
yield ()

val bermudanPut = for
  s0 <- spot("AAPL", refTime)
  _ <- List(90, 180, 270, 360)
    .map(refTime.plusDays)
    .traverse_(d =>
      spot("AAPL", d).flatMap(s => exercisable(max(1 - s / s0, 0).some.filter(_ > 0), d))
    )
  s <- spot("AAPL", expiry)
  _ <- cashflow(max(1 - s / s0, 0), settle)
yield ()

val europeanUpAndOutCall = for
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

val worstOfContinuousDownAndInPut = for
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

val worstOfEuropeanDownAndInPut = for
  s1_0 <- spot("AAPL", refTime)
  s2_0 <- spot("MSFT", refTime)
  s3_0 <- spot("GOOG", refTime)
  s1 <- spot("AAPL", expiry)
  s2 <- spot("MSFT", expiry)
  s3 <- spot("GOOG", expiry)
  p <- hitProb(
    Barrier.Discrete(
      Barrier.Direction.Down,
      Map(
        "AAPL" -> List((expiry, 0.8 * s1_0)),
        "MSFT" -> List((expiry, 0.8 * s2_0)),
        "GOOG" -> List((expiry, 0.8 * s3_0))
      )
    )
  )
  _ <- cashflow(p * max(0, 1 - min(s1 / s1_0, s2 / s2_0, s3 / s3_0)), settle)
yield ()

val barrierReverseConvertible =
  val relBarrier = 0.7
  val couponTimes = List(90, 180, 270, 360).map(refTime.plusDays)
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
    _ <- couponTimes.traverse_(t => cashflow(5.0, t))
    _ <- cashflow(100 * (1 - p * max(0, 1 - min(s1 / s1_0, s2 / s2_0, s3 / s3_0))), settle)
  yield ()

val callableBarrierReverseConvertible =
  val relBarrier = 0.7
  val callTimes = List(90, 180, 270, 360).map(refTime.plusDays)
  val couponTimes = List(90, 180, 270, 360).map(refTime.plusDays)
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
    _ <- callTimes.traverse_(t => callable(100.0.some, t))
    _ <- couponTimes.traverse_(t => cashflow(5.0, t))
    _ <- cashflow(100 * (1 - p * max(0, 1 - min(s1 / s1_0, s2 / s2_0, s3 / s3_0))), settle)
  yield ()

val spots = Map("AAPL" -> 195.0, "MSFT" -> 370.0, "GOOG" -> 135.0)

val vols = Map("AAPL" -> 0.37.vol, "MSFT" -> 0.31.vol, "GOOG" -> 0.33.vol)

val correlations =
Map(("AAPL", "MSFT") -> 0.7, ("AAPL", "GOOG") -> 0.6, ("MSFT", "GOOG") -> 0.65)

val rate = 0.05.rate

val dirNums = Sobol.directionNumbers(5000).toTry.get

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

val nSims = 32767
```

```scala mdoc
europeanCall.fairValue(sim, nSims)

europeanPut.fairValue(sim, nSims)

bermudanPut.fairValue(sim, nSims)

bermudanPut.putProbabilities(sim, nSims)

callableBarrierReverseConvertible.fairValue(sim, nSims)

callableBarrierReverseConvertible.callProbabilities(sim, nSims)
```