# Derifree

Derivative pricing for the ~~lactose~~ side-effect intolerant.

This is an experimental library exploring the use of free monads to
implement a contract definition language for equity derivatives.

## Examples

```scala
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

```scala
europeanCall.fairValue(sim, nSims)
// res0: Either[Error, PV] = Right(value = 0.16876819953535963)

europeanPut.fairValue(sim, nSims)
// res1: Either[Error, PV] = Right(value = 0.12005830195618698)

bermudanPut.fairValue(sim, nSims)
// res2: Either[Error, PV] = Right(value = 0.12405844388377392)

bermudanPut.putProbabilities(sim, nSims)
// res3: Either[Error, Map[Instant, Double]] = Right(
//   value = Map(
//     2023-09-28T18:00:00Z -> 0.15707876827295755,
//     2023-04-01T18:00:00Z -> 0.04016235847041231,
//     2023-12-27T18:00:00Z -> 0.10843226416821802,
//     2023-06-30T18:00:00Z -> 0.10135196996978668
//   )
// )

callableBarrierReverseConvertible.fairValue(sim, nSims)
// res4: Either[Error, PV] = Right(value = 99.17636094773215)

callableBarrierReverseConvertible.callProbabilities(sim, nSims)
// res5: Either[Error, Map[Instant, Double]] = Right(
//   value = Map(
//     2023-09-28T18:00:00Z -> 0.10998870815149388,
//     2023-04-01T18:00:00Z -> 0.0013428144169438765,
//     2023-12-27T18:00:00Z -> 0.11050752281258583,
//     2023-06-30T18:00:00Z -> 0.009491256447035128
//   )
// )
```
