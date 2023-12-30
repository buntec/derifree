# Derifree

Derivative pricing for the ~~lactose~~ side-effect intolerant.

Derifree is an experimental library exploring the use of free monads to
implement a contract definition language for (equity) derivatives.

*This is work in progress!*

## Examples

```scala
import cats.*
import cats.syntax.all.*
import derifree.*
import derifree.literals.*
import derifree.syntax.*

// use any time type with a `TimeLike` instance
val dsl = Dsl[java.time.Instant]
import dsl.*

val refTime = i"2023-12-27T18:00:00Z"
val expiry = refTime.plusDays(365)
val settle = expiry.plusDays(2)

val europeanCall = for
  s0 <- spot("AAPL", refTime)
  s <- spot("AAPL", expiry)
  _ <- cashflow(max(s / s0 - 1, 0.0), Ccy.USD, settle)
yield ()

val europeanUpAndOutCall = for
  s0 <- spot("AAPL", refTime)
  s <- spot("AAPL", expiry)
  p <- survivalProb(
    Barrier.Discrete(
      Barrier.Direction.Up,
      Map("AAPL" -> List((expiry, 1.5 * s0)))
    )
  )
  _ <- cashflow(p * max(s / s0 - 1, 0.0), Ccy.USD, settle)
yield ()

val europeanPut = for
  s0 <- spot("AAPL", refTime)
  s <- spot("AAPL", expiry)
  _ <- cashflow(max(1 - s / s0, 0.0), Ccy.USD, settle)
yield ()

val bermudanPut = for
  s0 <- spot("AAPL", refTime)
  _ <- List(90, 180, 270, 360)
    .map(refTime.plusDays)
    .traverse_(d =>
      spot("AAPL", d).flatMap(s =>
        exercisable(max(1 - s / s0, 0.0).some.filter(_ > 0.0), Ccy.USD, d)
      )
    )
  s <- spot("AAPL", expiry)
  _ <- cashflow(max(1 - s / s0, 0.0), Ccy.USD, settle)
yield ()

val quantoEuropeanCall = for
  s0 <- spot("AAPL", refTime)
  s <- spot("AAPL", expiry)
  _ <- cashflow(max(s / s0 - 1, 0.0), Ccy.EUR, settle)
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
  _ <- cashflow(p * max(0.0, 1 - min(s1 / s1_0, s2 / s2_0, s3 / s3_0)), Ccy.USD, settle)
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
  _ <- cashflow(p * max(0.0, 1 - min(s1 / s1_0, s2 / s2_0, s3 / s3_0)), Ccy.USD, settle)
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
    _ <- couponTimes.traverse_(t => cashflow(5.0, Ccy.USD, t))
    _ <- cashflow(
      100 * (1 - p * max(0.0, 1 - min(s1 / s1_0, s2 / s2_0, s3 / s3_0))),
      Ccy.USD,
      settle
    )
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
    _ <- callTimes.traverse_(t => callable(100.0.some, Ccy.USD, t))
    _ <- couponTimes.traverse_(t => cashflow(5.0, Ccy.USD, t))
    _ <- cashflow(
      100 * (1 - p * max(0.0, 1 - min(s1 / s1_0, s2 / s2_0, s3 / s3_0))),
      Ccy.USD,
      settle
    )
  yield ()

// let's price using a simple Black-Scholes model

val discount = YieldCurve.fromContinuouslyCompoundedRate(0.05.rate, refTime)

val aapl = models.blackscholes.Asset(
  "AAPL",
  Ccy.USD,
  Forward(195.0, divs = Nil, discount = discount, borrow = YieldCurve.zero),
  0.23.vol
)

val msft = models.blackscholes.Asset(
  "MSFT",
  Ccy.USD,
  Forward(370.0, divs = Nil, discount = discount, borrow = YieldCurve.zero),
  0.25.vol
)

val goog = models.blackscholes.Asset(
  "GOOG",
  Ccy.USD,
  Forward(135.0, divs = Nil, discount = discount, borrow = YieldCurve.zero),
  0.29.vol
)

val correlations =
  Map(
    ("AAPL", "MSFT") -> 0.7,
    ("AAPL", "GOOG") -> 0.6,
    ("MSFT", "GOOG") -> 0.65,
    ("EURUSD", "AAPL") -> -0.2
  )

// for quantos
val fxVols = Map(CcyPair(Ccy.EUR, Ccy.USD) -> 0.07.vol)

val dirNums = Sobol.directionNumbers(10000).toTry.get

val sim = models.blackscholes.simulator(
  TimeGrid.Factory.almostEquidistant(YearFraction.oneDay),
  NormalGen.Factory.sobol(dirNums),
  refTime,
  List(aapl, msft, goog),
  correlations,
  discount,
  fxVols,
)

// the number of Monte Carlo simulations
val nSims = 32767
```

```scala
europeanCall.fairValue(sim, nSims)
// res0: Either[Error, PV] = Right(value = 0.11573966972403645)

// should be cheaper than plain European call
europeanUpAndOutCall.fairValue(sim, nSims)
// res1: Either[Error, PV] = Right(value = 0.08550169593261212)

europeanPut.fairValue(sim, nSims)
// res2: Either[Error, PV] = Right(value = 0.06699973963703337)

// should be more expensive than European put
bermudanPut.fairValue(sim, nSims)
// res3: Either[Error, PV] = Right(value = 0.07099091173390634)

// what are the probabilities of early exercise?
bermudanPut.putProbabilities(sim, nSims)
// res4: Either[Error, Map[Instant, Double]] = Right(
//   value = Map(
//     2024-09-22T18:00:00Z -> 0.12759788811914427,
//     2024-03-26T18:00:00Z -> 0.06445509201330607,
//     2024-12-21T18:00:00Z -> 0.08111819818720054,
//     2024-06-24T18:00:00Z -> 0.11185033722952971
//   )
// )

quantoEuropeanCall.fairValue(sim, nSims)
// res5: Either[Error, PV] = Right(value = 0.11372366271297153)

worstOfContinuousDownAndInPut.fairValue(sim, nSims)
// res6: Either[Error, PV] = Right(value = 0.11952007594500867)

// should be cheaper than continuous barrier
worstOfEuropeanDownAndInPut.fairValue(sim, nSims)
// res7: Either[Error, PV] = Right(value = 0.09498951398431003)

barrierReverseConvertible.fairValue(sim, nSims)
// res8: Either[Error, PV] = Right(value = 106.09836341818976)

// should be cheaper than non-callable BRC
callableBarrierReverseConvertible.fairValue(sim, nSims)
// res9: Either[Error, PV] = Right(value = 105.49635516689791)

// what are the probabilities of being called?
callableBarrierReverseConvertible.callProbabilities(sim, nSims)
// res10: Either[Error, Map[Instant, Double]] = Right(
//   value = Map(
//     2024-09-22T18:00:00Z -> 0.15909298989837337,
//     2024-03-26T18:00:00Z -> 0.00173955504013184,
//     2024-12-21T18:00:00Z -> 0.16666158024842068,
//     2024-06-24T18:00:00Z -> 0.006286812952055422
//   )
// )
```
