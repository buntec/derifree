# Derifree

Derivative pricing for the ~~lactose~~ side-effect intolerant.

This is an experimental library exploring the use of free monads to
implement a contract definition language for equity derivatives.

## Examples


```scala
import derifree.*
import derifree.literals.*

val dsl = Dsl[java.time.Instant]
import dsl.*

val refTime = i"2023-01-01T18:00:00Z"
val expiry = i"2024-01-01T18:00:00Z"
val settle = i"2024-01-03T18:00:00Z"

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
```
