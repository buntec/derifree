package example1

import derifree.*
import derifree.literals.*
import math.*

val dsl = Dsl[java.time.Instant]
import dsl.*

val europeanVanillaCall = for
  s <- spot("AAPL", i"2024-12-01T18:00:00Z")
  pv <- cashflow(max(s - 195, 0), i"2024-12-03T18:00:00Z")
yield pv

val europeanVanillaPut = for
  s <- spot("AAPL", i"2024-12-01T18:00:00Z")
  pv <- cashflow(max(195 - s, 0), i"2024-12-03T18:00:00Z")
yield pv

val europeanUpAndOutCall = for
  s <- spot("AAPL", i"2024-12-01T18:00:00Z")
  p <- survivalProb(
    Barrier.Discrete(
      Barrier.Direction.Up,
      Map("AAPL" -> List((i"2024-12-01T18:00:00Z", 250)))
    )
  )
  pv <- cashflow(max(195 - s, 0), i"2024-12-03T18:00:00Z")
yield pv
