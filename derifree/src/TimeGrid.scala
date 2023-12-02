package derifree

opaque type TimeGrid = IndexedSeq[Tick]

import TimeGrid.*
import scala.collection.Searching.Found
import scala.collection.Searching.InsertionPoint

extension (tg: TimeGrid)

  def apply(i: Int): Tick = tg(i)

  def indexOf(t: YearFraction): Option[Int] =
    val tick = nearestTick(t)
    tg.search(tick) match
      case Found(foundIndex) => Some(foundIndex)
      case InsertionPoint(_) => None

  def yearFractions: IndexedSeq[YearFraction] = tg.map(tickToYearFraction)

  def deltas: IndexedSeq[YearFraction] =
    (yearFractions zip yearFractions.tail).map((t0, t1) => t1 - t0)

object TimeGrid:

  val tickSize: YearFraction = YearFraction.oneSecond

  def nearestTick(t: YearFraction): Tick = Tick(math.round(t.ratio(tickSize)))

  def tickToYearFraction(tick: Tick): YearFraction = tickSize * tick.toLong

  def equidistant(n: Int, stop: YearFraction, includes: Set[YearFraction]): TimeGrid =
    val dt = stop / (n - 1)
    val times = List.tabulate(n)(i => dt * i).toSet
    IArray.unsafeFromArray(times.union(includes).map(nearestTick).toArray.sorted)
