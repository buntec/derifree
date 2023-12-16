package derifree

import cats.syntax.all.*

import scala.collection.Searching.Found
import scala.collection.Searching.InsertionPoint
import scala.collection.immutable.ArraySeq

import TimeGrid.*

opaque type TimeGrid = IndexedSeq[Tick]

extension (tg: TimeGrid)

  def apply(i: Int): Tick = tg(i)

  def indexOf(t: YearFraction): Option[Int] =
    val tick = nearestTick(t)
    tg.search(tick) match
      case Found(foundIndex) => Some(foundIndex)
      case InsertionPoint(_) => None

  def length: Int = tg.length

  def yearFractions: IndexedSeq[YearFraction] = tg.map(tickToYearFraction)

  def deltas: IndexedSeq[YearFraction] =
    (yearFractions zip yearFractions.tail).map((t0, t1) => t1 - t0)

object TimeGrid:
  self =>

  trait Factory:
    def apply(includes: Set[YearFraction]): TimeGrid

  object Factory:
    def almostEquidistant(dt: YearFraction): Factory = new Factory:
      def apply(includes: Set[YearFraction]): TimeGrid =
        val tg = self.almostEquidistant(dt, includes)
        // println(s"timegrid: $tg")
        tg

  val tickSize: YearFraction = YearFraction.oneSecond

  def nearestTick(t: YearFraction): Tick = Tick(math.round(t / tickSize))

  def tickToYearFraction(tick: Tick): YearFraction = tickSize * tick.toLong

  def equidistant(n: Int, stop: YearFraction, includes: Set[YearFraction]): TimeGrid =
    val dt = stop / (n - 1)
    val times = List.tabulate(n)(i => dt * i).toSet
    ArraySeq.unsafeWrapArray(times.union(includes).map(nearestTick).toArray.sorted)

  def almostEquidistant(dt: YearFraction, includes: Set[YearFraction]): TimeGrid =
    val knots = (includes + YearFraction.zero).toList.sorted
    val yfs = (knots zip knots.tail).foldMap: (t1, t2) =>
      val dt0 = t2 - t1
      val k = math.ceil(dt0 / dt).toInt
      val dt1 = dt0 / k
      List.tabulate(k)(i => t1 + dt1 * i).toSet + t2
    ArraySeq.unsafeWrapArray(yfs.map(nearestTick).toArray.sorted)
