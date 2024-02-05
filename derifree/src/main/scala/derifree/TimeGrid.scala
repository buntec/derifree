/*
 * Copyright 2023 buntec
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package derifree

import cats.syntax.all.*

import scala.collection.Searching.Found
import scala.collection.Searching.InsertionPoint
import scala.collection.View
import scala.collection.immutable.ArraySeq
import scala.math.max
import scala.math.min
import scala.math.pow
import scala.math.round

import TimeGrid.*

case class TimeGrid(ticks: IndexedSeq[Tick]):

  def apply(i: Int): Tick = ticks(i)

  def indexOf(t: YearFraction): Option[Int] =
    val tick = nearestTick(t)
    ticks.search(tick) match
      case Found(foundIndex) => Some(foundIndex)
      case InsertionPoint(_) => None

  def slice(t1: YearFraction, t2: YearFraction): Option[TimeGrid] =
    (indexOf(t1), indexOf(t2)).mapN((i, j) => TimeGrid(ticks.slice(i, j + 1)))

  def length: Int = ticks.length

  def yearFractions: IndexedSeq[YearFraction] = ticks.map(tickToYearFraction)

  def deltas: IndexedSeq[YearFraction] =
    (yearFractions zip yearFractions.tail).map((t0, t1) => t1 - t0)

object TimeGrid:
  self =>

  trait Factory:

    def apply(includes: Set[YearFraction]): TimeGrid

  object Factory:

    def almostEquidistant(dt: YearFraction): Factory = new Factory:
      def apply(includes: Set[YearFraction]): TimeGrid =
        self.almostEquidistant(dt, includes)

    def powerRule(
        alpha: Double,
        beta: Double,
        dtMin: Double,
        dtMax: Double
    ): Factory = new Factory:
      def apply(includes: Set[YearFraction]): TimeGrid =
        self.powerRule(alpha, beta, dtMin, dtMax, includes)

  val tickSize: YearFraction = YearFraction.oneSecond

  def nearestTick(t: YearFraction): Tick = Tick(round(t / tickSize))

  def tickToYearFraction(tick: Tick): YearFraction = tickSize * tick.toLong

  def equidistant(n: Int, stop: YearFraction, includes: Set[YearFraction]): TimeGrid =
    val dt = stop / (n - 1)
    val times = List.tabulate(n)(i => dt * i).toSet
    TimeGrid(ArraySeq.unsafeWrapArray(times.union(includes).map(nearestTick).toArray.sorted))

  def almostEquidistant(dt: YearFraction, includes: Set[YearFraction]): TimeGrid =
    val knots = (includes + YearFraction.zero).toList.sorted
    val yfs = (knots zip knots.tail).foldMap: (t1, t2) =>
      val dt0 = t2 - t1
      val k = round(dt0 / dt).toInt
      val dt1 = dt0 / k
      List.tabulate(k)(i => t1 + dt1 * i).toSet + t2
    TimeGrid(ArraySeq.unsafeWrapArray(yfs.map(nearestTick).toArray.sorted))

  def powerRule(
      alpha: Double,
      beta: Double,
      dtMin: Double,
      dtMax: Double,
      includes: Set[YearFraction]
  ): TimeGrid =
    val knots = (includes + YearFraction.zero).toList.sorted
    val yfs = (knots zip knots.tail).foldMap: (t1, t2) =>
      View
        .unfold(t1) { t =>
          val tau = beta * pow(t.toDouble, alpha)
          val dt = YearFraction(max(dtMin, min(dtMax, tau)))
          if t.toDouble <= t2.toDouble then Some((t, t + dt)) else None
        }
        .toSet + t1 + t2
    TimeGrid(ArraySeq.unsafeWrapArray(yfs.map(nearestTick).toArray.sorted))
