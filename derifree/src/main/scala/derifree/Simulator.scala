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

import cats.kernel.Order
import cats.syntax.all.*
import derifree.syntax.*
import derifree.Simulation.Realization
import derifree.Simulation.Spec
import org.apache.commons.math3.util.{FastMath => math}

import scala.collection.View
import scala.collection.immutable.ArraySeq

trait Simulator[T]:

  def apply(
      spec: Simulation.Spec[T],
      nSims: Int,
      offset: Int
  ): Either[derifree.Error, View[Simulation.Realization[T]]]

  def refTime: T

object Simulator:

  enum Error extends derifree.Error:
    case MissingTimeIndex

  def blackScholes[T: TimeLike](
      timeGridFactory: TimeGrid.Factory,
      normalGenFactory: NormalGen.Factory,
      refTime0: T,
      spots: Map[String, Double],
      vols: Map[String, Vol],
      correlations: Map[(String, String), Double],
      rate: Rate
  ): Simulator[T] =
    val discountCurve = YieldCurve.fromConstantRate(rate, refTime0)
    blackScholes(
      timeGridFactory,
      normalGenFactory,
      refTime0,
      spots.map((udl, s0) => udl -> Forward.buehler(s0, Nil, discountCurve, YieldCurve.zero)),
      vols,
      correlations,
      discountCurve
    )

  def blackScholes[T: TimeLike](
      timeGridFactory: TimeGrid.Factory,
      normalGenFactory: NormalGen.Factory,
      refTime0: T,
      forwards: Map[String, Forward[T]],
      vols: Map[String, Vol],
      correlations: Map[(String, String), Double],
      discountCurve: YieldCurve[T]
  ): Simulator[T] =
    new Simulator[T]:

      def refTime: T = refTime0

      def apply(
          spec: Spec[T],
          nSims: Int,
          offset: Int
      ): Either[derifree.Error, View[Realization[T]]] =
        val udls = spec.spotObs.keySet.toList
        val nUdl = udls.length
        val divTimes = forwards.values.map(_.dividends.map(_.exDiv)).toList.flatten.toSet
        val obsTimes =
          (divTimes union spec.spotObs.values.reduce(_ union _) union spec.discountObs).toList
            .sorted(Order[T].toOrdering)

        val spotObsTimes = udls
          .map(udl =>
            udl -> (spec.spotObs(udl) union forwards(udl).dividends.map(_.exDiv).toSet).toList
              .sorted(Order[T].toOrdering)
          )
          .toMap

        val yearFractions = obsTimes.map(t => TimeLike[T].yearFractionBetween(refTime, t))
        val timeGrid = timeGridFactory(yearFractions.toSet)
        val timeIndexMaybe = (obsTimes zip yearFractions)
          .traverse((t, yf) => timeGrid.indexOf(yf).tupleLeft(t))
          .map(_.toMap)

        val udlsWithIndex = udls.zipWithIndex.toArray
        val udlIndices = udls.indices.toArray

        val ts = timeGrid.yearFractions
        val dts = timeGrid.deltas
        val dtsArr = dts.toArray.asInstanceOf[Array[Double]]
        val sdts = dts.map(dt => math.sqrt(dt.toDouble))
        val nt = timeGrid.length
        val spots0 = udls.map(udl => forwards(udl).spot).toArray
        val vols0 = udls.map(udl => vols(udl)).toArray

        (
          normalGenFactory(nUdl, nt - 1),
          utils.cholesky(correlations, udls),
          Either.fromOption(timeIndexMaybe, Error.MissingTimeIndex)
        ).mapN: (normalGen, w, timeIndex) =>

          val jumps = Array.ofDim[Double](nUdl, nt)
          val vols = Array.ofDim[Double](nUdl, nt)
          val discounts = Array.ofDim[Double](nt)
          val logspots = Array.ofDim[Double](nUdl, nt)
          val spots = Array.ofDim[Double](nUdl, nt)

          val logspotsPure = Array.ofDim[Double](nUdl, nt)

          val spotObsIndices =
            udls.map(udl => spotObsTimes(udl).map(timeIndex).toArray).toArray

          val fwds =
            udls.map(udl => spotObsTimes(udl).map(t => forwards(udl)(t)).toArray).toArray

          val fwdsLeft =
            udls
              .map(udl => spotObsTimes(udl).map(t => forwards(udl)(t.plusSeconds(-1))).toArray)
              .toArray

          val divFloors =
            udls
              .map(udl => spotObsTimes(udl).map(t => forwards(udl).dividendFloor(t)).toArray)
              .toArray

          val divFloorsLeft =
            udls
              .map(udl =>
                spotObsTimes(udl)
                  .map(t => forwards(udl).dividendFloor(t.plusSeconds(-1)))
                  .toArray
              )
              .toArray

          var i = 0
          while (i < nUdl) {
            spots(i)(0) = spots0(i)
            logspots(i)(0) = math.log(spots0(i))
            vols(i)(0) = vols0(i).toDouble
            logspotsPure(i)(0) = 0.0
            i += 1
          }

          spec.discountObs.foreach: t =>
            val j = timeIndex(t)
            discounts(j) = discountCurve.discountFactor(t)
          val discounts0 = ArraySeq.unsafeWrapArray(discounts)

          val z = Array.ofDim[Double](nUdl)

          View.unfold((0, normalGen.init(offset))): (count, normalState) =>
            if count < nSims then
              val (nextNormalState, z0) = normalGen.next.run(normalState).value

              var j = 0
              while (j < nt - 1) {

                var i = 0
                while (i < nUdl) {
                  z(i) = 0
                  var k = 0
                  while (k <= i) {
                    z(i) += z0(k)(j) * w(i)(k)
                    k += 1
                  }
                  i += 1
                }

                i = 0
                while (i < nUdl) {
                  val v = vols(i)(j)
                  vols(i)(j + 1) = v
                  logspotsPure(i)(j + 1) =
                    logspotsPure(i)(j) - 0.5 * v * v * dtsArr(j) + v * sdts(j) * z(i)
                  i += 1
                }

                j += 1
              }

              udlIndices.foreach: i =>
                val idxs = spotObsIndices(i)
                val f = fwds(i)
                val fLeft = fwdsLeft(i)
                val d = divFloors(i)
                val dLeft = divFloorsLeft(i)
                var j = 0
                while (j < idxs.length) {
                  val j0 = idxs(j)
                  val spotPure = math.exp(logspotsPure(i)(j0))
                  val spot = (f(j) - d(j)) * spotPure + d(j)
                  val spotLeft = (fLeft(j) - dLeft(j)) * spotPure + dLeft(j)
                  spots(i)(j0) = spot
                  jumps(i)(j0) = spot - spotLeft
                  logspots(i)(j0) = math.log(spots(i)(j0))
                  j += 1
                }

              val spotsMB = Map.newBuilder[String, IndexedSeq[Double]]
              val logspotsMB = Map.newBuilder[String, IndexedSeq[Double]]
              val jumpsMB = Map.newBuilder[String, IndexedSeq[Double]]
              val volsMB = Map.newBuilder[String, IndexedSeq[Vol]]

              udlsWithIndex.foreach: (udl, i) =>
                spotsMB.addOne(udl, ArraySeq.unsafeWrapArray(spots(i).clone))
                logspotsMB.addOne(udl, ArraySeq.unsafeWrapArray(logspots(i).clone))
                jumpsMB.addOne(udl, ArraySeq.unsafeWrapArray(jumps(i).clone))
                volsMB.addOne(
                  udl,
                  ArraySeq.unsafeWrapArray(vols(i).clone).asInstanceOf[IndexedSeq[Vol]]
                )

              Some(
                Simulation
                  .Realization[T](
                    spotObsTimes,
                    timeIndex,
                    ts,
                    dts,
                    spotsMB.result,
                    logspotsMB.result,
                    jumpsMB.result,
                    volsMB.result,
                    discounts0
                  ),
                (count + 1, nextNormalState)
              )
            else None
