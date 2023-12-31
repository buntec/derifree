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
package models
package blackscholes

import cats.kernel.Order
import cats.syntax.all.*
import derifree.Simulation.Realization
import derifree.Simulation.Spec
import derifree.syntax.*
import org.apache.commons.math3.util.{FastMath => math}

import scala.collection.View
import scala.collection.immutable.ArraySeq

final case class Asset[T](
    name: String,
    ccy: Ccy,
    forward: Forward[T],
    vol: Vol
)

def simulator[T: TimeLike](
    timeGridFactory: TimeGrid.Factory,
    normalGenFactory: NormalGen.Factory,
    refTime0: T,
    ccy: Ccy,
    spots: Map[String, Double],
    vols: Map[String, Vol],
    correlations: Map[(String, String), Double],
    rate: Rate
): Simulator[T] =
  val discountCurve = YieldCurve.fromContinuouslyCompoundedRate(rate, refTime0)
  val assets =
    spots
      .map((udl, s0) =>
        Asset(udl, ccy, Forward(s0, Nil, discountCurve, YieldCurve.zero), vols(udl))
      )
      .toList
  simulator(
    timeGridFactory,
    normalGenFactory,
    refTime0,
    assets,
    correlations,
    discountCurve,
    Map.empty
  )

def simulator[T: TimeLike](
    timeGridFactory: TimeGrid.Factory,
    normalGenFactory: NormalGen.Factory,
    refTime0: T,
    assets: List[Asset[T]],
    correlations: Map[(String, String), Double],
    discountCurve: YieldCurve[T],
    fxVols: Map[CcyPair, Vol]
): Simulator[T] =
  new Simulator[T]:

    def refTime: T = refTime0

    def apply(
        spec: Spec[T],
        offset: Int
    ): Either[derifree.Error, View[Realization[T]]] =
      val ccy = spec.ccy
      val udls = spec.spotObs.keySet.toList
      val nUdl = udls.length
      val assetsByUdl = assets.map(asset => asset.name -> asset).toMap
      val forwards = assets.map(asset => asset.name -> asset.forward).toMap
      val vols = assets.map(asset => asset.name -> asset.vol).toMap
      val divTimes = forwards.values.map(_.dividends.map(_.exDiv)).toList.flatten.toSet
      val obsTimes =
        (divTimes union spec.spotObs.values.reduce(_ union _) union spec.discountObs).toList
          .sorted(Order[T].toOrdering)

      val spotObsTimesE = udls
        .traverse(udl =>
          (
            spec.spotObs.get(udl).toRight(Simulator.Error(s"missing spot obs for $udl")),
            forwards.get(udl).toRight(Simulator.Error(s"missing forward for $udl"))
          )
            .mapN { (spotObs, forward) =>
              (spotObs union forward.dividends.map(_.exDiv).toSet).toList
                .sorted(Order[T].toOrdering)
            }
            .tupleLeft(udl)
        )
        .map(_.toMap)

      val yearFractions = obsTimes.map(t => TimeLike[T].yearFractionBetween(refTime, t))
      val timeGrid = timeGridFactory(yearFractions.toSet)

      val timeIndexE = (obsTimes zip yearFractions)
        .traverse((t, yf) =>
          timeGrid
            .indexOf(yf)
            .toRight(Simulator.Error(s"$yf is missing from timegrid"))
            .tupleLeft(t)
        )
        .map(_.toMap)

      val udlsWithIndex = udls.zipWithIndex.toArray
      val udlIndices = udls.indices.toArray

      val ts = timeGrid.yearFractions
      val dts = timeGrid.deltas
      val dtsArr = dts.toArray.asInstanceOf[Array[Double]]
      val sdts = dts.map(dt => math.sqrt(dt.toDouble))
      val nt = timeGrid.length

      val volsE =
        udls.traverse(udl => vols.get(udl).toRight(Simulator.Error(s"missing vol for $udl")))

      val quantoDriftAdjE = udls
        .traverse(udl =>
          assetsByUdl
            .get(udl)
            .toRight(Simulator.Error(s"missing asset for $udl"))
            .flatMap: asset =>
              if asset.ccy == ccy then 0.0.pure
              else
                val ccyPair = CcyPair(asset.ccy, ccy)
                val corr = correlations
                  .get((ccyPair.toString, udl))
                  .orElse(correlations.get((udl, ccyPair.toString)))
                  .orElse(
                    correlations
                      .get((ccyPair.inverse.toString, udl))
                      .orElse(correlations.get((udl, ccyPair.inverse.toString)))
                      .map(-_)
                  )
                  .toRight(
                    Simulator
                      .Error(s"missing correlation between $udl and $ccyPair (or its inverse)")
                  )
                val fxVol = fxVols
                  .get(ccyPair)
                  .orElse(fxVols.get(ccyPair.inverse))
                  .toRight(Simulator.Error(s"missing vol for $ccyPair"))
                (corr, fxVol).mapN((corr, fxVol) => -corr * fxVol.toDouble * asset.vol.toDouble)
        )
        .map(_.toArray)

      (
        normalGenFactory(nUdl, nt - 1),
        utils.cholesky(correlations, udls),
        timeIndexE,
        spotObsTimesE,
        volsE,
        quantoDriftAdjE
      ).mapN: (normalGen, w, timeIndex, spotObsTimes, vols0, quantoDriftAdj) =>

        val jumps = Array.ofDim[Double](nUdl, nt)
        val vols = Array.ofDim[Double](nUdl, nt)
        val discounts = Array.ofDim[Double](nt)
        val logspots = Array.ofDim[Double](nUdl, nt)
        val spots = Array.ofDim[Double](nUdl, nt)

        val logspotsPure = Array.ofDim[Double](nUdl, nt)

        val spotObsIndices =
          udls.map(udl => spotObsTimes(udl).map(timeIndex).toArray).toArray

        val isDivTime =
          udls.map(udl => spotObsTimes(udl).map(t => divTimes.contains(t)).toArray).toArray

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
          vols(i)(0) = vols0(i).toDouble
          logspotsPure(i)(0) = 0.0
          i += 1
        }

        spec.discountObs.foreach: t =>
          val j = timeIndex(t)
          discounts(j) = discountCurve.discountFactor(t)
        val discounts0 = ArraySeq.unsafeWrapArray(discounts)

        val z = Array.ofDim[Double](nUdl)

        normalGen
          .view(offset)
          .map: z0 =>
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
                val q = quantoDriftAdj(i)
                logspotsPure(i)(j + 1) =
                  logspotsPure(i)(j) + (q - 0.5 * v * v) * dtsArr(j) + v * sdts(j) * z(i)
                i += 1
              }

              j += 1
            }

            udlIndices.foreach: i =>
              val f = fwds(i)
              val fLeft = fwdsLeft(i)
              val d = divFloors(i)
              val dLeft = divFloorsLeft(i)
              var j = 0
              while (j < spotObsIndices(i).length) {
                val j0 = spotObsIndices(i)(j)
                val spotPure = math.exp(logspotsPure(i)(j0))
                val spot = (f(j) - d(j)) * spotPure + d(j)
                spots(i)(j0) = spot
                if (isDivTime(i)(j)) {
                  val spotLeft = (fLeft(j) - dLeft(j)) * spotPure + dLeft(j)
                  jumps(i)(j0) = spot - spotLeft
                } else {
                  jumps(i)(j0) = 0.0
                }
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
              )
