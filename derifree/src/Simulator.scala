package derifree

import cats.kernel.Order
import cats.syntax.all.*
import derifree.Simulation.Realization
import derifree.Simulation.Spec
import org.apache.commons.math3.util.{FastMath => math}

import scala.collection.immutable.ArraySeq
import scala.collection.View

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
    new Simulator[T]:

      def refTime: T = refTime0

      def apply(
          spec: Spec[T],
          nSims: Int,
          offset: Int
      ): Either[derifree.Error, View[Realization[T]]] =
        val udls = spec.spotObs.keySet.toList
        val nUdl = udls.length
        val obsTimes = (spec.spotObs.values.reduce(_ union _) union spec.discountObs).toList
          .sorted(Order[T].toOrdering)

        val yearFractions = obsTimes.map(t => TimeLike[T].yearFractionBetween(refTime, t))
        val timeGrid = timeGridFactory(yearFractions.toSet)
        val timeIndexMaybe = (obsTimes zip yearFractions)
          .traverse((t, yf) => timeGrid.indexOf(yf).tupleLeft(t))
          .map(_.toMap)

        val ts = timeGrid.yearFractions
        // println(s"time grid: $ts")

        val dts = timeGrid.deltas
        val sdts = dts.map(dt => math.sqrt(dt.toDouble))
        val nt = timeGrid.length
        val spots0 = udls.map(udl => spots(udl)).toArray
        val vols0 = udls.map(udl => vols(udl)).toArray
        val r = rate.toDouble

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

          val spotObsIndices =
            udls.map(udl => spec.spotObs(udl).toList.map(timeIndex).sorted.toArray).toArray

          var i = 0
          while (i < nUdl) {
            logspots(i)(0) = math.log(spots0(i))
            spots(i)(0) = spots0(i)
            vols(i)(0) = vols0(i).toDouble
            i += 1
          }

          discounts(0) = 1.0
          var j = 1
          while (j < nt) {
            discounts(j) = math.exp(-rate * ts(j))
            j += 1
          }
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
                  logspots(i)(j + 1) =
                    logspots(i)(j) + (r - 0.5 * v * v) * dts(i).toDouble + v * sdts(i) * z(i)
                  i += 1
                }
                j += 1
              }

              udls.indices.foreach: i =>
                val idxs = spotObsIndices(i)
                var j = 0
                while (j < idxs.length) {
                  val j0 = idxs(j)
                  spots(i)(j0) = math.exp(logspots(i)(j0))
                  j += 1
                }

              Some(
                Simulation
                  .Realization[T](
                    timeIndex,
                    ts,
                    dts,
                    udls.zipWithIndex
                      .map((udl, i) => udl -> ArraySeq.unsafeWrapArray(spots(i).clone))
                      .toMap,
                    udls.zipWithIndex
                      .map((udl, i) => udl -> ArraySeq.unsafeWrapArray(logspots(i).clone))
                      .toMap,
                    udls.zipWithIndex
                      .map((udl, i) => udl -> ArraySeq.unsafeWrapArray(jumps(i).clone))
                      .toMap,
                    udls.zipWithIndex
                      .map((udl, i) =>
                        udl -> ArraySeq
                          .unsafeWrapArray(vols(i).clone)
                          .asInstanceOf[IndexedSeq[Vol]]
                      )
                      .toMap,
                    discounts0
                  ),
                (count + 1, nextNormalState)
              )
            else None
