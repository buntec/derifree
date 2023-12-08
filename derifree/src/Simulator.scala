package derifree

import cats.kernel.Order
import cats.syntax.all.*
import derifree.Simulation.Realization
import derifree.Simulation.Spec
import org.apache.commons.math3.util.{FastMath => math}

import scala.collection.immutable.ArraySeq

trait Simulator[T]:

  def apply(spec: Simulation.Spec[T]): Either[derifree.Error, Seq[Simulation.Realization[T]]]

object Simulator:

  enum Error extends derifree.Error:
    case MissingTimeIndex

  def blackScholes[T: TimeLike](
      nSimulations: Int,
      directionNumbers: Sobol.DirectionNumbers
  )(
      ref: T,
      spots: Map[String, Double],
      vols: Map[String, Vol],
      correlations: Map[(String, String), Double],
      rate: Rate
  ): Simulator[T] =
    new Simulator[T]:
      def apply(spec: Spec[T]): Either[derifree.Error, Seq[Realization[T]]] =
        val udls = spec.spotObs.keySet.toList
        val nUdl = udls.length
        val obsTimes = (spec.spotObs.values.reduce(_ union _) union spec.discountObs).toList
          .sorted(Order[T].toOrdering)

        val yearFractions = obsTimes.map(t => TimeLike[T].yearFractionBetween(ref, t))
        val timeGrid = TimeGrid.equidistant(100, yearFractions.max, yearFractions.toSet)
        val timeIndexMaybe =
          (obsTimes zip yearFractions)
            .traverse((t, yf) => timeGrid.indexOf(yf).tupleLeft(t))
            .map(_.toMap)

        val ts = timeGrid.yearFractions
        val dts = timeGrid.deltas
        val sdts = dts.map(dt => math.sqrt(dt.toDouble))
        val nt = timeGrid.length
        val spots0 = udls.map(udl => spots(udl)).toArray
        val vols0 = udls.map(udl => vols(udl)).toArray
        val r = rate.toDouble
        (
          NormalGen.fromSobol(nUdl, nt - 1, directionNumbers),
          Either.fromOption(timeIndexMaybe, Error.MissingTimeIndex)
        ).mapN: (normalGen, timeIndex) =>

          val jumps = Array.ofDim[Double](nUdl, nt)
          val vols = Array.ofDim[Double](nUdl, nt)
          val discounts = Array.ofDim[Double](nt)
          val ls = Array.ofDim[Double](nUdl, nt) // log-spots
          val spots = Array.ofDim[Double](nUdl, nt)

          val spotObsIndices =
            udls.map(udl => spec.spotObs(udl).toList.map(timeIndex).sorted.toArray).toArray

          // println(spotObsIndices.show)

          var i = 0
          while (i < nUdl) {
            ls(i)(0) = math.log(spots0(i))
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

          LazyList.unfold((0, normalGen.init)): (count, normalState) =>
            if count < nSimulations then
              val (nextNormalState, z) = normalGen.next.run(normalState).value

              var j = 0
              while (j < nt - 1) {
                var i = 0
                while (i < nUdl) {
                  val v = vols(i)(j)
                  vols(i)(j + 1) = v
                  ls(i)(j + 1) = ls(i)(j) + (r.toDouble - 0.5 * v * v) * dts(
                    i
                  ).toDouble + v * sdts(i) * z(i)(j)
                  i += 1
                }
                j += 1
              }

              udls.indices.foreach: i =>
                val idxs = spotObsIndices(i)
                var j = 0
                while (j < idxs.length) {
                  val j0 = idxs(j)
                  spots(i)(j0) = math.exp(ls(i)(j0))
                  j += 1
                }

              // println(spots.show)

              val spots1 =
                udls.zipWithIndex
                  .map((udl, i) => udl -> ArraySeq.unsafeWrapArray(spots(i).clone))
                  .toMap

              val jumps0 =
                udls.zipWithIndex
                  .map((udl, i) => udl -> ArraySeq.unsafeWrapArray(jumps(i).clone))
                  .toMap

              val vols1 =
                udls.zipWithIndex
                  .map((udl, i) =>
                    udl -> ArraySeq
                      .unsafeWrapArray(vols(i).clone)
                      .asInstanceOf[IndexedSeq[Vol]]
                  )
                  .toMap

              Some(
                Simulation
                  .Realization[T](timeIndex, ts, dts, spots1, jumps0, vols1, discounts0),
                (count + 1, nextNormalState)
              )
            else None
