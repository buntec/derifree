package derifree

import derifree.Simulation.Realization
import derifree.Simulation.Spec
import cats.syntax.all.*
import cats.kernel.Order
import scala.collection.immutable.ArraySeq

trait Simulator[T]:

  def apply(spec: Simulation.Spec[T]): Either[String, Seq[Simulation.Realization[T]]]

object Simulator:

  def blackScholes[T: TimeLike](
      sobol: Sobol,
      nSimulations: Int
  )(
      ref: T,
      spots: Map[String, Double],
      vols: Map[String, Vol],
      correlations: Map[(String, String), Double],
      rate: Rate
  ): Simulator[T] =
    new Simulator[T]:
      def apply(spec: Spec[T]): Either[String, Seq[Realization[T]]] =
        val udls = spec.spotObs.keySet.toList
        val nUdl = udls.length
        val obsTimes = (spec.spotObs.values.reduce(_ union _) union spec.discountObs).toList.sorted(Order[T].toOrdering)
        val yearFractions = obsTimes.map(t => TimeLike[T].yearFractionBetween(ref, t))
        val timeGrid = TimeGrid.equidistant(100, yearFractions.max, yearFractions.toSet)
        val timeIndexMaybe =
          (obsTimes zip yearFractions).traverse((t, yf) => timeGrid.indexOf(yf).tupleLeft(t)).map(_.toMap)
        val dts = timeGrid.deltas
        val sdts = dts.map(dt => math.sqrt(dt.toDouble))
        val nt = timeGrid.length
        val spots0 = udls.map(udl => spots(udl)).toArray
        val vols0 = udls.map(udl => vols(udl)).toArray
        val r = rate.toDouble
        Either
          .fromOption(timeIndexMaybe, "failed to compute time index")
          .map(timeIndex =>
            LazyList.unfold((0, sobol.initialState)): (count, sobolState) =>
              if count < nSimulations then
                val (nextSobolState, sobolNumbers) = sobol.next.run(sobolState).value
                val z = sobolNumbers.map(normal.inverseCdf)

                val jumps = Array.ofDim[Double](nUdl, nt)
                val vols = Array.ofDim[Double](nUdl, nt)
                val ls = Array.ofDim[Double](nUdl, nt)

                var i = 0
                while (i < nUdl) {
                  ls(i)(0) = math.log(spots0(i))
                  vols(i)(0) = vols0(i).toDouble
                  var j = 0
                  while (j < nt) {
                    val v = vols(i)(j)
                    vols(i)(j + 1) = v
                    ls(i)(j + 1) = ls(i)(j) + (r.toDouble - 0.5 * v * v) * dts(i).toDouble + v * sdts(i) * z(i * nt + j)
                    j += 1
                  }
                  i += 1
                }

                val discounts = yearFractions.map(t => math.exp(-rate * t)).toArray.toIndexedSeq

                val spots =
                  udls.zipWithIndex.map((udl, i) => udl -> ArraySeq.unsafeWrapArray(ls(i).map(math.exp))).toMap

                val jumps0 =
                  udls.zipWithIndex.map((udl, i) => udl -> ArraySeq.unsafeWrapArray(jumps(i))).toMap

                val vols1 =
                  udls.zipWithIndex.map((udl, i) => udl -> ArraySeq.unsafeWrapArray(vols(i).map(Vol(_)))).toMap

                Some(
                  Simulation.Realization[T](timeIndex, dts, spots, jumps0, vols1, discounts),
                  (count + 1, nextSobolState)
                )
              else None
          )
