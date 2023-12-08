package derifree

import cats.kernel.Order
import cats.syntax.all.*
import derifree.Simulation.Realization
import derifree.Simulation.Spec
import org.apache.commons.math3.util.{FastMath => math}

import scala.collection.immutable.ArraySeq
import org.ejml.data.DMatrixRMaj
import org.ejml.dense.row.factory.DecompositionFactory_DDRM

trait Simulator[T]:

  def apply(
      spec: Simulation.Spec[T]
  ): Either[derifree.Error, LazyList[Simulation.Realization[T]]]

object Simulator:

  enum Error extends derifree.Error:
    case MissingTimeIndex

  private def cholesky(
      corrs: Map[(String, String), Double],
      udls: List[String]
  ): IndexedSeq[IndexedSeq[Double]] =
    val udlsArr = udls.toArray
    val n = udlsArr.length

    val data = for {
      udl1 <- udls
      udl2 <- udls
    } yield
      if (udl1 == udl2) then 1.0 else corrs.get((udl1, udl2)).getOrElse(corrs((udl2, udl1)))

    val mat = new DMatrixRMaj(udlsArr.length, udlsArr.length, true, data*)
    val decomp = DecompositionFactory_DDRM.chol(true)
    val success = decomp.decompose(mat)
    if (!success) {
      throw new RuntimeException("cholesky decomp failed")
    }
    val lower = decomp.getT(null)
    val res = Array.ofDim[Double](n, n)
    var i = 0
    while (i < n) {
      var j = 0
      while (j < n) {
        res(i)(j) = lower.get(i, j)
        j += 1
      }
      i += 1
    }
    ArraySeq.unsafeWrapArray(res.map(row => ArraySeq.unsafeWrapArray(row)))

  def blackScholes[T: TimeLike](
      timeGridFactory: TimeGrid.Factory,
      normalGenFactory: NormalGen.Factory,
      nSimulations: Int,
      refTime: T,
      spots: Map[String, Double],
      vols: Map[String, Vol],
      correlations: Map[(String, String), Double],
      rate: Rate
  ): Simulator[T] =
    new Simulator[T]:
      def apply(spec: Spec[T]): Either[derifree.Error, LazyList[Realization[T]]] =
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
        val dts = timeGrid.deltas
        val sdts = dts.map(dt => math.sqrt(dt.toDouble))
        val nt = timeGrid.length
        val spots0 = udls.map(udl => spots(udl)).toArray
        val vols0 = udls.map(udl => vols(udl)).toArray
        val r = rate.toDouble

        val chol = cholesky(correlations, udls)

        (
          normalGenFactory(nUdl, nt - 1),
          Either.fromOption(timeIndexMaybe, Error.MissingTimeIndex)
        ).mapN: (normalGen, timeIndex) =>

          val jumps = Array.ofDim[Double](nUdl, nt)
          val vols = Array.ofDim[Double](nUdl, nt)
          val discounts = Array.ofDim[Double](nt)
          val logspots = Array.ofDim[Double](nUdl, nt)
          val spots = Array.ofDim[Double](nUdl, nt)

          val spotObsIndices =
            udls.map(udl => spec.spotObs(udl).toList.map(timeIndex).sorted.toArray).toArray

          // println(spotObsIndices.show)

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

          LazyList.unfold((0, normalGen.init)): (count, normalState) =>
            if count < nSimulations then
              val (nextNormalState, z0) = normalGen.next.run(normalState).value

              var j = 0
              while (j < nt - 1) {

                var i = 0
                while (i < nUdl) {
                  z(i) = 0
                  var k = 0
                  while (k < i) {
                    z(i) += z0(k)(j) * chol(i)(k)
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
