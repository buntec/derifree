package derifree

import cats.data.Writer
import cats.kernel.Monoid
import cats.syntax.all.*
import cats.~>
import org.apache.commons.math3.util.{FastMath => math}

import scala.math.Fractional.Implicits.*

import Compiler.*

private[derifree] trait Compiler[T]:

  def run[V: Fractional: Monoid](dsl: Dsl[T], simulator: Simulator[T])(
      rv: dsl.RV[V]
  ): Either[derifree.Error, V] = {
    val log = rv.foldMap(toSpec(dsl)).run(0)
    simulator(log).flatMap(sims =>
      val (sumE, n) = sims.foldMapM(sim => (rv.foldMap(toValue(dsl, sim)), Fractional[V].one))
      sumE.map(_ / n)
    )
  }

  def toSpec(dsl: Dsl[T]): dsl.RVA ~> ([A] =>> Writer[Simulation.Spec[T], A])

  def toValue(
      dsl: Dsl[T],
      sim: Simulation.Realization[T]
  ): dsl.RVA ~> ([A] =>> Either[Error, A])

private[derifree] object Compiler:

  enum Error extends derifree.Error:
    case Generic(override val getMessage: String)

  def apply[T: TimeLike]: Compiler[T] =
    new Compiler[T]:

      private val contBarrierObsTimesCache =
        scala.collection.mutable.HashMap.empty[(T, T), Set[T]]

      private val contBarrierObsIndicesCache =
        scala.collection.mutable.HashMap.empty[(T, T), Either[Error, Array[Int]]]

      private def mkContBarrierObsTimes(from: T, to: T): Set[T] =
        Set(from, to) <+> TimeLike[T].dailyStepsBetween(from, to).toSet

      private def getContBarrierObsTimes(from: T, to: T): Set[T] =
        contBarrierObsTimesCache.getOrElseUpdate((from, to), mkContBarrierObsTimes(from, to))

      def toSpec(dsl: Dsl[T]): dsl.RVA ~> ([A] =>> Writer[Simulation.Spec[T], A]) =
        new (dsl.RVA ~> ([A] =>> Writer[Simulation.Spec[T], A])):
          def apply[A](fa: dsl.RVA[A]): Writer[Simulation.Spec[T], A] = fa match
            case dsl.Spot(ticker, time) =>
              Writer(Simulation.Spec.spotObs(ticker, Set(time)), 1.0)

            case dsl.Cashflow(amount, time) =>
              Writer(Simulation.Spec.discountObs(time), PV(1.0))

            case dsl.HitProb(dsl.Barrier.Discrete(_, _, levels)) =>
              Writer(
                levels.toList.foldMap((ticker, obs) =>
                  Simulation.Spec.spotObs(ticker, obs.map(_(0)).toSet)
                ),
                0.5
              )

            case dsl.HitProb(dsl.Barrier.Continuous(_, _, levels, from, to)) =>
              Writer(
                levels.keys.toList.foldMap(ticker =>
                  Simulation.Spec.spotObs(
                    ticker,
                    getContBarrierObsTimes(from, to)
                  )
                ),
                0.5
              )

      def toValue(
          dsl: Dsl[T],
          sim: Simulation.Realization[T]
      ): dsl.RVA ~> ([A] =>> Either[Error, A]) =
        new (dsl.RVA ~> ([A] =>> Either[Error, A])):
          def apply[A](fa: dsl.RVA[A]): Either[Error, A] = fa match
            case dsl.Spot(ticker, time) =>
              Either
                .fromOption(
                  sim.timeIndex
                    .get(time),
                  Error.Generic(s"missing time index for $time")
                )
                .flatMap(i =>
                  Either
                    .fromOption(
                      sim.spots.get(ticker),
                      Error.Generic(s"missing spots for $ticker")
                    )
                    .map: spots =>
                      val spot = spots(i)
                      // println(s"spot($ticker, $time)=$spot")
                      spot
                )

            case dsl.Cashflow(amount, time) =>
              Either.fromOption(
                sim.timeIndex.get(time).map(i => PV(amount * sim.discounts(i))),
                Error.Generic(s"missing time index for $time")
              )

            case dsl.HitProb(dsl.Barrier.Discrete(direction, policy, levels)) =>
              (
                levels.keys.toList
                  .traverse(ticker =>
                    Either
                      .fromOption(
                        sim.spots.get(ticker),
                        Error.Generic(s"missing spots for $ticker")
                      )
                      .tupleLeft(ticker)
                  )
                  .map(_.toMap),
                levels.keys.toList
                  .traverse(ticker =>
                    Either
                      .fromOption(
                        sim.jumps.get(ticker),
                        Error.Generic(s"missing jumps for $ticker")
                      )
                      .tupleLeft(ticker)
                  )
                  .map(_.toMap),
                levels.keys.toList
                  .traverse(ticker =>
                    Either
                      .fromOption(
                        sim.vols.get(ticker),
                        Error.Generic(s"missing vols for $ticker")
                      )
                      .tupleLeft(ticker)
                  )
                  .map(_.toMap),
              ).mapN { case (spots, jumps, vols) =>
                val sign = direction match {
                  case dsl.Barrier.Direction.Up   => 1
                  case dsl.Barrier.Direction.Down => -1
                }

                policy match
                  case dsl.Barrier.Policy.And =>
                    val hitTimes = levels
                      .map((ticker, obs) =>
                        ticker -> obs
                          .filter((time, level) =>
                            sign * (spots(ticker)(sim.timeIndex(time)) - level) >= 0
                          )
                          .map(_(0))
                          .toSet
                      )
                      .values
                      .reduce(_ intersect _)
                    if hitTimes.nonEmpty then 1.0 else 0.0
                  case dsl.Barrier.Policy.Or =>
                    val hit = levels.exists((ticker, obs) =>
                      obs.exists((time, level) =>
                        sign * (spots(ticker)(sim.timeIndex(time)) - level) >= 0
                      )
                    )
                    if hit then 1.0 else 0.0

              }

            case dsl.HitProb(dsl.Barrier.Continuous(direction, policy, levels, from, to)) =>
              Either.raiseUnless(policy == dsl.Barrier.Policy.Or)(
                Error.Generic("Only `or` barriers are supported for continuous monitoring")
              ) *>
                (
                  contBarrierObsIndicesCache.getOrElseUpdate(
                    (from, to),
                    Either.fromOption(
                      mkContBarrierObsTimes(from, to).toList
                        .traverse(t => sim.timeIndex.get(t))
                        .map(_.toArray.sorted),
                      Error.Generic("missing spot time index")
                    )
                  ),
                  levels.keys.toList
                    .traverse(ticker =>
                      Either
                        .fromOption(
                          sim.spots.get(ticker),
                          Error.Generic(s"missing spots for $ticker")
                        )
                        .tupleLeft(ticker)
                    )
                    .map(_.toMap),
                  levels.keys.toList
                    .traverse(ticker =>
                      Either
                        .fromOption(
                          sim.logSpots.get(ticker),
                          Error.Generic(s"missing log-spots for $ticker")
                        )
                        .tupleLeft(ticker)
                    )
                    .map(_.toMap),
                  levels.keys.toList
                    .traverse(ticker =>
                      Either
                        .fromOption(
                          sim.jumps.get(ticker),
                          Error.Generic(s"missing jumps for $ticker")
                        )
                        .tupleLeft(ticker)
                    )
                    .map(_.toMap),
                  levels.keys.toList
                    .traverse(ticker =>
                      Either
                        .fromOption(
                          sim.vols.get(ticker),
                          Error.Generic(s"missing vols for $ticker")
                        )
                        .tupleLeft(ticker)
                    )
                    .map(_.toMap),
                ).mapN { case (spotObsIdxs, spots, logSpots, jumps, vols) =>
                  var p = 1.0 // survival probability
                  val sign = direction match {
                    case dsl.Barrier.Direction.Up   => -1
                    case dsl.Barrier.Direction.Down => 1
                  }
                  levels.foreach: (ticker, level) =>
                    val logLevel = math.log(level)
                    val s = spots(ticker)
                    val ls = logSpots(ticker)
                    val v = vols(ticker)
                    val jmp = jumps(ticker)
                    var i = 0
                    while (i < spotObsIdxs.length - 1) {
                      val i0 = spotObsIdxs(i)
                      val i1 = spotObsIdxs(i + 1)
                      val dt = sim.ts(i1) - sim.ts(i0)
                      val s0 = s(i0)
                      val ls0 = ls(i0)
                      // we want s1 = S(t_{i+1}-) (limit from the left) and use Jump(t_i) = S(t_i) - S(t_i-)
                      val jmp1 = jmp(i1)
                      val s1 = s(i1) - jmp1
                      val s1r = s(i1)
                      val ls1r = ls(i1)
                      val vol = v(i0)
                      // println(s"s0 = $s0, s1 = $s1, s1r = $s1r, vol =$vol")
                      if (
                        sign * (s0 - level) <= 0 || sign * (s1 - level) <= 0 || sign * (s1r - level) <= 0
                      ) {
                        p = 0.0
                      } else {
                        val d1 = (logLevel - ls0)
                        val d2 = if (jmp1 != 0) then math.log(level / s1) else logLevel - ls1r
                        val u = d1 * d2 / (vol * vol * dt)
                        p *= (1 - math.exp(-2 * u))
                      }
                      i += 1
                    }
                  1 - p
                }
