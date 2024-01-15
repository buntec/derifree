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

import alleycats.std.iterable.*
import cats.Monoid
import cats.data.Chain
import cats.data.Writer
import cats.data.WriterT
import cats.free.Free
import cats.syntax.all.*
import cats.~>
import derifree.syntax.given
import org.apache.commons.math3.util.{FastMath => math}

import Compiler.*

private[derifree] sealed trait Compiler[T]:

  def mean[A: Fractional: Monoid](
      dsl: Dsl[T],
      simulator: Simulator[T],
      nSims: Int,
      rv: dsl.RV[A]
  ): Either[derifree.Error, A] =
    for
      spec0 <- spec(dsl)(rv)
      sims <- simulator(spec0, 0)
      (sumE, n) = (sims.take(nSims): Iterable[Simulation.Realization[T]]).foldMapM(sim =>
        (eval(dsl, sim, rv), Fractional[A].one)
      )
      sum <- sumE
    yield Fractional[A].div(sum, n)

  def fairValue[A](
      dsl: Dsl[T],
      simulator: Simulator[T],
      nSims: Int,
      rv: dsl.RV[A]
  )(using TimeLike[T]): Either[derifree.Error, FairValueResult[T]] =
    spec(dsl)(rv).flatMap: spec0 =>
      if spec0.callTimes.nonEmpty || spec0.putTimes.nonEmpty then
        fairValueByLsm(dsl, simulator, nSims / 8, nSims, rv)
      else
        simulator(spec0, 0).flatMap: sims =>
          val (sumE, n) =
            (sims.take(nSims): Iterable[Simulation.Realization[T]]).foldMapM(sim =>
              (profile(dsl, sim, rv).map(_.cashflows.map(_(1)).sum), 1)
            )
          sumE.map(_ / n).map(pv => FairValueResult(pv, Map.empty, Map.empty))

  private def fairValueByLsm[A](
      dsl: Dsl[T],
      simulator: Simulator[T],
      nSimsLsm: Int,
      nSims: Int,
      rv: dsl.RV[A]
  )(using TimeLike[T]): Either[derifree.Error, FairValueResult[T]] =
    for
      estimators <- lsmEstimators[A](dsl, simulator, nSimsLsm, 0, rv)
      spec0 <- spec(dsl)(rv)
      earlyExTimes = (spec0.callTimes union spec0.putTimes).toList.sorted
      facRvs <- factorRvs(dsl, earlyExTimes, simulator.refTime, rv)
      combinedSpec <- spec(dsl)(facRvs.sequence *> rv)
      sims <- simulator(combinedSpec, offset = nSimsLsm)
      tuple <- (sims.take(nSims): Iterable[Simulation.Realization[T]]).foldMapM { sim =>
        val factorsE = facRvs.traverse(rv => eval(dsl, sim, rv))
        val profileE = profile(dsl, sim, rv)
        (factorsE, profileE).mapN((factors, profile) =>
          val earlyExerciseTimeAndAmountMaybe =
            (earlyExTimes zip factors).collectFirstSome:
              case (t, fac) =>
                estimators
                  .get(t)
                  .flatMap: estimator =>
                    val callAmountMaybe = profile.callAmounts.get(t)
                    val putAmountMaybe = profile.putAmounts.get(t)
                    val estContValue =
                      if callAmountMaybe.isDefined || putAmountMaybe.isDefined then
                        estimator(fac.toIndexedSeq)
                      else 0.0 // dummy value that won't be used
                    (callAmountMaybe, putAmountMaybe) match
                      case (Some(c), None) if c.toDouble < estContValue =>
                        Some((t, c, true))
                      case (None, Some(p)) if p.toDouble > estContValue =>
                        Some((t, p, false))
                      case (Some(c), Some(p)) =>
                        if c.toDouble < estContValue then Some((t, c, true))
                        else if p.toDouble > estContValue then Some((t, p, false))
                        else None
                      case _ => None

          earlyExerciseTimeAndAmountMaybe.fold(
            (
              profile.cashflows.map(_(1)).toList.sum,
              Counter.empty[T, Int],
              Counter.empty[T, Int]
            )
          )((t, amount, isCall) =>
            val cashflowsUntil = profile.cashflows.filter(_(0) <= t).map(_(1)).toList.sum
            (
              amount + cashflowsUntil,
              if isCall then Counter(t -> 1) else Counter.empty[T, Int],
              if isCall then Counter.empty[T, Int] else Counter(t -> 1)
            )
          )
        )
      }
      (pv, nCalls, nPuts) = tuple // why can't this be done in one line?
    yield FairValueResult(
      pv / nSims,
      (nCalls |+| Counter(spec0.callTimes.toList.map(_ -> 0)*)).toMap.map((k, n) =>
        k -> n.toDouble / nSims
      ),
      (nPuts |+| Counter(spec0.putTimes.toList.map(_ -> 0)*)).toMap.map((k, n) =>
        k -> n.toDouble / nSims
      )
    )

  private def eval[A](
      dsl: Dsl[T],
      sim: Simulation.Realization[T],
      rv: dsl.RV[A]
  ): Either[Error, A] =
    rv.foldMap(toProfileWriter(dsl, sim)).value

  private def profile[A](
      dsl: Dsl[T],
      sim: Simulation.Realization[T],
      rv: dsl.RV[A]
  ): Either[Error, Profile[T]] =
    rv.foldMap(toProfileWriter(dsl, sim)).written.map(_.build)

  private def spec[A](dsl: Dsl[T])(rv: dsl.RV[A]): Either[Error, Simulation.Spec[T]] =
    rv.foldMap(toSpecWriter(dsl)).written.map(_.build)

  private def barriers[A](dsl: Dsl[T])(rv: dsl.RV[A]): List[dsl.Barrier] =
    rv.foldMap(toBarrierWriter(dsl)).run(0)

  extension [A](l: List[A])
    def zipWithNext: List[(A, Option[A])] =
      l.zip(l.tail.map(_.some) ::: List(none[A]))

  private def lsmEstimators[A](
      dsl: Dsl[T],
      simulator: Simulator[T],
      nSims: Int,
      offset: Int = 0,
      rv: dsl.RV[A]
  )(using TimeLike[T]): Either[derifree.Error, Map[T, Lsm.Estimator]] =
    val lsm0 = Lsm.fromPoly(3)
    for {
      spec0 <- spec(dsl)(rv)
      earlyExTimes = (spec0.callTimes union spec0.putTimes).toList.sorted
      facRvs <- factorRvs(dsl, earlyExTimes, simulator.refTime, rv)
      combinedSpec <- spec(dsl)(facRvs.sequence *> rv)
      sims0 <- simulator(combinedSpec, offset)
      sims = sims0.take(nSims).toList
      profiles <- sims.traverse(sim => profile(dsl, sim, rv))
      estimators <- (earlyExTimes.zipWithNext zip facRvs).reverse
        .foldLeftM((List.fill(nSims)(PV(0.0)), Map.empty[T, Lsm.Estimator])) {
          case ((futCFs, estimators), ((earlyExTime, nextEarlyExTimeMaybe), factorRv)) =>
            val rowsE = (sims zip profiles zip futCFs).traverse {
              case ((sim, profile), futCf) =>
                eval(dsl, sim, factorRv).map: factors =>
                  val contValue = profile.cashflows
                    .filter((t, _) => t > earlyExTime && nextEarlyExTimeMaybe.forall(t < _))
                    .map(_(1))
                    .sum + futCf
                  val callAmountMaybe = profile.callAmounts.get(earlyExTime)
                  val putAmountMaybe = profile.putAmounts.get(earlyExTime)
                  (factors, contValue, callAmountMaybe, putAmountMaybe)
            }
            val estimatorE = rowsE.flatMap(rows =>
              val filteredRows = rows
                .filter((_, _, callAmountMaybe, putAmountMaybe) =>
                  callAmountMaybe.isDefined || putAmountMaybe.isDefined
                )
                .map((factors, contValue, _, _) => (factors, contValue.toDouble))
              // TODO: 1% is an arbitrary cutoff
              if (
                filteredRows.nonEmpty && filteredRows.length > math
                  .max(0.01 * nSims, filteredRows.head(0).length)
              )
              then lsm0.continuationValueEstimator(filteredRows).map(_.some)
              else Right(None) // too few relevant paths for least-squares
            )
            val nextFutCfs = (estimatorE, rowsE).mapN((estimatorMaybe, rows) =>
              estimatorMaybe match
                case None => rows.map((_, futCf, _, _) => futCf)
                case Some(estimator) =>
                  rows.map((factors, futCf, callAmount, putAmount) =>
                    val estContValue = estimator(factors.toIndexedSeq)
                    (callAmount, putAmount) match
                      case (Some(c), None) if c.toDouble < estContValue => c
                      case (None, Some(p)) if p.toDouble > estContValue => p
                      case (Some(c), Some(p))                           =>
                        // caller takes precedence
                        if c.toDouble < estContValue then c
                        else if p.toDouble > estContValue then p
                        else futCf
                      case _ => futCf
                  )
            )
            (nextFutCfs, estimatorE).mapN((futCfs, estimatorMaybe) =>
              (
                futCfs,
                estimatorMaybe.fold(estimators)(est => estimators + (earlyExTime -> est))
              )
            )
        }
        .map(_(1))
    } yield estimators

  // Factors for Longstaff-Schwartz regression
  // TODO: for stochastic vols or rates we should include vols and discount factors
  private def factorRvs[A](dsl: Dsl[T], times: List[T], refTime: T, rv: dsl.RV[A])(using
      TimeLike[T]
  ): Either[Error, List[dsl.RV[List[Double]]]] =
    for
      spec0 <- spec(dsl)(rv)
      barriers0 = barriers(dsl)(rv)
      udls = spec0.spotObs.keys.toList.sorted
    yield times.map: t =>
      import dsl.*
      for
        spots0 <- udls.traverse(udl => spot(udl, refTime))
        spots <- udls.traverse(udl => spot(udl, t))
        hitProbs <- barriers0.traverse: barrier =>
          barrier match
            case Barrier.Continuous(direction, levels, from, to, policy) =>
              if (from >= t) then pure(0.0)
              else hitProb(Barrier.Continuous(direction, levels, from, min(t, to), policy))
            case Barrier.Discrete(direction, levels, policy) =>
              val filteredObs =
                levels.map((udl, obs) => udl -> obs.filter((tObs, _) => tObs <= t))
              hitProb(Barrier.Discrete(direction, filteredObs, policy))
      yield (spots zip spots0).map(_ / _ - 1) ++ hitProbs

  protected def toSpecWriter(dsl: Dsl[T]): dsl.RVA ~> Writer[SpecBuilder[T], _]

  protected def toProfileWriter(
      dsl: Dsl[T],
      sim: Simulation.Realization[T]
  ): dsl.RVA ~> WriterT[Either[Error, _], ProfileBuilder[T], _]

  protected def toBarrierWriter(dsl: Dsl[T]): dsl.RVA ~> (Writer[List[dsl.Barrier], _])

private[derifree] object Compiler:

  case class Profile[T](
      cashflows: List[(T, PV)],
      callAmounts: Map[T, PV],
      putAmounts: Map[T, PV]
  )

  case class ProfileBuilder[T](
      cashflows: Chain[(T, PV)],
      callAmounts: Chain[(T, PV)],
      putAmounts: Chain[(T, PV)]
  ):
    def build: Profile[T] =
      Profile(cashflows.toList, callAmounts.iterator.toMap, putAmounts.iterator.toMap)

  object ProfileBuilder:

    def cashflow[T](time: T, amount: PV) =
      ProfileBuilder[T](Chain.one((time, amount)), Chain.empty, Chain.empty)

    def callAmount[T](time: T, amount: PV) =
      ProfileBuilder[T](Chain.empty, Chain.one((time, amount)), Chain.empty)

    def putAmount[T](time: T, amount: PV) =
      ProfileBuilder[T](Chain.empty, Chain.empty, Chain.one((time, amount)))

  given [T]: Monoid[ProfileBuilder[T]] = new Monoid[ProfileBuilder[T]]:
    def empty: ProfileBuilder[T] = ProfileBuilder[T](Chain.empty, Chain.empty, Chain.empty)
    def combine(x: ProfileBuilder[T], y: ProfileBuilder[T]): ProfileBuilder[T] =
      ProfileBuilder[T](
        x.cashflows |+| y.cashflows,
        x.callAmounts |+| y.callAmounts,
        x.putAmounts |+| y.putAmounts
      )

  case class SpecBuilder[T](
      spotObs: Chain[(String, T)],
      discountObs: Chain[(T, Ccy)],
      putTimes: Chain[T],
      callTimes: Chain[T]
  ):
    def build: Either[Error, Simulation.Spec[T]] =
      discountObs.map(_(1)).toList.distinct match {
        case ccy :: Nil =>
          Right(
            Simulation.Spec[T](
              ccy,
              spotObs.toList.groupBy(_(0)).map((udl, a) => udl -> a.map(_(1)).toSet),
              discountObs.map(_(0)).iterator.toSet,
              putTimes.iterator.toSet,
              callTimes.iterator.toSet
            )
          )
        case Nil  => Left(Error.AmbiguousCcy("no currency found"))
        case ccys => Left(Error.AmbiguousCcy(s"multiple ccys found: $ccys"))
      }

  object SpecBuilder:

    def make[T](
        spotObs: Chain[(String, T)] = Chain.empty,
        discountObs: Chain[(T, Ccy)] = Chain.empty,
        putTimes: Chain[T] = Chain.empty,
        callTimes: Chain[T] = Chain.empty
    ): SpecBuilder[T] = SpecBuilder(spotObs, discountObs, putTimes, callTimes)

    def spotObs[T](ticker: String, times: Seq[T]) =
      make[T](spotObs = Chain.fromSeq(times.map(ticker -> _)))

    def spotObs[T](ticker: String, time: T) = make[T](
      spotObs = Chain.one((ticker -> time))
    )

    def discountObs[T](time: T, ccy: Ccy) = make(discountObs = Chain.one(time -> ccy))

    def putTime[T](time: T) = make(putTimes = Chain.one(time))

    def callTime[T](time: T) = make(callTimes = Chain.one(time))

    given [T]: Monoid[SpecBuilder[T]] = new Monoid[SpecBuilder[T]]:
      def empty: SpecBuilder[T] = make()
      def combine(x: SpecBuilder[T], y: SpecBuilder[T]): SpecBuilder[T] =
        make(
          x.spotObs |+| y.spotObs,
          x.discountObs |+| y.discountObs,
          x.putTimes |+| y.putTimes,
          x.callTimes |+| y.callTimes
        )

  enum Error(message: String) extends derifree.Error(message):
    case Generic(message: String) extends Error(message)
    case AmbiguousCcy(message: String) extends Error(message)

  // The implementation uses mutable state for performance,
  // but this is never observable by clients.
  def apply[T: TimeLike]: Compiler[T] =
    new Compiler[T]:

      private val contBarrierObsIndicesCache =
        scala.collection.mutable.HashMap.empty[(T, T), Either[Error, Array[Int]]]

      private def mkContBarrierObsTimes(from: T, to: T): Set[T] =
        Set(from, to) |+| TimeLike[T].dailyStepsBetween(from, to).toSet

      protected def toBarrierWriter(dsl: Dsl[T]): dsl.RVA ~> Writer[List[dsl.Barrier], _] =
        new (dsl.RVA ~> Writer[List[dsl.Barrier], _]):
          def apply[A](fa: dsl.RVA[A]): Writer[List[dsl.Barrier], A] = fa match
            case dsl.HitProb(barrier)  => Writer(List(barrier), 0.5)
            case dsl.Cashflow(_, _, _) => Writer.value(PV(1.0))
            case dsl.Spot(_, _)        => Writer.value(1.0)
            case dsl.Callable(_, _, _) => Writer.value(())
            case dsl.Puttable(_, _, _) => Writer.value(())

      def toSpecWriter(dsl: Dsl[T]): dsl.RVA ~> Writer[SpecBuilder[T], _] =
        new (dsl.RVA ~> Writer[SpecBuilder[T], _]):
          def apply[A](fa: dsl.RVA[A]): Writer[SpecBuilder[T], A] = fa match
            case dsl.Spot(ticker, time) =>
              Writer(SpecBuilder.spotObs(ticker, time), 1.0)

            case dsl.Cashflow(amount, ccy, time) =>
              Writer(SpecBuilder.discountObs(time, ccy), PV(1.0))

            case dsl.HitProb(dsl.Barrier.Discrete(_, levels, _)) =>
              Writer(
                levels.toList.foldMap((ticker, obs) =>
                  SpecBuilder.spotObs(ticker, obs.map(_(0)))
                ),
                0.5
              )

            case dsl.HitProb(dsl.Barrier.Continuous(_, levels, from, to, _)) =>
              Writer(
                levels.keys.toList.foldMap(ticker =>
                  SpecBuilder.spotObs(ticker, mkContBarrierObsTimes(from, to).toSeq)
                ),
                0.5
              )

            case dsl.Puttable(_, ccy, time) =>
              Writer(
                SpecBuilder.putTime(time) |+| SpecBuilder.discountObs(time, ccy),
                ()
              )

            case dsl.Callable(_, ccy, time) =>
              Writer(SpecBuilder.callTime(time) |+| SpecBuilder.discountObs(time, ccy), ())

      def toProfileWriter(
          dsl: Dsl[T],
          sim: Simulation.Realization[T]
      ): dsl.RVA ~> WriterT[Either[Error, _], ProfileBuilder[T], _] =
        new (dsl.RVA ~> WriterT[Either[Error, _], ProfileBuilder[T], _]):
          def apply[A](fa: dsl.RVA[A]): WriterT[Either[Error, _], ProfileBuilder[T], A] =
            fa match
              case dsl.Spot(ticker, time) =>
                WriterT.liftF(
                  Either
                    .fromOption(
                      sim.timeIndex.get(time),
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
                )

              case dsl.Cashflow(amount, _, time) =>
                WriterT(
                  Either
                    .fromOption(
                      sim.timeIndex
                        .get(time)
                        .map(i => PV(amount * sim.discounts(i))),
                      Error.Generic(s"missing time index for discount time $time")
                    )
                    .map(pv => (ProfileBuilder.cashflow(time, pv), pv))
                )

              case dsl.HitProb(dsl.Barrier.Discrete(direction, levels, policy)) =>
                WriterT.liftF(
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
                  ).mapN: (spots, jumps, vols) =>
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
                )

              case dsl.HitProb(dsl.Barrier.Continuous(direction, levels, from, to, policy)) =>
                val e = Either.raiseUnless(levels.size <= 1 || policy == dsl.Barrier.Policy.Or)(
                  Error.Generic("Only `or` barriers are supported for continuous monitoring")
                ) *>
                  (
                    contBarrierObsIndicesCache.getOrElseUpdate(
                      (from, to),
                      Either.fromOption(
                        sim.spotObsTimes.values
                          .map(_.filter(t => t >= from && t <= to))
                          .flatten
                          .toSet
                          .toList
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
                  ).mapN: (spotObsIdxs, spots, logSpots, jumps, vols) =>
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
                WriterT.liftF(e)

              case dsl.Puttable(amount, _, time) =>
                amount.fold(WriterT.liftF(Either.right[Error, Unit](())))(amount =>
                  WriterT(
                    Either
                      .fromOption(
                        sim.timeIndex.get(time).map(i => PV(amount * sim.discounts(i))),
                        Error.Generic(s"missing time index for discount time $time")
                      )
                      .map(pv => (ProfileBuilder.putAmount(time, pv), ()))
                  )
                )

              case dsl.Callable(amount, _, time) =>
                amount.fold(WriterT.liftF(Either.right[Error, Unit](())))(amount =>
                  WriterT(
                    Either
                      .fromOption(
                        sim.timeIndex.get(time).map(i => PV(amount * sim.discounts(i))),
                        Error.Generic(s"missing time index for discount time $time")
                      )
                      .map(pv => (ProfileBuilder.callAmount(time, pv), ()))
                  )
                )
