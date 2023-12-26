package derifree

import alleycats.std.iterable.*
import cats.Monoid
import cats.data.Writer
import cats.data.WriterT
import cats.free.Free
import cats.syntax.all.*
import cats.~>
import derifree.syntax.given
import org.apache.commons.math3.util.{FastMath => math}

import Compiler.*
import cats.data.Chain

private[derifree] sealed trait Compiler[T]:

  def mean[A: Fractional: Monoid](
      dsl: Dsl[T],
      simulator: Simulator[T],
      nSims: Int,
      rv: dsl.RV[A]
  ): Either[derifree.Error, A] =
    simulator(spec(dsl)(rv), nSims, 0).flatMap: sims =>
      val (sumE, n) = (sims: Iterable[Simulation.Realization[T]]).foldMapM(sim =>
        (eval(dsl, sim, rv), Fractional[A].one)
      )
      sumE.map(a => Fractional[A].div(a, n))

  def fairValue[A](
      dsl: Dsl[T],
      simulator: Simulator[T],
      nSims: Int,
      rv: dsl.RV[A]
  )(using TimeLike[T]): Either[derifree.Error, PV] =
    val spec0 = spec(dsl)(rv)
    // println(s"spec: $spec0")
    if spec0.callDates.nonEmpty || spec0.exerciseDates.nonEmpty then
      fairValueByLsm(dsl, simulator, nSims / 8, nSims, rv).map: (pv, _) =>
        // println(s"early term prob: ${probs.values.sum}")
        pv
    else
      simulator(spec0, nSims, 0).flatMap: sims =>
        val (sumE, n) =
          (sims: Iterable[Simulation.Realization[T]]).foldMapM(sim =>
            (profile(dsl, sim, rv).map(_.cashflows.map(_(1)).toList.sum), 1)
          )
        sumE.map(_ / n)

  def earlyTerminationProbabilities[A](
      dsl: Dsl[T],
      simulator: Simulator[T],
      nSims: Int,
      rv: dsl.RV[A]
  )(using TimeLike[T]): Either[derifree.Error, Map[T, Double]] =
    fairValueByLsm(dsl, simulator, nSims / 8, nSims, rv).map(_(1))

  // returns pv + call/put probabilities
  private def fairValueByLsm[A](
      dsl: Dsl[T],
      simulator: Simulator[T],
      nSimsLsm: Int,
      nSims: Int,
      rv: dsl.RV[A]
  )(using TimeLike[T]): Either[derifree.Error, (PV, Map[T, Double])] =
    val estimatorsMapE = toLsmEstimator[A](dsl, simulator, nSimsLsm, 0, rv)
    val spec0 = spec(dsl)(rv)
    val etDates = (spec0.callDates union spec0.exerciseDates).toList.sorted
    val facRvs = factorRvs(dsl, etDates, simulator.refTime, rv)
    val combinedSpec = spec(dsl)(facRvs.sequence *> rv)
    // println(s"combined spec: $combinedSpec")
    val sumE = simulator(combinedSpec, nSims, nSimsLsm).flatMap: sims =>
      (sims: Iterable[Simulation.Realization[T]]).foldMapM: sim =>
        val factorsE = facRvs.traverse(rv => eval(dsl, sim, rv))
        val profileE = profile(dsl, sim, rv)
        (estimatorsMapE, factorsE, profileE).mapN((estimators, factors, profile) =>
          val earlyTerminationDateAndAmount =
            (etDates zip factors).collectFirstSome:
              case (t, fac) =>
                estimators
                  .get(t)
                  .flatMap: estimator =>
                    val callAmountMaybe = profile.callAmounts.find(_(0) == t).map(_(1))
                    val putAmountMaybe = profile.putAmounts.find(_(0) == t).map(_(1))
                    val estContValue = estimator(fac.toIndexedSeq)
                    (callAmountMaybe, putAmountMaybe) match
                      case (Some(c), None) if c.toDouble < estContValue =>
                        // println(s"calling: t=$t, pv=$c, cont value=$estContValue, factors=$fac")
                        Some((t, c))
                      case (None, Some(p)) if p.toDouble > estContValue =>
                        // println(s"putting: t=$t, pv=$p, cont value=$estContValue, factors=$fac")
                        Some((t, p))
                      case (Some(c), Some(p)) =>
                        if c.toDouble < estContValue then Some((t, c))
                        else if p.toDouble > estContValue then Some((t, p))
                        else None
                      case _ => None

          earlyTerminationDateAndAmount.fold(
            profile.cashflows.map(_(1)).toList.sum -> Counter.empty[T, Int]
          )((t, amount) =>
            val cashflowsUntil = profile.cashflows.filter(_(0) <= t).map(_(1)).toList.sum
            // println(s"et: t=$t, pv=$amount, cfs until=$cashflowsUntil")
            amount + cashflowsUntil -> Counter(t -> 1)
          )
        )
    sumE.map((pv, nEarlyTerminations) =>
      pv / nSims -> (nEarlyTerminations |+| Counter(etDates.map(_ -> 0)*)).toMap.map((k, n) =>
        k -> n.toDouble / nSims
      )
    )

  private def eval[A](
      dsl: Dsl[T],
      sim: Simulation.Realization[T],
      rv: dsl.RV[A]
  ): Either[Error, A] =
    rv.foldMap(toValue(dsl, sim)).value

  private def profile[A](
      dsl: Dsl[T],
      sim: Simulation.Realization[T],
      rv: dsl.RV[A]
  ): Either[Error, Profile[T]] =
    rv.foldMap(toValue(dsl, sim)).written

  private def spec[A](dsl: Dsl[T])(rv: dsl.RV[A]): Simulation.Spec[T] =
    rv.foldMap(toSpec(dsl)).written

  private def toLsmEstimator[A](
      dsl: Dsl[T],
      simulator: Simulator[T],
      nSims: Int,
      offset: Int = 0,
      rv: dsl.RV[A]
  )(using TimeLike[T]): Either[derifree.Error, Map[T, Lsm.Estimator]] =
    val lsm0 = Lsm.fromPoly(3)
    val spec0 = spec(dsl)(rv)
    val etDates = (spec0.callDates union spec0.exerciseDates).toList.sorted
    val facRvs = factorRvs(dsl, etDates, simulator.refTime, rv)
    val combinedSpec = spec(dsl)(facRvs.sequence *> rv)
    simulator(combinedSpec, nSims, offset).flatMap: sims0 =>
      val sims = sims0.toList
      (etDates zip facRvs).reverse
        .foldLeftM((List.fill(nSims)(PV(0.0)), Map.empty[T, Lsm.Estimator])) {
          case ((futCFs, estimators), (etDate, factorRv)) =>
            val rowsE = (sims zip futCFs).traverse: (sim, futCf) =>
              for
                profile <- rv.foldMap(toValue(dsl, sim)).written
                factors <- eval(dsl, sim, factorRv)
              yield
                val nextEtDateMaybe = etDates.find(_ > etDate)
                val contValue = profile.cashflows
                  .filter((t, _) => t > etDate && nextEtDateMaybe.forall(t < _))
                  .map(_(1))
                  .toIterable
                  .sum + futCf
                val callAmountMaybe = profile.callAmounts.find((t, _) => t == etDate).map(_(1))
                val putAmountMaybe = profile.putAmounts.find((t, _) => t == etDate).map(_(1))
                (factors, contValue, callAmountMaybe, putAmountMaybe)
            val estimatorE = rowsE.flatMap(rows =>
              val filteredRows = rows
                .filter((_, _, callAmountMaybe, putAmountMaybe) =>
                  callAmountMaybe.isDefined || putAmountMaybe.isDefined
                )
                .map((factors, contValue, _, _) => (factors, contValue.toDouble))
              if (filteredRows.nonEmpty && filteredRows.length > filteredRows.head(0).length)
              then lsm0.toContValueEstimator(filteredRows).map(_.some)
              else Right(None) // too few relevant paths for least-squares
            )
            val nextFutCfs = (estimatorE, rowsE).mapN((estimatorMaybe, rows) =>
              estimatorMaybe match
                case None => rows.map((_, futCf, _, _) => futCf)
                case Some(estimator) =>
                  rows.map((factors, futCf, callAmount, putAmount) =>
                    val estContValue = estimator(factors.toIndexedSeq)
                    // println(s"et: $etDate, factors: $factors, est cont value: $estContValue")
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
              (futCfs, estimatorMaybe.fold(estimators)(est => estimators + (etDate -> est)))
            )
        }
        .map(_(1))

  private def factorRvs[A](dsl: Dsl[T], times: List[T], refTime: T, rv: dsl.RV[A])(using
      TimeLike[T]
  ): List[dsl.RV[List[Double]]] =
    val spec0 = spec(dsl)(rv)
    val barriers = rv.foldMap(toBarriers(dsl)).run(0)
    val udls = spec0.spotObs.keySet.toList.sorted
    times.map: t =>
      import dsl.*
      for
        spots0 <- udls.traverse(udl => spot(udl, refTime))
        spots <- udls.traverse(udl => spot(udl, t))
        hitProbs <- barriers.traverse: barrier =>
          barrier match
            case Barrier.Continuous(direction, levels, from, to, policy) =>
              if (from >= t) then pure(0.0)
              else hitProb(Barrier.Continuous(direction, levels, from, min(t, to), policy))
            case Barrier.Discrete(direction, levels, policy) =>
              val filteredLevels =
                levels.map((udl, obs) => udl -> obs.filter((tObs, _) => tObs > t))
              hitProb(Barrier.Discrete(direction, filteredLevels, policy))
      yield (spots zip spots0).map(_ / _ - 1) ++ hitProbs

  protected def toSpec(dsl: Dsl[T]): dsl.RVA ~> Writer[Simulation.Spec[T], _]

  protected def toValue(
      dsl: Dsl[T],
      sim: Simulation.Realization[T]
  ): dsl.RVA ~> WriterT[Either[Error, _], Profile[T], _]

  protected def toBarriers(dsl: Dsl[T]): dsl.RVA ~> (Writer[List[dsl.Barrier], _])

private[derifree] object Compiler:

  case class Profile[T](
      cashflows: Chain[(T, PV)],
      callAmounts: Chain[(T, PV)],
      putAmounts: Chain[(T, PV)]
  )

  object Profile:
    def cashflow[T](time: T, amount: PV) =
      Profile[T](Chain((time, amount)), Chain.empty, Chain.empty)
    def callAmount[T](time: T, amount: PV) =
      Profile[T](Chain.empty, Chain((time, amount)), Chain.empty)
    def putAmount[T](time: T, amount: PV) =
      Profile[T](Chain.empty, Chain.empty, Chain((time, amount)))

  given [T]: Monoid[Profile[T]] = new Monoid[Profile[T]]:
    def empty: Profile[T] = Profile[T](Chain.empty, Chain.empty, Chain.empty)
    def combine(x: Profile[T], y: Profile[T]): Profile[T] =
      Profile[T](
        x.cashflows |+| y.cashflows,
        x.callAmounts |+| y.callAmounts,
        x.putAmounts |+| y.putAmounts
      )

  enum Error extends derifree.Error:
    case Generic(override val getMessage: String)

  // The implementation uses mutable state for performance,
  // but this is never observable by clients.
  def apply[T: TimeLike]: Compiler[T] =
    new Compiler[T]:

      private val contBarrierObsTimesCache =
        scala.collection.mutable.HashMap.empty[(T, T), Set[T]]

      private val contBarrierObsIndicesCache =
        scala.collection.mutable.HashMap.empty[(T, T), Either[Error, Array[Int]]]

      private def mkContBarrierObsTimes(from: T, to: T): Set[T] =
        Set(from, to) |+| TimeLike[T].dailyStepsBetween(from, to).toSet

      private def getContBarrierObsTimes(from: T, to: T): Set[T] =
        contBarrierObsTimesCache.getOrElseUpdate((from, to), mkContBarrierObsTimes(from, to))

      protected def toBarriers(dsl: Dsl[T]): dsl.RVA ~> Writer[List[dsl.Barrier], _] =
        new (dsl.RVA ~> Writer[List[dsl.Barrier], _]):
          def apply[A](fa: dsl.RVA[A]): Writer[List[dsl.Barrier], A] = fa match
            case dsl.HitProb(barrier) => Writer(List(barrier), 0.5)
            case dsl.Cashflow(_, _)   => Writer.value(PV(1.0))
            case dsl.Spot(_, _)       => Writer.value(1.0)
            case dsl.Callable(_, _)   => Writer.value(())
            case dsl.Puttable(_, _)   => Writer.value(())

      def toSpec(dsl: Dsl[T]): dsl.RVA ~> Writer[Simulation.Spec[T], _] =
        new (dsl.RVA ~> Writer[Simulation.Spec[T], _]):
          def apply[A](fa: dsl.RVA[A]): Writer[Simulation.Spec[T], A] = fa match
            case dsl.Spot(ticker, time) =>
              Writer(Simulation.Spec.spotObs(ticker, Set(time)), 1.0)

            case dsl.Cashflow(amount, time) =>
              Writer(Simulation.Spec.discountObs(time), PV(1.0))

            case dsl.HitProb(dsl.Barrier.Discrete(_, levels, _)) =>
              Writer(
                levels.toList.foldMap((ticker, obs) =>
                  Simulation.Spec.spotObs(ticker, obs.map(_(0)).toSet)
                ),
                0.5
              )

            case dsl.HitProb(dsl.Barrier.Continuous(_, levels, from, to, _)) =>
              Writer(
                levels.keys.toList.foldMap(ticker =>
                  Simulation.Spec.spotObs(ticker, getContBarrierObsTimes(from, to))
                ),
                0.5
              )

            case dsl.Puttable(_, time) =>
              Writer(
                Simulation.Spec.exerciseDate(time) |+| Simulation.Spec.discountObs(time),
                ()
              )

            case dsl.Callable(_, time) =>
              Writer(Simulation.Spec.callDate(time) |+| Simulation.Spec.discountObs(time), ())

      def toValue(
          dsl: Dsl[T],
          sim: Simulation.Realization[T]
      ): dsl.RVA ~> WriterT[Either[Error, _], Profile[T], _] =
        new (dsl.RVA ~> WriterT[Either[Error, _], Profile[T], _]):
          def apply[A](fa: dsl.RVA[A]): WriterT[Either[Error, _], Profile[T], A] =
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

              case dsl.Cashflow(amount, time) =>
                WriterT(
                  Either
                    .fromOption(
                      sim.timeIndex
                        .get(time)
                        .map(i =>
                          // println(s"df: ${sim.discounts(i)}")
                          PV(amount * sim.discounts(i))
                        ),
                      Error.Generic(s"missing time index for discount time $time")
                    )
                    .map(pv => (Profile.cashflow(time, pv), pv))
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

              case dsl.Puttable(amount, time) =>
                amount.fold(WriterT.liftF(Either.right[Error, Unit](())))(amount =>
                  WriterT(
                    Either
                      .fromOption(
                        sim.timeIndex.get(time).map(i => PV(amount * sim.discounts(i))),
                        Error.Generic(s"missing time index for discount time $time")
                      )
                      .map(pv => (Profile.putAmount(time, pv), ()))
                  )
                )

              case dsl.Callable(amount, time) =>
                amount.fold(WriterT.liftF(Either.right[Error, Unit](())))(amount =>
                  WriterT(
                    Either
                      .fromOption(
                        sim.timeIndex.get(time).map(i => PV(amount * sim.discounts(i))),
                        Error.Generic(s"missing time index for discount time $time")
                      )
                      .map(pv => (Profile.callAmount(time, pv), ()))
                  )
                )
