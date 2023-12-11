package derifree

import derifree.syntax.given
import cats.data.Writer
import cats.kernel.Monoid
import cats.syntax.all.*
import cats.~>
import org.apache.commons.math3.util.{FastMath => math}

import scala.math.Fractional.Implicits.*

import Compiler.*
import cats.free.Free
import cats.data.WriterT

private[derifree] sealed trait Compiler[T]:

  def mean[A: Fractional: Monoid](
      dsl: Dsl[T],
      simulator: Simulator[T],
      nSims: Int,
      offset: Int = 0,
      rv: dsl.RV[A]
  ): Either[derifree.Error, A] =
    simulator(spec(dsl)(rv), nSims, offset).flatMap(sims =>
      val (sumE, n) = sims.foldMapM(sim => (eval(dsl, sim, rv), Fractional[A].one))
      sumE.map(_ / n)
    )

  def eval[A](dsl: Dsl[T], sim: Simulation.Realization[T], rv: dsl.RV[A]): Either[Error, A] = 
    rv.foldMap(toValue(dsl, sim)).value

  def spec[A](dsl: Dsl[T])(rv: dsl.RV[A]): Simulation.Spec[T] =
    rv.foldMap(toSpec(dsl)).run(0)

  def lsm[A: Fractional: Monoid](
      dsl: Dsl[T],
      simulator: Simulator[T],
      nSims: Int,
      offset: Int = 0,
      rv: dsl.RV[A]
  )(using TimeLike[T]) = simulator(spec(dsl)(rv), nSims, offset).flatMap: sims =>
        val spec0 = spec(dsl)(rv)
        val callDates = spec0.callDates.toList.sorted
        val facRvs = factorRvs(dsl)(rv)
        val blah = (callDates zip facRvs).reverse.foldLeft(Option.empty[List[Double]]){ case (futCFs, (callDate, factorRv)) =>
          sims.map: sim =>
            for
              profile <- rv.foldMap(toValue(dsl, sim)).written
              factors <- eval(dsl, sim, factorRv)
            yield 
              val cfs = profile.cashflows.filter((t, _) => t > callDate)
              ???
          ???
        }
        ???


  protected def toSpec(dsl: Dsl[T]): dsl.RVA ~> ([A] =>> Writer[Simulation.Spec[T], A])

  protected def toValue(
      dsl: Dsl[T],
      sim: Simulation.Realization[T]
  ): dsl.RVA ~> ([A] =>> WriterT[[B] =>> Either[Error, B], Profile[T], A])

  protected def toBarriers(dsl: Dsl[T]): dsl.RVA ~> ([A] =>> Writer[List[dsl.Barrier], A])

  protected def factorRvs[A](dsl: Dsl[T])(rv: dsl.RV[A])(using
      TimeLike[T]
  ): List[dsl.RV[List[Double]]] =
    val spec0 = spec(dsl)(rv)
    val barriers = rv.foldMap(toBarriers(dsl)).run(0)
    val udls = spec0.spotObs.keySet.toList.sorted
    val callDates = spec0.callDates.toList.sorted
    callDates.map: t =>
      import dsl.*
      for
        spots <- udls.traverse(udl => spot(udl, t))
        hitProbs <- barriers.traverse: barrier =>
          barrier match
            case Barrier.Continuous(direction, levels, from, to, policy) =>
              hitProb(Barrier.Continuous(direction, levels, from, List(t, to).min))
            case Barrier.Discrete(direction, levels, policy) =>
              hitProb(Barrier.Discrete(direction, levels, policy))
      yield (spots ++ hitProbs)

private[derifree] object Compiler:

  case class Profile[T](cashflows: List[(T, Double)], callAmounts: List[(T, Double)])

  given profileMonoid[T]: Monoid[Profile[T]] = new Monoid[Profile[T]]:
    def empty: Profile[T] = Profile[T](Nil, Nil)
    def combine(x: Profile[T], y: Profile[T]): Profile[T] =
      Profile[T](x.cashflows ++ y.cashflows, x.callAmounts ++ y.callAmounts)

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

      protected def toBarriers(dsl: Dsl[T]): dsl.RVA ~> ([A] =>> Writer[List[dsl.Barrier], A]) =
        new (dsl.RVA ~> ([A] =>> Writer[List[dsl.Barrier], A])):
          def apply[A](fa: dsl.RVA[A]): Writer[List[dsl.Barrier], A] = fa match
            case dsl.HitProb(barrier)   => Writer(List(barrier), 0.5)
            case dsl.Cashflow(_, _)     => Writer.value(PV(1.0))
            case dsl.Spot(_, _)         => Writer.value(1.0)
            case dsl.Callable(_, _)     => Writer.value(())
            case dsl.Exerciseable(_, _) => Writer.value(())

      def toSpec(dsl: Dsl[T]): dsl.RVA ~> ([A] =>> Writer[Simulation.Spec[T], A]) =
        new (dsl.RVA ~> ([A] =>> Writer[Simulation.Spec[T], A])):
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
                  Simulation.Spec.spotObs(
                    ticker,
                    getContBarrierObsTimes(from, to)
                  )
                ),
                0.5
              )

            case dsl.Exerciseable(_, time) =>
              Writer(Simulation.Spec.exerciseDate(time), ())

            case dsl.Callable(_, time) =>
              Writer(Simulation.Spec.callDate(time), ())

      def toValue(
          dsl: Dsl[T],
          sim: Simulation.Realization[T]
      ): dsl.RVA ~> ([A] =>> WriterT[[B] =>> Either[Error, B], Profile[T], A]) =
        new (dsl.RVA ~> ([A] =>> WriterT[[B] =>> Either[Error, B], Profile[T], A])):
          def apply[A](fa: dsl.RVA[A]): WriterT[[B] =>> Either[Error, B], Profile[T], A] =
            fa match
              case dsl.Spot(ticker, time) =>
                WriterT.liftF(
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
                )

              case dsl.Cashflow(amount, time) =>
                WriterT.putT(
                  Either.fromOption(
                    sim.timeIndex.get(time).map(i => PV(amount * sim.discounts(i))),
                    Error.Generic(s"missing time index for $time")
                  )
                )(Profile[T](List((time, amount)), Nil))

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

                })

              case dsl.HitProb(dsl.Barrier.Continuous(direction, levels, from, to, policy)) =>
                WriterT.liftF(
                Either.raiseUnless(levels.size <= 1 || policy == dsl.Barrier.Policy.Or)(
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
                  })

              case dsl.Exerciseable(_, _) => WriterT.liftF(Right(()))

              case dsl.Callable(amount, time) => WriterT.put(())(Profile[T](Nil, List((time, amount))))
