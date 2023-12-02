package derifree

import cats.syntax.all.*
import cats.~>
import cats.data.Writer

object Compiler:

  def run[T: TimeOrder](
      dsl: Dsl[T],
      simulator: Simulation.Spec[T] => Either[String, Seq[Simulation.Realization[T]]]
  )(
      rv: dsl.RV[Double]
  ): Either[String, Double] = {
    val log = rv.foldMap(toSpec(dsl)).run(0)
    simulator(log).flatMap(sims =>
      val (sumE, n) = sims.foldMapM(sim => (rv.foldMap(toValue(dsl, sim)), 1))
      sumE.map(_ / n)
    )
  }

  private def toSpec[T: TimeOrder](
      dsl: Dsl[T]
  ): dsl.RVA ~> ([A] =>> Writer[Simulation.Spec[T], A]) =
    new (dsl.RVA ~> ([A] =>> Writer[Simulation.Spec[T], A])):
      def apply[A](fa: dsl.RVA[A]): Writer[Simulation.Spec[T], A] = fa match
        case dsl.Spot(ticker, time) =>
          Writer(Simulation.Spec.spotObs(ticker, Set(time)), 1.0)

        case dsl.Cashflow(amount, time) => Writer(Simulation.Spec.discountObs(time), PV(1.0))

        case dsl.HitProb(dsl.Barrier.Discrete(_, _, levels)) =>
          Writer(levels.toList.foldMap((ticker, obs) => Simulation.Spec.spotObs(ticker, obs.map(_(0)).toSet)), 0.5)

        case dsl.HitProb(dsl.Barrier.Continuous(_, _, levels, from, to)) =>
          Writer(
            levels.keys.toList.foldMap(ticker =>
              Simulation.Spec.spotObs(ticker, Set(from, to) <+> TimeOrder[T].dailyStepsBetween(from, to).toSet)
            ),
            0.5
          )

  private def toValue[T: TimeOrder](
      dsl: Dsl[T],
      sim: Simulation.Realization[T]
  ): dsl.RVA ~> ([A] =>> Either[String, A]) =
    new (dsl.RVA ~> ([A] =>> Either[String, A])):
      def apply[A](fa: dsl.RVA[A]): Either[String, A] = fa match
        case dsl.Spot(ticker, time) =>
          Either.fromOption(
            sim.timeIndex
              .get(time)
              .flatMap(t => sim.spots.get(ticker).map(_(t))),
            s"missing spot for $ticker"
          )
        case dsl.Cashflow(amount, time) =>
          Either.fromOption(
            sim.timeIndex.get(time).map(i => PV(amount * sim.discounts(i))),
            s"missing time index for $time"
          )

        case dsl.HitProb(dsl.Barrier.Discrete(direction, policy, levels)) =>
          (
            levels.keys.toList
              .traverse(ticker =>
                Either.fromOption(sim.spots.get(ticker), s"missing spots for $ticker").tupleLeft(ticker)
              )
              .map(_.toMap),
            levels.keys.toList
              .traverse(ticker =>
                Either.fromOption(sim.jumps.get(ticker), s"missing jumps for $ticker").tupleLeft(ticker)
              )
              .map(_.toMap),
            levels.keys.toList
              .traverse(ticker =>
                Either.fromOption(sim.vols.get(ticker), s"missing vols for $ticker").tupleLeft(ticker)
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
                      .filter((time, level) => sign * (spots(ticker)(sim.timeIndex(time)) - level) >= 0)
                      .map(_(0))
                      .toSet
                  )
                  .values
                  .reduce(_ intersect _)
                if hitTimes.nonEmpty then 1.0 else 0.0
              case dsl.Barrier.Policy.Or =>
                val hit = levels.exists((ticker, obs) =>
                  obs.exists((time, level) => sign * (spots(ticker)(sim.timeIndex(time)) - level) >= 0)
                )
                if hit then 1.0 else 0.0

          }

        case dsl.HitProb(dsl.Barrier.Continuous(direction, policy, levels, from, to)) =>
          Either.raiseUnless(policy == dsl.Barrier.Policy.Or)(
            "Only `or` barriers are supported for continuous monitoring"
          ) *>
            (
              Either.fromOption(sim.timeIndex.get(from), "missing `from` index"),
              Either.fromOption(sim.timeIndex.get(to), "missing `to` index"),
              levels.keys.toList
                .traverse(ticker =>
                  Either.fromOption(sim.spots.get(ticker), s"missing spots for $ticker").tupleLeft(ticker)
                )
                .map(_.toMap),
              levels.keys.toList
                .traverse(ticker =>
                  Either.fromOption(sim.jumps.get(ticker), s"missing jumps for $ticker").tupleLeft(ticker)
                )
                .map(_.toMap),
              levels.keys.toList
                .traverse(ticker =>
                  Either.fromOption(sim.vols.get(ticker), s"missing vols for $ticker").tupleLeft(ticker)
                )
                .map(_.toMap),
            ).mapN { case (i1, i2, spots, jumps, vols) =>
              var p = 1.0 // survival probability
              val sign = direction match {
                case dsl.Barrier.Direction.Up   => -1
                case dsl.Barrier.Direction.Down => 1
              }
              levels.foreach: (ticker, level) =>
                val s = spots(ticker)
                val v = vols(ticker)
                val jmp = jumps(ticker)
                var i = i1
                while (i < i2) {
                  val dt = sim.deltaTs(i)
                  val s0 = s(i)
                  // we want s1 = S(t_{i+1}-) (limit from the left) and use Jump(t_i) = S(t_i) - S(t_i-)
                  val s1 = s(i + 1) - jmp(i + 1)
                  val s1r = s(i + 1)
                  val vol = v(i)
                  if (sign * (s0 - level) <= 0 || sign * (s1 - level) <= 0 || sign * (s1r - level) <= 0) {
                    p = 0.0
                  } else {
                    val u = math.log(level / s0) * math.log(level / s1) / (dt * vol * vol)
                    p *= math.exp(sign * 2.0 * u)
                  }
                  i += 1
                }
              1 - p
            }
