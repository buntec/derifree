package derifree

import cats.syntax.all.*
import cats.~>
import cats.data.Writer
import cats.Monoid

object Compiler:

  case class Log[T](spotObservations: Map[String, List[T]], payments: List[T])

  object Log:
    def spotObs[T](ticker: String, times: List[T]) =
      Log[T](Map(ticker -> times), Nil)
    def payment[T](time: T) = Log[T](Map.empty, List(time))

    given monoid[T]: Monoid[Log[T]] = ???

  case class Simulation[T](
      timeIndex: Map[T, Int],
      deltaTs: IArray[Double],
      spots: Map[String, IArray[Double]],
      jumps: Map[String, IArray[Double]],
      vols: Map[String, IArray[Double]],
      discounts: IArray[Double]
  )

  def run[T: TimeOrder](
      dsl: Dsl[T],
      simulator: Log[T] => Either[String, Seq[Simulation[T]]]
  )(
      rv: dsl.RV[Double]
  ): Either[String, Double] = {
    val log = rv.foldMap(toLog(dsl)).run(0)
    simulator(log).flatMap(sims =>
      sims
        .traverse(sim => rv.foldMap(toValue(dsl, sim)))
        .map(_.sum / sims.length)
    )
  }

  private def toLog[T: TimeOrder](
      dsl: Dsl[T]
  ): dsl.RVA ~> ([A] =>> Writer[Log[T], A]) =
    new (dsl.RVA ~> ([A] =>> Writer[Log[T], A])):
      def apply[A](fa: dsl.RVA[A]): Writer[Log[T], A] = fa match
        case dsl.Spot(ticker, time) =>
          Writer(Log.spotObs(ticker, List(time)), 1.0)

        case dsl.Cashflow(amount, time) => Writer(Log.payment(time), PV(1.0))

        case dsl.HitProb(ticker, level, from, to, fromBelow) =>
          Writer(Log.spotObs(ticker, List(from, to)), 0.5)

  private def toValue[T: TimeOrder](
      dsl: Dsl[T],
      sim: Simulation[T]
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

        case dsl.HitProb(ticker, level, from, to, fromBelow) =>
          (
            Either.fromOption(sim.timeIndex.get(from), "missing `from` index"),
            Either.fromOption(sim.timeIndex.get(to), "missing `to` index"),
            Either.fromOption(
              sim.spots.get(ticker),
              s"missing spots for $ticker"
            ),
            Either.fromOption(
              sim.jumps.get(ticker),
              s"missing jumps for $ticker"
            ),
            Either.fromOption(
              sim.vols.get(ticker),
              s"missing vols for $ticker"
            ),
          ).mapN { case (i1, i2, spots, jumps, vols) =>
            var p = 1.0
            var i = i1
            while (i < i2) {
              val dt = sim.deltaTs(i)
              val s0 = spots(i)
              val s1 = spots(i + 1)
              val vol = vols(i)
              val sign = if fromBelow then -1.0 else 1.0
              if (sign * (s0 - level) <= 0 || sign * (s1 - level) <= 0) {
                p = 0.0
              } else {
                val u =
                  math.log(level / s0) * math.log(level / s1) / (dt * vol * vol)
                p *= math.exp(sign * 2.0 * u)
              }
              i += 1
            }
            1 - p
          }
