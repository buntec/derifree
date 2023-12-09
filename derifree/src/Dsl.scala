package derifree

import cats.Monoid
import cats.free.Free
import cats.free.Free.*

trait Dsl[T]:
  self =>

  import Barrier.*
  enum Barrier:
    case Continuous(
        direction: Direction,
        levels: Map[String, Double],
        from: T,
        to: T,
        policy: Policy = Policy.Or
    )
    case Discrete(
        direction: Direction,
        levels: Map[String, List[(T, Double)]],
        policy: Policy = Policy.Or
    )

  object Barrier:
    enum Direction:
      case Down, Up

    enum Policy:
      case And, Or

  sealed trait RVA[A]
  case class Spot(ticker: String, time: T) extends RVA[Double]
  case class Cashflow(amount: Double, time: T) extends RVA[PV]
  case class HitProb(barrier: Barrier) extends RVA[Double]

  /** A random variable measurable with respect to a simulation. */
  type RV[A] = Free[RVA, A]

  def spot(ticker: String, time: T): RV[Double] =
    liftF[RVA, Double](Spot(ticker, time))

  def cashflow(amount: Double, time: T): RV[PV] =
    liftF[RVA, PV](Cashflow(amount, time))

  def hitProb(barrier: Barrier): RV[Double] =
    liftF[RVA, Double](HitProb(barrier))

  def survivalProb(barrier: Barrier): RV[Double] =
    liftF[RVA, Double](HitProb(barrier)).map(1 - _)

  extension [A](rva: RV[A])
    def mean(
        simulator: Simulator[T]
    )(using TimeLike[T], Fractional[A], Monoid[A]): Either[derifree.Error, A] =
      Compiler[T].mean(self, simulator)(rva)

object Dsl:

  def apply[T]: Dsl[T] = new Dsl[T] {}
