package derifree

import cats.free.Free
import cats.free.Free.*

trait Dsl[T]:

  import Barrier.*
  enum Barrier:
    case Continuous(
        direction: Barrier.Direction,
        policy: Barrier.Policy,
        levels: Map[String, Double],
        from: T,
        to: T
    )
    case Discrete(
        direction: Barrier.Direction,
        policy: Barrier.Policy,
        levels: Map[String, List[(T, Double)]]
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

object Dsl:

  def apply[T]: Dsl[T] = new Dsl[T] {}
