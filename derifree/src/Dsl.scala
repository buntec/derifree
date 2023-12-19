package derifree

import cats.Monad
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
  case class Puttable(amount: Option[Double], time: T) extends RVA[Unit]
  case class Callable(amount: Option[Double], time: T) extends RVA[Unit]

  /** A random variable measurable with respect to a simulation. */
  type RV[A] = Free[RVA, A]

  type ContingentClaim = RV[Unit]

  def pure[A](a: A): RV[A] = Monad[RV].pure(a)

  def spot(ticker: String, time: T): RV[Double] =
    liftF[RVA, Double](Spot(ticker, time))

  def cashflow(amount: Double, time: T): RV[PV] =
    liftF[RVA, PV](Cashflow(amount, time))

  def hitProb(barrier: Barrier): RV[Double] =
    liftF[RVA, Double](HitProb(barrier))

  def survivalProb(barrier: Barrier): RV[Double] =
    liftF[RVA, Double](HitProb(barrier)).map(1 - _)

  def exercisable(amount: Option[Double], time: T): RV[Unit] =
    liftF[RVA, Unit](Puttable(amount, time))

  /** Alias for `exercisable`. */
  def puttable(amount: Option[Double], time: T): RV[Unit] =
    liftF[RVA, Unit](Puttable(amount, time))

  def callable(amount: Option[Double], time: T): RV[Unit] =
    liftF[RVA, Unit](Callable(amount, time))

  def min[A: Ordering](as: A*): A = as.min

  def max[A: Ordering](as: A*): A = as.max

  extension [A](rva: RV[A])
    def mean(
        simulator: Simulator[T],
        nSims: Int
    )(using TimeLike[T], Fractional[A], Monoid[A]): Either[derifree.Error, A] =
      Compiler[T].mean(self, simulator, nSims, rva)

  extension (cc: ContingentClaim)
    def fairValue(
        simulator: Simulator[T],
        nSims: Int
    )(using TimeLike[T]): Either[derifree.Error, PV] =
      Compiler[T].fairValue(self, simulator, nSims, cc)

    def earlyTerminationProbabilities(
        simulator: Simulator[T],
        nSims: Int
    )(using TimeLike[T]): Either[derifree.Error, Map[T, Double]] =
      Compiler[T].earlyTerminationProbabilities(self, simulator, nSims, cc)

object Dsl:

  def apply[T]: Dsl[T] = new Dsl[T] {}
