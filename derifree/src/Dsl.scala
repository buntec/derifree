package derifree

import cats.free.Free
import cats.free.Free.*

trait Dsl[T]:

  sealed trait RVA[A]
  case class Spot(ticker: String, time: T) extends RVA[Double]
  case class Cashflow(amount: Double, time: T) extends RVA[PV]
  case class HitProb(
      ticker: String,
      level: Double,
      from: T,
      to: T,
      fromBelow: Boolean
  ) extends RVA[Double]

  type RV[A] = Free[RVA, A]

  def spot(ticker: String, time: T): RV[Double] =
    liftF[RVA, Double](Spot(ticker, time))

  def cashflow(amount: Double, time: T): RV[PV] =
    liftF[RVA, PV](Cashflow(amount, time))

  def hitProb(
      ticker: String,
      level: Double,
      from: T,
      to: T,
      fromBelow: Boolean
  ): RV[Double] =
    liftF[RVA, Double](HitProb(ticker, level, from, to, fromBelow))

  def survivalProb(
      ticker: String,
      level: Double,
      from: T,
      to: T,
      fromBelow: Boolean
  ): RV[Double] =
    liftF[RVA, Double](HitProb(ticker, level, from, to, fromBelow)).map(1 - _)

object Dsl:

  def apply[T]: Dsl[T] = new Dsl[T] {}
