package derifree

import Lsm.*

trait Lsm:

  def toBasisFunctions(factors: List[Double]): List[Double]

  def toContValueEstimator(rows: Seq[(List[Double], Double)]): Estimator

object Lsm:

  trait Estimator:
    def apply(factors: List[Double]): Double

  def apply: Lsm = new Lsm:
    def toBasisFunctions(factors: List[Double]): List[Double] = ???
    def toContValueEstimator(rows: Seq[(List[Double], Double)]): Estimator = ???
