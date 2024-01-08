package derifree.fd

trait Priceable[A, T]:

  def terminalPayoff(a: A, spot: Double): Double

  def lowerBoundary: BoundaryCondition

  def upperBoundary: BoundaryCondition

  def valueTransforms(a: A): List[(T, (Double, Double) => Double)]

  def americanExerciseValue(a: A, t: T, spot: Double): Option[Double]
