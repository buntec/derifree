package derifree

trait YieldCurve[T]:
  def discountFactor(t: T): Double
  def discountFactor(t1: T, t2: T): Double
  def spotRate(t: T): Rate

object YieldCurve:

  def fromConstantRate[T: TimeLike](r: Rate, refTime: T): YieldCurve[T] = new YieldCurve[T]:
    def discountFactor(t: T): Double = discountFactor(refTime, t)
    def discountFactor(t1: T, t2: T): Double =
      val dt = TimeLike[T].yearFractionBetween(t1, t2)
      math.exp(-r * dt)
    def spotRate(t: T): Rate = r

  def zero[T]: YieldCurve[T] = new YieldCurve[T]:
    def discountFactor(at: T): Double = 1.0
    def discountFactor(t1: T, t2: T): Double = 1.0
    def spotRate(t: T): Rate = Rate(0.0)
