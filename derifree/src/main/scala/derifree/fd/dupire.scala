package derifree
package fd

import derifree.syntax.*

import scala.math.max

object dupire:

  trait CallPriceSurface[T]:

    def apply(strike: Double, expiry: T): Double

  def blackScholesCallPrices[T: TimeLike](
      forward: Forward[T],
      discount: YieldCurve[T],
      vol: Double,
      refTime: T,
      tMax: T,
      timeGridFactory: TimeGrid.Factory,
      settings: Settings
  ): CallPriceSurface[T] =
    val s0 = 1.0 // pure process starting value
    val spatialGridFactory = SpatialGrid.Factory.logSinh(250, 0.1, s0)
    val t = TimeLike[T].yearFractionBetween(refTime, tMax)
    val timegrid = timeGridFactory(Set(t))

    val sMin = SpatialGrid.lognormalPercentile(vol, t.toDouble, 0, settings.gridQuantile)
    val sMax = SpatialGrid.lognormalPercentile(vol, t.toDouble, 0, 1 - settings.gridQuantile)
    val initialGrid = spatialGridFactory(sMin, sMax)
    val initialInteriorPoints = initialGrid.slice(1, initialGrid.length - 1)
    val initialInteriorValues = initialInteriorPoints.map(s => max(s0 - s, 0.0))

    val ts = timegrid.yearFractions

    val valuesOnGrid =
      (ts zip ts.zipWithIndex.tail).zipWithIndex.scanLeft(initialInteriorValues):
        case (v1, ((t1, (t2, i)), j)) =>
          val dt = (t2 - t1).toDouble

          val interiorPoints = initialGrid.slice(1, initialGrid.length - 1)
          val n = interiorPoints.length

          val reaction = Array.ofDim[Double](n)
          val convection = Array.ofDim[Double](n)
          val diffusion = Array.ofDim[Double](n)

          var k = 0
          while (k < n) {
            val s = interiorPoints(k)
            reaction(k) = 0.0
            convection(k) = 0.0
            diffusion(k) = 0.5 * s * s * vol * vol
            k += 1
          }

          val op =
            Operator(
              initialGrid,
              IArray.unsafeFromArray(convection),
              IArray.unsafeFromArray(diffusion),
              IArray.unsafeFromArray(reaction),
              BoundaryCondition.Linear,
              BoundaryCondition.Linear
            )

          val v2 =
            if j < settings.nRannacherSteps then
              // Rannacher smoothing
              op.implicitStep(op.implicitStep(v1, 0.5 * dt), 0.5 * dt)
            else
              // Crank-Nicolson
              op.thetaStep(v1, dt, 0.5)

          v2

    val interpolatedValues =
      valuesOnGrid.map(values => CubicSpline.natural(initialInteriorPoints, values))

    def callPrice(expiry: T, strike: Double): Double =
      val t = refTime.yearFractionTo(expiry)
      require(t.toDouble > 0.0 && t < ts.max)
      val i = ts.search(t).insertionPoint
      val pureStrike =
        buehler.strikeToPureStrike(strike, forward(expiry), forward.dividendFloor(expiry))
      val t1 = ts(i - 1)
      val t2 = ts(i)
      val c1 = interpolatedValues(i - 1)(pureStrike)
      val c2 = interpolatedValues(i)(pureStrike)
      val c = c1 + (c2 - c1) * (t.toDouble - t1.toDouble) / (t2.toDouble - t1.toDouble)
      discount.discountFactor(expiry) * (forward(expiry) - forward.dividendFloor(expiry)) * c

    new CallPriceSurface[T]:
      def apply(strike: Double, expiry: T): Double = callPrice(expiry, strike)
