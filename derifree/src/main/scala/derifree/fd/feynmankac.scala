package derifree
package fd

import derifree.syntax.*

import scala.collection.immutable.ArraySeq

object feynmankac:

  def blackScholes[T: TimeLike](
      forward: Forward[T],
      discount: YieldCurve[T],
      vol: Double,
      expiry: T,
      payout: Double => Double,
      refTime: T,
      lowerBoundary: BoundaryCondition,
      upperBoundary: BoundaryCondition,
      timeGridFactory: TimeGrid.Factory,
      spatialGridFactory: SpatialGrid.Factory
  ): Double =
    val specialTimes = expiry :: forward.dividends.map(_.exDiv)
    val specialYfs = specialTimes.map(t => refTime.yearFractionTo(t)).toSet
    val timegrid = timeGridFactory(specialYfs)
    val tMax = specialYfs.max

    val divsByIndex = forward.dividends
      .groupBy(_.exDiv)
      .map((t, divs) =>
        t -> divs.reduce((d1, d2) =>
          d1.copy(cash = d1.cash + d2.cash, prop = d1.prop + d2.prop)
        )
      )
      .map((t, div) => timegrid.indexOf(refTime.yearFractionTo(t)).get -> div)

    val s0 = forward.spot

    val pLower = s0 * SpatialGrid.lognormalPercentile(vol, tMax.toDouble, 0, 1e-5)
    val pUpper = s0 * SpatialGrid.lognormalPercentile(vol, tMax.toDouble, 0, 1 - 1e-5)

    val sMin =
      lowerBoundary match
        case BoundaryCondition.Linear           => pLower
        case BoundaryCondition.Dirichlet(lb, _) => math.max(pLower, lb)

    val sMax = upperBoundary match
      case BoundaryCondition.Linear           => pUpper
      case BoundaryCondition.Dirichlet(lb, _) => math.min(pUpper, lb)

    val ts = timegrid.yearFractions
    val tEps = TimeGrid.tickSize

    val initialGrid = spatialGridFactory(sMin, sMax)
    val initialInteriorValues = initialGrid.slice(1, initialGrid.length - 1).map(payout)

    val (finalGrid, finalInteriorValues) =
      (ts zip (ts.zipWithIndex).tail).reverse.zipWithIndex
        .foldLeft((initialGrid, initialInteriorValues)):
          case ((grid, v2), ((yf1, (yf2, i)), j)) =>
            val dt = (yf2 - yf1).toDouble
            val t1 = refTime.plusYearFraction(yf1 + tEps)
            val t2 = refTime.plusYearFraction(yf2 - tEps)

            val r = -math.log(discount.discountFactor(t1, t2)) / dt
            val mu = math.log(forward(t2) / forward(t1)) / dt

            val (gridNext, v2Minus) =
              divsByIndex
                .get(i)
                .fold((grid, v2)): div =>
                  (lowerBoundary, upperBoundary) match
                    case (BoundaryCondition.Linear, BoundaryCondition.Linear) =>
                      grid.map(s => (s + div.cash) / (1 - div.prop)) -> v2

                    case (
                          BoundaryCondition.Dirichlet(lowerBound, value),
                          BoundaryCondition.Linear
                        ) =>
                      val newGrid = spatialGridFactory(sMin, (sMax + div.cash) / (1 - div.prop))
                      val interp =
                        interpolation.naturalCubicSpline(
                          grid,
                          SpatialGrid.addBoundaryValues(grid, v2, lowerBoundary, upperBoundary)
                        )
                      newGrid -> newGrid
                        .slice(1, newGrid.length - 1)
                        .map(s =>
                          if s <= lowerBound then value
                          else interp(s * (1 - div.prop) - div.cash)
                        )

                    case (
                          BoundaryCondition.Linear,
                          BoundaryCondition.Dirichlet(upperBound, value)
                        ) =>
                      val newGrid = spatialGridFactory(
                        (sMin + div.cash) / (1 - div.prop),
                        sMax
                      )
                      val interp =
                        interpolation.naturalCubicSpline(
                          grid,
                          SpatialGrid.addBoundaryValues(grid, v2, lowerBoundary, upperBoundary)
                        )
                      newGrid -> newGrid
                        .slice(1, newGrid.length - 1)
                        .map(s =>
                          if s >= upperBound then value
                          else interp(s * (1 - div.prop) - div.cash)
                        )

                    case (
                          BoundaryCondition.Dirichlet(lowerBound, value1),
                          BoundaryCondition.Dirichlet(upperBound, value2)
                        ) =>
                      val interp =
                        interpolation.naturalCubicSpline(
                          grid,
                          SpatialGrid.addBoundaryValues(grid, v2, lowerBoundary, upperBoundary)
                        )
                      grid -> grid
                        .slice(1, grid.length - 1)
                        .map(s =>
                          if s <= lowerBound then value1
                          else if s >= upperBound then value2
                          else interp(s * (1 - div.prop) - div.cash)
                        )

            val interiorPoints = gridNext.slice(1, gridNext.length - 1)

            val reaction = interiorPoints.map(_ => r)
            val convection = interiorPoints.map(_ * mu)
            val diffusion = interiorPoints.map(s => 0.5 * s * s * vol * vol)

            val op =
              Operator(gridNext, convection, diffusion, reaction, upperBoundary, lowerBoundary)

            val v1 =
              if j < 2 then
                // Rannacher smoothing
                op.implicitStep(op.implicitStep(v2Minus, 0.5 * dt), 0.5 * dt)
              else
                // Crank-Nicolson
                op.thetaStep(v2Minus, dt, 0.5)

            (gridNext, v1)

    val finalValues =
      SpatialGrid.addBoundaryValues(
        finalGrid,
        finalInteriorValues,
        lowerBoundary,
        upperBoundary
      )

    interpolation.naturalCubicSpline(finalGrid, finalValues)(s0)
