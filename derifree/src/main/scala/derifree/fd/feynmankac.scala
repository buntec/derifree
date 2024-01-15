/*
 * Copyright 2023 buntec
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package derifree
package fd

import derifree.syntax.*

object feynmankac:

  def blackScholes[T: TimeLike, P: FD[_, T]](
      payoff: P,
      forward: Forward[T],
      discount: YieldCurve[T],
      vol: Double,
      refTime: T,
      timeGridFactory: TimeGrid.Factory,
      spatialGridFactory: SpatialGrid.Factory,
      settings: Settings
  ): Double =
    val P = FD[P, T]
    val lowerBoundary = P.lowerBoundary(payoff)
    val upperBoundary = P.upperBoundary(payoff)
    val expiry = P.terminalPayoff(payoff)(0)
    val terminalPayout = P.terminalPayoff(payoff)(1)

    val specialTimes =
      expiry :: forward.dividends.map(_.exDiv) ::: P.valueTransforms(payoff).map(_(0))
    val specialYfs = specialTimes.map(t => refTime.yearFractionTo(t)).toSet
    val timegrid = timeGridFactory(specialYfs)
    val tMax = specialYfs.max

    val valTransformByIndex =
      P.valueTransforms(payoff)
        .map((t, f) => timegrid.indexOf(refTime.yearFractionTo(t)).get -> f)
        .toMap

    val divsByIndex = forward.dividends
      .groupBy(_.exDiv)
      .map((t, divs) =>
        t -> divs.reduce((d1, d2) =>
          d1.copy(cash = d1.cash + d2.cash, prop = d1.prop + d2.prop)
        )
      )
      .map((t, div) => timegrid.indexOf(refTime.yearFractionTo(t)).get -> div)

    val s0 = forward.spot

    val pLower =
      s0 * SpatialGrid.lognormalPercentile(vol, tMax.toDouble, 0, settings.gridQuantile)
    val pUpper =
      s0 * SpatialGrid.lognormalPercentile(vol, tMax.toDouble, 0, 1 - settings.gridQuantile)

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
    val initialInteriorValues = initialGrid.slice(1, initialGrid.length - 1).map(terminalPayout)

    val (finalGrid, finalInteriorValues) =
      (ts zip (ts.zipWithIndex).tail).reverse.zipWithIndex
        .foldLeft((initialGrid, initialInteriorValues)):
          case ((grid, v2Pre), ((yf1, (yf2, i)), j)) =>
            val dt = (yf2 - yf1).toDouble
            val t1 = refTime.plusYearFraction(yf1 + tEps)
            val t2 = refTime.plusYearFraction(yf2 - tEps)

            val r = -math.log(discount.discountFactor(t1, t2)) / dt
            val mu = math.log(forward(t2) / forward(t1)) / dt

            val v2 = valTransformByIndex
              .get(i)
              .fold(v2Pre)(f =>
                (grid.slice(1, grid.length - 1) zip v2Pre).map((x, v) => f(x, v))
              )

            val (gridNext, v2MinusPre) =
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
            val n = interiorPoints.length

            val reaction = Array.ofDim[Double](n)
            val convection = Array.ofDim[Double](n)
            val diffusion = Array.ofDim[Double](n)

            var k = 0
            while (k < n) {
              val s = interiorPoints(k)
              reaction(k) = r
              convection(k) = s * mu
              diffusion(k) = 0.5 * s * s * vol * vol
              k += 1
            }

            val op =
              Operator(
                gridNext,
                IArray.unsafeFromArray(convection),
                IArray.unsafeFromArray(diffusion),
                IArray.unsafeFromArray(reaction),
                upperBoundary,
                lowerBoundary
              )

            val v2Minus = P
              .americanExerciseValue(payoff)
              .fold(v2MinusPre): f =>
                val exerciseAmount = f(t2)
                (interiorPoints zip v2MinusPre).map((s, v) => math.max(exerciseAmount(s), v))

            val v1 =
              if j < settings.nRannacherSteps then
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
