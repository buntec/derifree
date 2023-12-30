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

package derifree.black

import cats.syntax.all.*
import derifree.normal
import org.apache.commons.math3.analysis.UnivariateFunction
import org.apache.commons.math3.analysis.solvers.BrentSolver
import org.apache.commons.math3.util.{FastMath => math}

enum OptionType:
  case Call, Put

object OptionType:
  extension (ot: OptionType)
    def sign: Int = ot match
      case OptionType.Call => 1
      case OptionType.Put  => -1

enum Error(message: String) extends derifree.Error(message):
  case PriceExceedsUpperBound extends Error("price exceeds upper bound")
  case PriceBelowIntrinsic extends Error("price is below intrinsic value")
  case SolverFailed(info: String) extends Error(s"solver failed: $info")

enum Solver:
  case Brent(maxIters: Int, absAccuracy: Double, minVol: Double, maxVol: Double)

def price(
    optionType: OptionType,
    strike: Double,
    timeToExpiry: Double,
    vol: Double,
    forward: Double,
    discountFactor: Double
): Double =
  val omega = optionType match
    case OptionType.Call => 1
    case OptionType.Put  => -1
  val stdDev = math.sqrt(timeToExpiry) * vol
  val d1 = math.log(forward / strike) / stdDev + 0.5 * stdDev
  val d2 = d1 - stdDev
  discountFactor * omega * (forward * normal.cdf(omega * d1) - strike * normal.cdf(omega * d2))

def impliedVol(
    optionType: OptionType,
    strike: Double,
    timeToExpiry: Double,
    forward: Double,
    discountFactor: Double,
    price: Double,
    solver: Solver = Solver.Brent(100, 1e-6, 0.001, 10.0)
): Either[Error, Double] =
  val omega = optionType match
    case OptionType.Call => 1
    case OptionType.Put  => -1
  if price <= discountFactor * math.max(omega * (forward - strike), 0) then
    Left(Error.PriceBelowIntrinsic)
  else if (omega > 0 && price > discountFactor * forward) || (omega < 0 && price > discountFactor * strike)
  then Left(Error.PriceExceedsUpperBound)
  else
    solver match
      case Solver.Brent(maxIters, absAccuracy, minVol, maxVol) =>
        val brent = new BrentSolver(absAccuracy)
        val objective = new UnivariateFunction:
          def value(x: Double): Double =
            price - derifree.black.price(
              optionType,
              strike,
              timeToExpiry,
              x,
              forward,
              discountFactor
            )
        Either
          .catchNonFatal(brent.solve(maxIters, objective, minVol, maxVol))
          .leftMap(t => Error.SolverFailed(t.getMessage))
