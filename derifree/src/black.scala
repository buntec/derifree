package derifree.black

import cats.syntax.all.*
import derifree.normal
import org.apache.commons.math3.analysis.UnivariateFunction
import org.apache.commons.math3.analysis.solvers.BrentSolver
import org.apache.commons.math3.util.{FastMath => math}

import scala.util.control.NoStackTrace

enum OptionType:
  case Call, Put

enum Error extends NoStackTrace:
  case PriceExceedsUpperBound
  case PriceBelowIntrinsic
  case SolverFailed(info: String)

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
