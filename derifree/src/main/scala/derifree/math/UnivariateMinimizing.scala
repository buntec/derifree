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
package math

import cats.syntax.all.*
import org.apache.commons.math3.analysis.UnivariateFunction
import org.apache.commons.math3.optim.MaxEval
import org.apache.commons.math3.optim.MaxIter
import org.apache.commons.math3.optim.nonlinear.scalar.GoalType
import org.apache.commons.math3.optim.univariate.BrentOptimizer
import org.apache.commons.math3.optim.univariate.SearchInterval
import org.apache.commons.math3.optim.univariate.UnivariateObjectiveFunction

object UnivariateMinimizing:

  case class Result(point: Double, value: Double)

  def brent(
      f: Double => Double,
      min: Double,
      max: Double,
      start: Double,
      absTol: Double = 1e-6,
      relTol: Double = 1e-6,
      maxEvals: Int = 30
  ): Either[Error, Result] =
    for
      opt <- Either
        .catchNonFatal(new BrentOptimizer(relTol, absTol))
        .leftMap(t => Error.BadInputs(t.getMessage))
      goalType = GoalType.MINIMIZE
      searchInterval <- Either
        .catchNonFatal(SearchInterval(min, max, start))
        .leftMap(t => Error.BadInputs(t.getMessage))
      uf = new UnivariateFunction:
        def value(x: Double): Double = f(x)
      objFun = new UnivariateObjectiveFunction(uf)
      pair <- Either
        .catchNonFatal(
          opt.optimize(
            goalType,
            searchInterval,
            MaxEval(maxEvals),
            MaxIter(maxEvals * 10),
            objFun
          )
        )
        .leftMap(t => Error.BadNumerics(t.getMessage))
    yield Result(pair.getPoint, pair.getValue)
