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

package derifree.math

import cats.syntax.all.*
import org.apache.commons.math3.analysis.UnivariateFunction
import org.apache.commons.math3.analysis.solvers.BrentSolver

object RootFinding:

  case class Settings(
      maxIters: Int = 30,
      absAccuracy: Double = 1e-6
  )

  case class Error(message: String) extends derifree.Error(message)

  def brent(
      f: Double => Double,
      min: Double,
      max: Double,
      settings: Settings = Settings()
  ): Either[Error, Double] =
    val solver = new BrentSolver(settings.absAccuracy)
    val uf = new UnivariateFunction:
      def value(x: Double): Double = f(x)
    Either
      .catchNonFatal(solver.solve(settings.maxIters, uf, min, max))
      .leftMap(t => Error(t.toString))
      .flatTap(root => Either.raiseWhen(root.isNaN)(Error("root solver returned NaN")))
