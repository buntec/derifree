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

package derifree.fd

trait Priceable[A, T]:

  def terminalPayoff(a: A, spot: Double): Double

  def lowerBoundary: BoundaryCondition

  def upperBoundary: BoundaryCondition

  def valueTransforms(a: A): List[(T, (Double, Double) => Double)]

  def americanExerciseValue(a: A, t: T, spot: Double): Option[Double]
