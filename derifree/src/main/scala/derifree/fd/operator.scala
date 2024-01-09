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

import scala.collection.immutable.ArraySeq

trait Operator:

  def a: TridiagMatrix

  def omega: (Double, Double)

  def size: Int

  // v are interior points
  // returns interior points
  def explicitStep(v: ArraySeq[Double], dt: Double): ArraySeq[Double] =
    val ud = a.upperDiagonal.map(_ * dt)
    val ld = a.lowerDiagonal.map(_ * dt)
    val d = a.diagonal.map(1.0 + dt * _)
    val a0 = TridiagMatrix(ld, d, ud)
    val result = a0.multiply(v)
    val underlying = result.unsafeArray.asInstanceOf[Array[Double]]
    underlying(0) += dt * omega(0)
    underlying(size - 1) += dt * omega(1)
    result

  // v are interior points
  // returns interior points
  def implicitStep(v: ArraySeq[Double], dt: Double): ArraySeq[Double] =
    val ud = a.upperDiagonal.map(_ * -dt)
    val ld = a.lowerDiagonal.map(_ * -dt)
    val d = a.diagonal.map(1.0 - dt * _)
    val op = TridiagMatrix(ld, d, ud)
    val v0 = v.toArray[Double]
    v0(0) += dt * omega(0)
    v0(size - 1) += dt * omega(1)
    op.solve(ArraySeq.unsafeWrapArray(v0))

  def thetaStep(v: ArraySeq[Double], dt: Double, theta: Double) =
    implicitStep(explicitStep(v, theta * dt), (1 - theta) * dt)

object Operator:

  def apply(
      grid: ArraySeq[Double],
      convection: ArraySeq[Double],
      diffusion: ArraySeq[Double],
      reaction: ArraySeq[Double],
      upperBoundary: BoundaryCondition,
      lowerBoundary: BoundaryCondition
  ): Operator =
    val n = grid.length - 2 // number of interior points
    require(
      convection.length == n && diffusion.length == n && reaction.length == n,
      "dimension mismatch"
    )
    val diag = Array.ofDim[Double](n)
    val lowerDiag = Array.ofDim[Double](n)
    val upperDiag = Array.ofDim[Double](n)

    var i = 0
    while (i < n) {
      val dxUp = grid(i + 2) - grid(i + 1)
      val dxDown = grid(i + 1) - grid(i)
      diag(i) =
        (convection(i) * (dxUp - dxDown) - 2.0 * diffusion(i)) / (dxUp * dxDown) - reaction(i)
      lowerDiag(i) = (-convection(i) * dxUp + 2.0 * diffusion(i)) / (dxDown * (dxUp + dxDown))
      upperDiag(i) = (convection(i) * dxDown + 2.0 * diffusion(i)) / (dxUp * (dxUp + dxDown))
      i += 1
    }

    lowerBoundary match
      case BoundaryCondition.Linear =>
        val dxUp = grid(2) - grid(1)
        val dxDown = grid(1) - grid(0)
        val lambda1 = (dxUp + dxDown) / dxUp
        val lambda2 = -dxDown / dxUp
        diag(0) += lambda1 * lowerDiag(0)
        upperDiag(0) += lambda2 * lowerDiag(0)
      case BoundaryCondition.Dirichlet(spot, value) => ()

    upperBoundary match
      case BoundaryCondition.Linear =>
        val dxUp = grid(n + 1) - grid(n)
        val dxDown = grid(n) - grid(n - 1)
        val lambda1 = (dxUp + dxDown) / dxDown
        val lambda2 = -dxUp / dxDown
        diag(n - 1) += lambda1 * upperDiag(n - 1)
        lowerDiag(n - 1) += lambda2 * upperDiag(n - 1)
      case BoundaryCondition.Dirichlet(spot, value) => ()

    new Operator:
      def size: Int = n

      def a: TridiagMatrix = TridiagMatrix(
        ArraySeq.unsafeWrapArray(lowerDiag),
        ArraySeq.unsafeWrapArray(diag),
        ArraySeq.unsafeWrapArray(upperDiag)
      )

      def omega = (
        lowerBoundary match
          case BoundaryCondition.Linear              => 0
          case BoundaryCondition.Dirichlet(_, value) => lowerDiag(0) * value
        ,
        upperBoundary match
          case BoundaryCondition.Linear              => 0
          case BoundaryCondition.Dirichlet(_, value) => upperDiag(n - 1) * value
      )
