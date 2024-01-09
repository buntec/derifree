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

trait Operator:

  def a: TridiagMatrix

  def omega: (Double, Double)

  def size: Int

  // v are interior points
  // returns interior points
  def explicitStep(v: IArray[Double], dt: Double): IArray[Double] =

    val ud = Array.ofDim[Double](size)
    val ld = Array.ofDim[Double](size)
    val d = Array.ofDim[Double](size)

    var i = 0
    while (i < size) {
      ud(i) = a.upperDiagonal(i) * dt
      ld(i) = a.lowerDiagonal(i) * dt
      d(i) = 1.0 + a.diagonal(i) * dt
      i += 1
    }

    val a0 = TridiagMatrix(
      IArray.unsafeFromArray(ld),
      IArray.unsafeFromArray(d),
      IArray.unsafeFromArray(ud)
    )

    val result = Array.ofDim[Double](size)
    a0.multiply(v).copyToArray(result)
    result(0) += dt * omega(0)
    result(size - 1) += dt * omega(1)
    IArray.unsafeFromArray(result)

  // v are interior points
  // returns interior points
  def implicitStep(v: IArray[Double], dt: Double): IArray[Double] =

    val ud = Array.ofDim[Double](size)
    val ld = Array.ofDim[Double](size)
    val d = Array.ofDim[Double](size)

    var i = 0
    while (i < size) {
      ud(i) = a.upperDiagonal(i) * -dt
      ld(i) = a.lowerDiagonal(i) * -dt
      d(i) = 1.0 - a.diagonal(i) * dt
      i += 1
    }

    val a0 = TridiagMatrix(
      IArray.unsafeFromArray(ld),
      IArray.unsafeFromArray(d),
      IArray.unsafeFromArray(ud)
    )

    val v0 = Array.ofDim[Double](v.length)
    v.copyToArray(v0)
    v0(0) += dt * omega(0)
    v0(size - 1) += dt * omega(1)

    a0.solve(IArray.unsafeFromArray(v0))

  def thetaStep(v: IArray[Double], dt: Double, theta: Double) =
    implicitStep(explicitStep(v, theta * dt), (1 - theta) * dt)

object Operator:

  def apply(
      grid: IArray[Double],
      convection: IArray[Double],
      diffusion: IArray[Double],
      reaction: IArray[Double],
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
        IArray.unsafeFromArray(lowerDiag),
        IArray.unsafeFromArray(diag),
        IArray.unsafeFromArray(upperDiag)
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
