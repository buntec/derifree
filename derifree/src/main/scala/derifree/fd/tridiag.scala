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

case class TridiagMatrix(
    lowerDiagonal: IArray[
      Double
    ], // same length as diagonal with the convention that the 0-th entry is used only for Dirichlet boundary conditions in PDEs
    diagonal: IArray[Double],
    upperDiagonal: IArray[
      Double
    ] // the same length as diagonal with the convention that the last entry is used only for Dirichlet boundary conditions in PDEs
):

  def length: Int = diagonal.length

  // computes y = mx
  def multiply(x: IArray[Double]): IArray[Double] =
    val n = length
    val y = Array.ofDim[Double](length)
    require(x.length == n && y.length == n, "dimension mismatch")
    val u = this.upperDiagonal
    val lo = this.lowerDiagonal
    val d = this.diagonal
    y(0) = x(0) * d(0) + x(1) * u(0)
    var i = 1
    while (i < n - 1) {
      y(i) = x(i - 1) * lo(i) + x(i) * d(i) + x(i + 1) * u(i)
      i += 1
    }
    y(n - 1) = x(n - 2) * lo(n - 1) + x(n - 1) * d(n - 1)
    IArray.unsafeFromArray(y)

  // solves my = x for y
  def solve(x: IArray[Double]): IArray[Double] =
    val n = length
    require(x.length == n)
    val y = Array.ofDim[Double](n)

    val a = Array.ofDim[Double](n - 1)
    val b = Array.ofDim[Double](n)
    val u = this.upperDiagonal
    val lo = this.lowerDiagonal
    val d = this.diagonal

    a(0) = u(0) / d(0)
    b(0) = x(0) / d(0)
    var i = 1
    while (i < n - 1) {
      a(i) = u(i) / (d(i) - lo(i) * a(i - 1))
      i += 1
    }
    i = 1
    while (i < n) {
      b(i) = (x(i) - lo(i) * b(i - 1)) / (d(i) - lo(i) * a(i - 1))
      i += 1
    }
    y(n - 1) = b(n - 1)
    i = n - 2
    while (i >= 0) {
      y(i) = b(i) - a(i) * y(i + 1)
      i -= 1
    }
    IArray.unsafeFromArray(y)
