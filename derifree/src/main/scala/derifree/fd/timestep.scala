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

object timestep:

  def explicit(v: ArraySeq[Double], dt: Double, op0: TridiagMatrix): ArraySeq[Double] =
    val ud = op0.upperDiagonal.map(_ * dt)
    val ld = op0.lowerDiagonal.map(_ * dt)
    val d = op0.diagonal.map(1.0 + dt * _)
    val op = TridiagMatrix(ld, d, ud)
    op.multiply(v)

  def `implicit`(v: ArraySeq[Double], dt: Double, op0: TridiagMatrix): ArraySeq[Double] =
    val ud = op0.upperDiagonal.map(_ * -dt)
    val ld = op0.lowerDiagonal.map(_ * -dt)
    val d = op0.diagonal.map(1.0 - dt * _)
    val op = TridiagMatrix(ld, d, ud)
    op.solve(v)
