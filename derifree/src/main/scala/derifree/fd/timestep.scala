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
