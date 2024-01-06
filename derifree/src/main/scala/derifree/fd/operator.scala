package derifree
package fd

import scala.collection.immutable.ArraySeq

object operator:

  def withLinearityBoundaryCondition(
      grid: ArraySeq[Double],
      convection: ArraySeq[Double],
      diffusion: ArraySeq[Double],
      reaction: ArraySeq[Double]
  ): TridiagMatrix = {

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
      diag(i) = (convection(i) * (dxUp - dxDown) + 2.0 * diffusion(
        i
      )) / (dxUp * dxDown) - reaction(i)
      lowerDiag(i) = (-convection(i) * dxUp - 2.0 * diffusion(
        i
      )) / (dxDown * (dxUp + dxDown))
      upperDiag(i) = (convection(i) * dxDown - 2.0 * diffusion(i)) / (dxUp * (dxUp + dxDown))
      i += 1
    }
    var dxUp = grid(2) - grid(1)
    var dxDown = grid(1) - grid(0)
    diag(0) += lowerDiag(0) * (dxUp + dxDown) / dxUp
    upperDiag(0) += lowerDiag(0) * (-dxDown / dxUp)
    dxUp = grid(n + 1) - grid(n)
    dxDown = grid(n) - grid(n - 1)
    diag(n - 1) += upperDiag(n - 1) * (dxUp + dxDown) / dxDown
    lowerDiag(n - 1) += upperDiag(n - 1) * (-dxUp / dxDown)

    TridiagMatrix(
      ArraySeq.unsafeWrapArray(lowerDiag),
      ArraySeq.unsafeWrapArray(diag),
      ArraySeq.unsafeWrapArray(upperDiag)
    )

  }
