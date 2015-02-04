package scalismo.registration

import scalismo.geometry.{Point, Dim}
import scalismo.statisticalmodel.LowRankGaussianProcess

import scala.NotImplementedError
import TransformationSpace.ParameterVector

import breeze.linalg.{DenseMatrix, DenseVector}


class GaussianProcessTransformationSpace[D <: Dim] private (gp: LowRankGaussianProcess[D, D]) extends TransformationSpace[D] with DifferentiableTransforms[D] {

  override type T = GaussianProcessTransformation[D]

  override def identityTransformParameters = DenseVector.zeros[Float](parametersDimensionality)

  override def parametersDimensionality = gp.rank

  override def transformForParameters(p: ParameterVector) = GaussianProcessTransformation[D](gp, p)
  override def takeDerivativeWRTParameters(p: ParameterVector) = {

    /**
     * The jacobian matrix of a sample, with respect to the given parameteers.
     * @param p
     * @return
     */
    (x: Point[D]) => {
      val dim = x.dimensionality
      val J = DenseMatrix.zeros[Float](dim, gp.klBasis.size)
      (0 until gp.rank).map(i => {
        val (lambda_i, phi_i) = gp.klBasis(i)
        J(::, i) := (phi_i(x) * math.sqrt(lambda_i).toFloat).toBreezeVector
      })
      J
    }
  }

}

class GaussianProcessTransformation[D <: Dim] private (gp: LowRankGaussianProcess[D, D], alpha: ParameterVector) extends ParametricTransformation[D] with CanDifferentiate[D] {

  val instance = gp.instance(alpha)
  val parameters = alpha

  override val domain = gp.domain

  override val  f = (x: Point[D]) => {
    val newPointAsVector = instance(x)
    x + newPointAsVector
  }
  def takeDerivative(x: Point[D]) = { throw new NotImplementedError("take derivative of kernel") }
}

object GaussianProcessTransformation {
  def apply[D <: Dim](gp: LowRankGaussianProcess[D, D], alpha: TransformationSpace.ParameterVector) = new GaussianProcessTransformation[D](gp, alpha)
}

object GaussianProcessTransformationSpace {
  def apply[D <: Dim](gp: LowRankGaussianProcess[D, D]) = new GaussianProcessTransformationSpace[D](gp)
}





