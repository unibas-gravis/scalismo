package scalismo.transformations

import breeze.linalg.{DenseMatrix, DenseVector}
import scalismo.common.{Domain, Field}
import scalismo.geometry.{Point, SquareMatrix}
import scalismo.transformations.ParametricTransformation.JacobianField
import scalismo.transformations.TransformationSpace.ParameterVector

/**
 *  Class defining transformations composed of two argument transforms.
 *  The resulting transform is <code>outerTransform compose innerTransform</code>
 *
 *  @param innerTransform transform to be applied first. Must be a parametric differentiable transform
 *  @param outerTransform transform to be applied second. Must be a parametric differentiable transform
 */
case class CompositeTransformation[D, O[D] <: ParametricTransformation[D] with CanDifferentiate[D], I[
  D
] <: ParametricTransformation[D] with CanDifferentiate[D]](outerTransform: O[D], innerTransform: I[D])
    extends ParametricTransformation[D]
    with CanDifferentiate[D] {

  override val domain = innerTransform.domain
  override val f = (x: Point[D]) => {
    (outerTransform.f compose innerTransform.f)(x)
  }

  override def derivative: Point[D] => SquareMatrix[D] = { p =>
    outerTransform.derivative(innerTransform(p)) * innerTransform.derivative(p)
  }
  override def numberOfParameters: Int = outerTransform.numberOfParameters + innerTransform.numberOfParameters

  /** parameters of the composed transform. This is simply a concatenation of the outer and inner transform parameters */
  override val parameters = DenseVector(outerTransform.parameters.data ++ innerTransform.parameters.data)

  override def jacobian: JacobianField[D] = {
    val jacobianField = (x: Point[D]) => {
      DenseMatrix.horzcat(
        outerTransform.jacobian(x),
        outerTransform.derivative(innerTransform(x)).toBreezeMatrix * innerTransform.jacobian(x)
      )
    }
    Field(domain, jacobianField)
  }

}

case class ProductTransformationSpace[D, OTS[D] <: TransformationSpaceWithDifferentiableTransforms[D], ITS[D] <: TransformationSpaceWithDifferentiableTransforms[
  D
]](
  outerTS: OTS[D],
  innerTS: ITS[D]
) extends TransformationSpaceWithDifferentiableTransforms[D] {
  final override type T[D] = CompositeTransformation[D, OTS[D]#T, ITS[D]#T]

  override def domain: Domain[D] = innerTS.domain

  override def numberOfParameters: Int = outerTS.numberOfParameters + innerTS.numberOfParameters

  override def transformationForParameters(p: ParameterVector): T[D] = {
    val outerParams = p(0 until outerTS.numberOfParameters)
    val innerParams = p(outerTS.numberOfParameters + 1 until p.length)
    CompositeTransformation[D, OTS[D]#T, ITS[D]#T](outerTS.transformationForParameters(outerParams),
                                                   innerTS.transformationForParameters(innerParams))
  }

  /** returns identity transformation) */
  override def identityTransformation: T[D] = {
    CompositeTransformation[D, OTS[D]#T, ITS[D]#T](outerTS.identityTransformation, innerTS.identityTransformation)
  }
}
