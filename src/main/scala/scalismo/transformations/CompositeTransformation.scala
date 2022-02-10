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
 *  @param innerTransformation transform to be applied first. Must be a parametric differentiable transform
 *  @param outerTransformation transform to be applied second. Must be a parametric differentiable transform
 */
class CompositeTransformation[D, O[D] <: ParametricTransformation[D] with CanDifferentiateWRTPosition[D], I[D] <: ParametricTransformation[
  D
]](
  val outerTransformation: O[D],
  val innerTransformation: I[D]
) extends ParametricTransformation[D] {

  override val domain = innerTransformation.domain
  override val f: Point[D] => Point[D] = (x: Point[D]) => {
    (outerTransformation.f.compose(innerTransformation.f))(x)
  }

  override def numberOfParameters: Int = outerTransformation.numberOfParameters + innerTransformation.numberOfParameters

  override val parameters = DenseVector(outerTransformation.parameters.data ++ innerTransformation.parameters.data)

  override def derivativeWRTParameters: JacobianField[D] = {
    val jacobianField = (x: Point[D]) => {
      DenseMatrix.horzcat(
        outerTransformation.derivativeWRTParameters(x),
        outerTransformation.derivativeWRTPosition(innerTransformation(x)).toBreezeMatrix * innerTransformation
          .derivativeWRTParameters(x)
      )
    }
    Field(domain, jacobianField)
  }

}

object CompositeTransformation {
  def apply[D, O[D] <: ParametricTransformation[D] with CanDifferentiateWRTPosition[D], I[D] <: ParametricTransformation[
    D
  ]](
    outerTransformation: O[D],
    innerTransformation: I[D]
  ): CompositeTransformation[D, O, I] = {
    new CompositeTransformation(outerTransformation, innerTransformation)
  }
}

case class ProductTransformationSpace[D, OuterTS[D] <: TransformationSpaceWithDifferentiableTransforms[D], InnerTS[D] <: TransformationSpace[
  D
]](
  outerTS: OuterTS[D],
  innerTS: InnerTS[D]
) extends TransformationSpace[D] {

  override type T[D] = CompositeTransformation[D, outerTS.T, innerTS.T]

  override def domain: Domain[D] = innerTS.domain

  override def numberOfParameters: Int = innerTS.numberOfParameters + outerTS.numberOfParameters

  override def transformationForParameters(p: ParameterVector): T[D] = {
    val outerParams = p(0 until outerTS.numberOfParameters).copy
    val innerParams = p(outerTS.numberOfParameters until outerTS.numberOfParameters + innerTS.numberOfParameters).copy
    val outerTransform: outerTS.T[D] = outerTS.transformationForParameters(outerParams)
    val innerTransform: innerTS.T[D] = innerTS.transformationForParameters(innerParams)
    CompositeTransformation(outerTransform, innerTransform)
  }

  /** returns identity transformation) */
  override def identityTransformation: T[D] = {
    CompositeTransformation(outerTS.identityTransformation, innerTS.identityTransformation)
  }
}

case class CompositeDifferentiableTransformation[D, O[D] <: ParametricTransformation[D] with CanDifferentiateWRTPosition[
  D
], I[
  D
] <: ParametricTransformation[D] with CanDifferentiateWRTPosition[D]](override val outerTransformation: O[D],
                                                                      override val innerTransformation: I[D])
    extends CompositeTransformation[D, O, I](outerTransformation, innerTransformation)
    with CanDifferentiateWRTPosition[D] {

  override def derivativeWRTPosition: Point[D] => SquareMatrix[D] = { p =>
    outerTransformation.derivativeWRTPosition(innerTransformation(p)) * innerTransformation.derivativeWRTPosition(p)
  }
}

object CompositeDifferentiableTransformation {
  def apply[D, O[D] <: ParametricTransformation[D] with CanDifferentiateWRTPosition[D], I[D] <: ParametricTransformation[
    D
  ] with CanDifferentiateWRTPosition[D]](
    outerTransformation: O[D],
    innerTransformation: I[D]
  ): CompositeDifferentiableTransformation[D, O, I] = {
    new CompositeDifferentiableTransformation(outerTransformation, innerTransformation)
  }
}

case class ProductTransformationSpaceWithDifferentiableTransforms[D, OuterTS[D] <: TransformationSpaceWithDifferentiableTransforms[
  D
], InnerTS[D] <: TransformationSpaceWithDifferentiableTransforms[D]](
  outerTS: OuterTS[D],
  innerTS: InnerTS[D]
) extends TransformationSpaceWithDifferentiableTransforms[D] {

  override type T[D] = CompositeDifferentiableTransformation[D, outerTS.T, innerTS.T]

  override def domain: Domain[D] = innerTS.domain

  override def numberOfParameters: Int = innerTS.numberOfParameters + outerTS.numberOfParameters

  override def transformationForParameters(p: ParameterVector): T[D] = {
    val outerParams = p(0 until outerTS.numberOfParameters)
    val innerParams = p(outerTS.numberOfParameters until innerTS.numberOfParameters)
    CompositeDifferentiableTransformation(outerTS.transformationForParameters(outerParams),
                                          innerTS.transformationForParameters(innerParams))
  }

  /** returns identity transformation) */
  override def identityTransformation: T[D] = {
    CompositeDifferentiableTransformation(outerTS.identityTransformation, innerTS.identityTransformation)
  }
}
