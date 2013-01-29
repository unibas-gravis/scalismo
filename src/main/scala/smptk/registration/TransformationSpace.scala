package smptk.registration

import TransformationSpace.ParameterVector
import breeze.linalg.DenseVector
import breeze.linalg.DenseMatrix
import smptk.image.{PointLike, VectorLike}

trait TransformationSpace[Point <: PointLike, Vector <: VectorLike] extends Function1[ParameterVector, Transformation[Point]] {
  type JacobianImage = Function1[Point, DenseMatrix[Float]]
  def parametersDimensionality: Int
  def takeDerivative(alpha : ParameterVector) : JacobianImage
}

trait Transformation[Point <: PointLike] extends (Point => Point) {

}
    
object TransformationSpace {
  type ParameterVector = DenseVector[Float]
	

}