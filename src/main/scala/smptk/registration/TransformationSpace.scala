package smptk.registration

import TransformationSpace.ParameterVector
import breeze.linalg.DenseVector
import breeze.linalg.DenseMatrix
import smptk.image.CoordVectorLike

trait TransformationSpace[Point <: CoordVectorLike] extends Function1[ParameterVector, Transformation[Point]] {
  type JacobianImage = Function1[Point, DenseMatrix[Float]]
  def parametersDimensionality: Int
  def takeDerivative(alpha : ParameterVector) : JacobianImage
}

trait Transformation[Point <: CoordVectorLike] extends (Point => Point) {

}
    
object TransformationSpace {
  type ParameterVector = DenseVector[Float]
	

}