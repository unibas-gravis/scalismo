package smptk.registration

import scala.language.higherKinds
import TransformationSpace.ParameterVector
import breeze.linalg.DenseVector
import breeze.linalg.DenseMatrix
import smptk.image.CoordVectorLike

trait TransformationSpace[CoordVector[A] <: CoordVectorLike[A]] extends Function1[ParameterVector, Transformation[CoordVector]] {
  type JacobianImage = Function1[CoordVector[Float], DenseMatrix[Float]]
  def parametersDimensionality: Int
  def takeDerivative(alpha : ParameterVector) : JacobianImage
}

trait Transformation[CoordVector[A] <: CoordVectorLike[A]] extends (CoordVector[Float] => CoordVector[Float]) {

}
    
object TransformationSpace {
  type ParameterVector = DenseVector[Float]
	

}