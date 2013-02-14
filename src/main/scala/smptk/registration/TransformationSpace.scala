package smptk
package registration

import scala.language.higherKinds
import TransformationSpace.ParameterVector
import breeze.linalg.DenseVector
import breeze.linalg.DenseMatrix
import image._
import smptk.image.Geometry._

trait TransformationSpace[CV[A] <: CoordVector[A]] extends Function1[ParameterVector, Transformation[CV]] {
  type JacobianImage = Function1[CV[Float], DenseMatrix[Float]]
  def parametersDimensionality: Int
  def takeDerivativeWRTParameters(alpha: ParameterVector): JacobianImage
}

trait Transformation[CV[A] <: CoordVector[A]] extends (CV[Float] => CV[Float]) {
  def takeDerivative(x : CV[Float]): DenseMatrix[Float]
}

case class TranslationSpace1D extends TransformationSpace[CoordVector1D] {
  def apply(p: ParameterVector) = {
    new Transformation[CoordVector1D] { 
      
      def apply(pt: Point1D) =   p(0) + pt(0)
      def takeDerivative(x : Point1D) = {
        DenseMatrix.eye[Float](1)
      } 
    }
  }
  def parametersDimensionality: Int = 1
  def takeDerivativeWRTParameters(p: ParameterVector) = { x: Point1D =>
    DenseMatrix.eye[Float](1)
  }
}

case class TranslationSpace2D extends TransformationSpace[CoordVector2D] {

   def parametersDimensionality: Int = 2
    
  def apply(p: ParameterVector) = {
    new Transformation[CoordVector2D] { 
      def apply(pt: Point2D) = CoordVector2D(p(0) + pt(0), p(1) + pt(1)) 
      def takeDerivative(x : Point2D) = {
        DenseMatrix.eye[Float](2)
      } 
    }
   }

  def takeDerivativeWRTParameters(p: ParameterVector) = { x: Point2D =>
    DenseMatrix.eye[Float](2)
  }
}


object TransformationSpace {
  type ParameterVector = DenseVector[Float]

}