package smptk
package registration

import scala.language.higherKinds
import TransformationSpace.ParameterVector
import breeze.linalg.DenseVector
import breeze.linalg._
import breeze.linalg.DenseMatrix
import image._
import smptk.image.Geometry._

trait TransformationSpace[CV[A] <: CoordVector[A]] extends Function1[ParameterVector, Transformation[CV]] {
  self: TransformationSpace[CV] =>

  type JacobianImage = Function1[CV[Float], DenseMatrix[Float]]
  def parametersDimensionality: Int
  def takeDerivativeWRTParameters(alpha: ParameterVector): JacobianImage

  // 
  def product(that: TransformationSpace[CV]): TransformationSpace[CV] = {
    ProductTransformationSpace(self, that)
  }

}

trait Transformation[CV[A] <: CoordVector[A]] extends (CV[Float] => CV[Float]) {
  def takeDerivative(x: CV[Float]): DenseMatrix[Float]
}


case class ProductTransformationSpace[CV[A] <: CoordVector[A]](outer : TransformationSpace[CV], inner  : TransformationSpace[CV]) extends TransformationSpace[CV] {

  def parametersDimensionality = outer.parametersDimensionality + inner.parametersDimensionality

  def apply(p: ParameterVector) = {

    new Transformation[CV] {
      def apply(x: CV[Float]) = {
        val (pThis, pThat) = splitProductParameterVector(p)
        val outerTransform = outer(pThis)
        val innerTransfrom = inner(pThat)
        (outerTransform compose innerTransfrom)(x)
      }
      def takeDerivative(x: CV[Float]) = {
        val (pThis, pThat) = splitProductParameterVector(p)
        val outerTransform = outer(pThis)
        val innerTransform = inner(pThat)
        outerTransform.takeDerivative(innerTransform(x)) * innerTransform.takeDerivative(x)
      }
    }
  }

  def takeDerivativeWRTParameters(p: ParameterVector) = {

    val split = splitProductParameterVector(p)
    (x: CV[Float]) => DenseMatrix.horzcat(
        outer.takeDerivativeWRTParameters(split._1)(x), 
        inner.takeDerivativeWRTParameters(split._2)(x))
  }

  private def splitProductParameterVector(p: ParameterVector): (ParameterVector, ParameterVector) = {
      val pThis = p.slice(0, outer.parametersDimensionality, 1) // +1 apparently excludes end
      val pThat = p.slice(outer.parametersDimensionality, p.length, 1)
      (pThis, pThat)
    }

  
}

case class TranslationSpace1D extends TransformationSpace[CoordVector1D] {
  def apply(p: ParameterVector) = {
    new Transformation[CoordVector1D] {

      def apply(pt: Point1D) = p(0) + pt(0)
      def takeDerivative(x: Point1D) = {
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
      def takeDerivative(x: Point2D) = {
        DenseMatrix.eye[Float](2)
      }
    }
  }

  def takeDerivativeWRTParameters(p: ParameterVector) = { x: Point2D =>
    DenseMatrix.eye[Float](2)
  }
}

case class RotationSpace2D(val centre: CoordVector2D[Float]) extends TransformationSpace[CoordVector2D] {

  def parametersDimensionality: Int = 1 //  angle
  def rotationParametersToParameterVector(phi: Float): ParameterVector = {
    DenseVector(phi)
  }
  def apply(p: ParameterVector) = {
    require(p.length == 1)

    val rotMatrix = DenseMatrix((math.cos(p(0)), -math.sin(p(0))), (math.sin(p(0)), math.cos(p(0))))

    new Transformation[CoordVector2D] {
      def apply(pt: Point2D) = {

        val rotCentered = rotMatrix * DenseVector(pt(0) - centre(0), pt(1) - centre(1)).map(_.toDouble)
        CoordVector2D((rotCentered(0) + centre(0)).toFloat, (rotCentered(1) + centre(1)).toFloat)

      }
      def takeDerivative(x: Point2D) = {
        rotMatrix.map(_.toFloat)
      }
    }
  }

  def takeDerivativeWRTParameters(p: ParameterVector) = { x: Point2D =>
    val sa = math.sin(p(0))
    val ca = math.cos(p(0))
    val cx = centre(0)
    val cy = centre(1)

    DenseMatrix(
      (-sa * (x(0) - cx) - ca * (x(1) - cy)).toFloat,
      (ca * (x(0) - cx) - sa * (x(1) - cy)).toFloat)
  }
}

object TransformationSpace {
  type ParameterVector = DenseVector[Float]

}