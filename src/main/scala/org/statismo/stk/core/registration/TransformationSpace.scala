package org.statismo.stk.core
package registration

import TransformationSpace.ParameterVector
import breeze.linalg.DenseVector
import breeze.linalg.DenseMatrix
import org.statismo.stk.core.geometry._

trait Transformation[D <: Dim] extends (Point[D] => Point[D]) {}

trait CanInvert[D <: Dim] {
  self: Transformation[D] =>
  def inverse: Transformation[D]
}

trait CanDifferentiate[D <: Dim] {
  self: Transformation[D] =>
  def takeDerivative(x: Point[D]): MatrixNxN[D]
}

trait TransformationSpace[D <: Dim] {

  type T <: Transformation[D]

  type JacobianImage = Function1[Point[D], DenseMatrix[Float]]

  def parametersDimensionality: Int

  def takeDerivativeWRTParameters(alpha: ParameterVector): JacobianImage

  def transformForParameters(p: ParameterVector): T

  def identityTransformParameters: DenseVector[Float]
}

object TransformationSpace {
  type ParameterVector = DenseVector[Float]

}

trait DifferentiableTransforms[D <: Dim] { self: TransformationSpace[D] =>
  override type T <: Transformation[D] with CanDifferentiate[D]

  def product(that: TransformationSpace[D] with DifferentiableTransforms[D]) = {
    new ProductTransformationSpace(this, that)
  }

}

class ProductTransformationSpace[D <: Dim, OT <: Transformation[D] with CanDifferentiate[D], IT <: Transformation[D] with CanDifferentiate[D]](outer: TransformationSpace[D] with DifferentiableTransforms[D], inner: TransformationSpace[D] with DifferentiableTransforms[D]) extends TransformationSpace[D] with DifferentiableTransforms[D] {

  override type T = ProductTransformation[D]

  def parametersDimensionality = outer.parametersDimensionality + inner.parametersDimensionality

  def identityTransformParameters = DenseVector.vertcat(outer.identityTransformParameters, inner.identityTransformParameters)

  override def transformForParameters(p: ParameterVector) = {
    val (outerParams, innerParams) = splitProductParameterVector(p)
    new ProductTransformation(outer.transformForParameters(outerParams), inner.transformForParameters(innerParams))
  }

  override def takeDerivativeWRTParameters(p: ParameterVector) = {

    val split = splitProductParameterVector(p)

    (x: Point[D]) => {
      DenseMatrix.horzcat(
        outer.takeDerivativeWRTParameters(split._1)(x),
        outer.transformForParameters(split._1).takeDerivative(inner.transformForParameters(split._2)(x)).toBreezeMatrix * inner.takeDerivativeWRTParameters(split._2)(x))
    }
  }

  protected def splitProductParameterVector(p: ParameterVector): (ParameterVector, ParameterVector) = {
    val pThis = p.slice(0, outer.parametersDimensionality, 1)
    val pThat = p.slice(outer.parametersDimensionality, p.length, 1)
    (pThis, pThat)
  }

}

class ProductTransformation[D <: Dim](outerTransform: Transformation[D] with CanDifferentiate[D], innerTransform: Transformation[D] with CanDifferentiate[D]) extends Transformation[D] with CanDifferentiate[D] {
  override def apply(x: Point[D]) = {
    (outerTransform compose innerTransform)(x)
  }

  override def takeDerivative(x: Point[D]) = {
    outerTransform.takeDerivative(innerTransform(x)) * innerTransform.takeDerivative(x)
  }
}

class TranslationSpace[D <: Dim: DimOps] extends TransformationSpace[D] with DifferentiableTransforms[D] {

  override type T = TranslationTransform[D]

  def parametersDimensionality: Int = implicitly[DimOps[D]].toInt
  override def identityTransformParameters = DenseVector.zeros(parametersDimensionality)

  override def transformForParameters(p: ParameterVector): TranslationTransform[D] = TranslationTransform(new Vector[D](p.data))

  override def takeDerivativeWRTParameters(p: ParameterVector) = { x: Point[D] => DenseMatrix.eye[Float](parametersDimensionality) }
}

object TranslationSpace {
  def apply[D <: Dim: DimOps] = new TranslationSpace[D]

}

class TranslationTransform[D <: Dim: DimOps] private (t: Vector[D]) extends Transformation[D] with CanInvert[D] with CanDifferentiate[D] {
  def apply(pt: Point[D]): Point[D] = pt + t
  override def takeDerivative(x: Point[D]): MatrixNxN[D] = MatrixNxN.eye[D]
  override def inverse: TranslationTransform[D] = new TranslationTransform(t * (-1f))
}

object TranslationTransform { def apply[D <: Dim: DimOps](t: Vector[D]) = new TranslationTransform[D](t) }

class RotationSpace[D <: Dim: DimOps] private (centre: Point[D]) extends TransformationSpace[D] with DifferentiableTransforms[D] {

  override type T = RotationTransform[D]

  def parametersDimensionality: Int = implicitly[DimOps[D]].toInt match {
    case 2 => 1
    case 3 => 3
  }

  //  Euler angles
  override def identityTransformParameters = DenseVector.zeros[Float](parametersDimensionality)

  override def transformForParameters(p: ParameterVector): RotationTransform[D] = {
    require(p.length == parametersDimensionality)

    val rotMatrix =  implicitly[DimOps[D]].toInt match {
      case 3 => {
        // rotation matrix according to the "x-convention" where Phi is rotation over x-axis, theta over y, and psi over z
        val cospsi = Math.cos(p(2)).toFloat
        val sinpsi = Math.sin(p(2)).toFloat

        val costh = Math.cos(p(1)).toFloat
        val sinth = Math.sin(p(1)).toFloat

        val cosphi = Math.cos(p(0)).toFloat
        val sinphi = Math.sin(p(0)).toFloat

        MatrixNxN(
          (costh * cosphi, sinpsi * sinth * cosphi - cospsi * sinphi, sinpsi * sinphi + cospsi * sinth * cosphi),
          (costh * sinphi, cospsi * cosphi + sinpsi * sinth * sinphi, cospsi * sinth * sinphi - sinpsi * cosphi),
          (-sinth, sinpsi * costh, cospsi * costh))

      }
      case 2 => {
        MatrixNxN(
          (math.cos(p(0)).toFloat, -math.sin(p(0)).toFloat),
          (math.sin(p(0)).toFloat, math.cos(p(0)).toFloat))
      }
    }

    RotationTransform[D](rotMatrix.asInstanceOf[MatrixNxN[D]], centre)
  }

  override def takeDerivativeWRTParameters(p: ParameterVector) = {

    val df = implicitly[DimOps[D]].toInt match {
      case 3 => (x: Point[_3D]) => {
        val cospsi = Math.cos(p(2))
        val sinpsi = Math.sin(p(2))
        val costh = Math.cos(p(1))
        val sinth = Math.sin(p(1))
        val cosphi = Math.cos(p(0))
        val sinphi = Math.sin(p(0))

        val x0minc0 = x(0) - centre(0)
        val x1minc1 = x(1) - centre(1)
        val x2minc2 = x(2) - centre(2)

        // 3 by 3 matrix (nbrows=point dim, nb cols = param dim )
        val dr00 = (-sinphi * costh * x0minc0) + (-sinphi * sinpsi * sinth - cospsi * cosphi) * x1minc1 + (sinpsi * cosphi - cospsi * sinth * sinphi) * x2minc2
        val dr01 = -sinth * cosphi * x0minc0 + costh * sinpsi * cosphi * x1minc1 + cospsi * costh * cosphi * x2minc2
        val dr02 = (cospsi * sinth * cosphi + sinpsi * sinphi) * x1minc1 + (cospsi * sinphi - sinpsi * sinth * cosphi) * x2minc2

        val dr10 = costh * cosphi * x0minc0 + (-sinphi * cospsi + sinpsi * sinth * cosphi) * x1minc1 + (cospsi * sinth * cosphi + sinpsi * sinphi) * x2minc2
        val dr11 = -sinth * sinphi * x0minc0 + sinpsi * costh * sinphi * x1minc1 + cospsi * costh * sinphi * x2minc2
        val dr12 = (-sinpsi * cosphi + cospsi * sinth * sinphi) * x1minc1 + (-sinpsi * sinth * sinphi - cospsi * cosphi) * x2minc2

        val dr20 = 0.0
        val dr21 = -costh * x0minc0 - sinpsi * sinth * x1minc1 - cospsi * sinth * x2minc2
        val dr22 = cospsi * costh * x1minc1 - sinpsi * costh * x2minc2

        DenseMatrix(
          (dr00, dr01, dr02),
          (dr10, dr11, dr12),
          (dr20, dr21, dr22)).map(_.toFloat)

      }

      case 2 => (x: Point[_2D]) => {
        val sa = math.sin(p(0))
        val ca = math.cos(p(0))
        val cx = centre(0)
        val cy = centre(1)

        DenseMatrix(
          (-sa * (x(0) - cx) - ca * (x(1) - cy)),
          (ca * (x(0) - cx) - sa * (x(1) - cy))).map(_.toFloat)
      }
    }

    df.asInstanceOf[Point[D] => DenseMatrix[Float]]
  }

}
object RotationSpace {
  def apply[D <: Dim: DimOps](centre: Point[D]) = new RotationSpace[D](centre)

  // with origin as default centre
  def apply[D <: Dim: DimOps]() = {
    val origin = Point[D](DenseVector.zeros[Float](implicitly[DimOps[D]].toInt).data)
    new RotationSpace[D](origin)
  }

}

class RotationTransform[D <: Dim: DimOps] private (rotMatrix: MatrixNxN[D], centre: Point[D]) extends Transformation[D] with CanInvert[D] with CanDifferentiate[D] {
  def apply(pt: Point[D]): Point[D] = {
    val ptCentered = pt - centre

    val rotCentered = rotMatrix * ptCentered
    centre + rotCentered
  }

  def takeDerivative(x: Point[D]): MatrixNxN[D] = {
    rotMatrix
  }

  override def inverse: RotationTransform[D] = {
    new RotationTransform[D](MatrixNxN.inv(rotMatrix), centre)
  }
}

object RotationTransform {

  def apply[D <: Dim: DimOps](rotMatrix: MatrixNxN[D], centre: Point[D]) = new RotationTransform[D](rotMatrix, centre)

  def apply[D <: Dim: DimOps](rotMatrix: MatrixNxN[D]) = {
    val centre = Point[D](DenseVector.zeros[Float](implicitly[DimOps[D]].toInt).data)
    new RotationTransform[D](rotMatrix, centre)
  }
}

class ScalingSpace[D <: Dim: DimOps] extends TransformationSpace[D] with DifferentiableTransforms[D] {

  override type T = ScalingTransformation[D]

  def parametersDimensionality: Int = 1

  override def identityTransformParameters = DenseVector(1f)

  override def transformForParameters(p: ParameterVector): ScalingTransformation[D] = {
    require(p.length == parametersDimensionality)
    ScalingTransformation[D](p(0))
  }

  override def takeDerivativeWRTParameters(p: ParameterVector) = {
    val df = implicitly[DimOps[D]].toInt match {
      case 3 => x: Point[_3D] => DenseMatrix((x(0)), (x(1)), (x(2)))
      case 2 => x: Point[_2D] => DenseMatrix((x(0)), (x(1)))
    }
    df.asInstanceOf[Point[D] => DenseMatrix[Float]]
  }
}

object ScalingSpace {
  def apply[D <: Dim: DimOps] = new ScalingSpace[D]
}

class ScalingTransformation[D <: Dim: DimOps] private (s: Float) extends Transformation[D] with CanInvert[D] with CanDifferentiate[D] {
  def apply(x: Point[D]): Point[D] = (x.toVector * s).toPoint

  def takeDerivative(x: Point[D]): MatrixNxN[D] = MatrixNxN.eye[D] * s

  override def inverse: ScalingTransformation[D] = {
    if (s == 0) new ScalingTransformation[D](0) else new ScalingTransformation[D](1.0f / s)
  }
}

object ScalingTransformation {
  def apply[D <: Dim: DimOps](s: Float) = new ScalingTransformation[D](s)
}

class RigidTransformationSpace[D <: Dim: DimOps] private (center: Point[D])
  extends ProductTransformationSpace[D, TranslationTransform[D], RotationTransform[D]](TranslationSpace[D], RotationSpace[D](center)) {

  override def transformForParameters(p: ParameterVector): RigidTransformation[D] = {
    val (outerParams, innerParams) = splitProductParameterVector(p)
    new RigidTransformationRotThenTrans[D](TranslationSpace[D].transformForParameters(outerParams), RotationSpace[D](center).transformForParameters(innerParams))
  }
}

object RigidTransformationSpace {

  def apply[D <: Dim: DimOps](center: Point[D]) = new RigidTransformationSpace[D](center)
  def apply[D <: Dim: DimOps]() = {
    val origin = Point[D](DenseVector.zeros[Float](implicitly[DimOps[D]].toInt).data)
    new RigidTransformationSpace[D](origin)
  }

}

// there are different possibilities to define rigid transformations. Either we first do a translation and then a rotation,
// or vice versa. We support both (and the inverse is always the other case).
trait RigidTransformation[D <: Dim] extends ProductTransformation[D] with CanInvert[D]

object RigidTransformation {
  def apply[D <: Dim: DimOps](translationTransform: TranslationTransform[D], rotationTransform: RotationTransform[D]): RigidTransformation[D] = new RigidTransformationRotThenTrans(translationTransform, rotationTransform)
  def apply[D <: Dim: DimOps](rotationTransform: RotationTransform[D], translationTransform: TranslationTransform[D]): RigidTransformation[D] = new RigidTransformationTransThenRot(rotationTransform, translationTransform)
}

private class RigidTransformationRotThenTrans[D <: Dim: DimOps](translationTransform: TranslationTransform[D], rotationTransform: RotationTransform[D])
  extends ProductTransformation[D](translationTransform, rotationTransform) with RigidTransformation[D] {

  def inverse: RigidTransformation[D] = new RigidTransformationTransThenRot[D](rotationTransform.inverse, translationTransform.inverse)
}

private class RigidTransformationTransThenRot[D <: Dim: DimOps](rotationTransform: RotationTransform[D], translationTransform: TranslationTransform[D])
  extends ProductTransformation[D](translationTransform, rotationTransform) with RigidTransformation[D] {
  def inverse: RigidTransformation[D] = new RigidTransformationRotThenTrans[D](translationTransform.inverse, rotationTransform.inverse)
}


