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

  type JacobianImage = Point[D] => DenseMatrix[Float]

  def parametersDimensionality: Int

  def takeDerivativeWRTParameters(alpha: ParameterVector): JacobianImage

  def transformForParameters(p: ParameterVector): T

  def apply(p: ParameterVector): T = transformForParameters(p)

  def identityTransformParameters: DenseVector[Float]
}

object TransformationSpace {
  type ParameterVector = DenseVector[Float]
}

trait DifferentiableTransforms[D <: Dim] {
  self: TransformationSpace[D] =>
  override type T <: Transformation[D] with CanDifferentiate[D]

  def product(that: TransformationSpace[D] with DifferentiableTransforms[D]) = {
    new ProductTransformationSpace(this, that)
  }
}


class ProductTransformationSpace[D <: Dim, OT <: Transformation[D] with CanDifferentiate[D], IT <: Transformation[D] with CanDifferentiate[D]]
(outer: TransformationSpace[D] with DifferentiableTransforms[D], inner: TransformationSpace[D] with DifferentiableTransforms[D]) extends TransformationSpace[D] with DifferentiableTransforms[D] {

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
        outer.transformForParameters(split._1).takeDerivative(inner(split._2)(x)).toBreezeMatrix * inner.takeDerivativeWRTParameters(split._2)(x))
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

case class TranslationSpace1D() extends TransformationSpace[_1D] with DifferentiableTransforms[_1D] {

  override type T = TranslationTransform1D

  def parametersDimensionality: Int = 1
  override def identityTransformParameters = DenseVector(0.0f)

  override def transformForParameters(p: ParameterVector): TranslationTransform1D = new TranslationTransform1D(Vector(p(0)))

  override def takeDerivativeWRTParameters(p: ParameterVector) = {
    x: Point[_1D] => DenseMatrix.eye[Float](1)
  }
}

case class TranslationTransform1D(t: Vector[_1D]) extends Transformation[_1D] with CanInvert[_1D] with CanDifferentiate[_1D] {
  def apply(pt: Point[_1D]): Point[_1D] = pt + t

  override def takeDerivative(x: Point[_1D]): MatrixNxN[_1D] = MatrixNxN.eye[_1D]
  override def inverse: TranslationTransform1D = TranslationTransform1D(t * (-1f))
}


case class TranslationSpace2D() extends TransformationSpace[_2D] with DifferentiableTransforms[_2D] {

  override type T = TranslationTransform2D

  def parametersDimensionality: Int = 2
  override def identityTransformParameters = DenseVector(0.0f, 0.0f)
  override def transformForParameters(p: ParameterVector): TranslationTransform2D = new TranslationTransform2D(Vector(p(0), p(1)))
  override def takeDerivativeWRTParameters(p: ParameterVector) = {
    x: Point[_2D] => DenseMatrix.eye[Float](2)
  }
}

case class TranslationTransform2D(t: Vector[_2D]) extends Transformation[_2D] with CanInvert[_2D] with CanDifferentiate[_2D] {
  def apply(pt: Point[_2D]) = pt + t
  override def takeDerivative(x: Point[_2D]): MatrixNxN[_2D] = MatrixNxN.eye[_2D]
  override def inverse = TranslationTransform2D(t * (-1f))
}

case class TranslationSpace3D() extends TransformationSpace[_3D] with DifferentiableTransforms[_3D] {

  override type T = TranslationTransform3D

  def parametersDimensionality: Int = 3
  override def identityTransformParameters = DenseVector(0.0f, 0.0f, 0.0f)
  override def transformForParameters(p: ParameterVector) = new TranslationTransform3D(Vector(p(0), p(1), p(2)))
  override def takeDerivativeWRTParameters(p: ParameterVector) = {
    x: Point[_3D] => DenseMatrix.eye[Float](3)
  }
}

case class TranslationTransform3D(t: Vector[_3D]) extends Transformation[_3D] with CanInvert[_3D] with CanDifferentiate[_3D] {
  def apply(pt: Point[_3D]) = pt + t
  override def takeDerivative(x: Point[_3D]) = MatrixNxN.eye[_3D]
  override def inverse = TranslationTransform3D(t * (-1f))
}


case class RotationSpace3D(centre: Point[_3D]) extends TransformationSpace[_3D] with DifferentiableTransforms[_3D] {

  override type T = RotationTransform3D

  def parametersDimensionality: Int = 3

  //  Euler angles
  override def identityTransformParameters = DenseVector(0f, 0f, 0f)

  def rotationParametersToParameterVector(phi: Double, theta: Double, psi: Double): ParameterVector = {
    DenseVector(phi.toFloat, theta.toFloat, psi.toFloat)
  }

  override def transformForParameters(p: ParameterVector): RotationTransform3D = {
    require(p.length == 3)
    //
    // rotation matrix according to the "x-convention" where Phi is rotation over x-axis, theta over y, and psi over z
    val cospsi = Math.cos(p(2)).toFloat
    val sinpsi = Math.sin(p(2)).toFloat

    val costh = Math.cos(p(1)).toFloat
    val sinth = Math.sin(p(1)).toFloat

    val cosphi = Math.cos(p(0)).toFloat
    val sinphi = Math.sin(p(0)).toFloat

    val rotMatrix = MatrixNxN(
      (costh * cosphi, sinpsi * sinth * cosphi - cospsi * sinphi, sinpsi * sinphi + cospsi * sinth * cosphi),
      (costh * sinphi, cospsi * cosphi + sinpsi * sinth * sinphi, cospsi * sinth * sinphi - sinpsi * cosphi),
      (-sinth, sinpsi * costh, cospsi * costh)
    )

    new RotationTransform3D(rotMatrix, centre)
  }


  override def takeDerivativeWRTParameters(p: ParameterVector) = {
    x: Point[_3D] =>

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

      //    val dr00 = (cospsi*sinth*cosphi+sinpsi*sinphi)*x1minc1+(cospsi*sinphi-sinpsi*sinth*cosphi)*x2minc2
      //    val dr01 = (-sinth*cosphi*x0minc0 + costh*cosphi*sinpsi*x1minc1 + cospsi*costh*cosphi* x2minc2)
      //    val dr02 = (-sinphi*costh*x0minc0)+ (-sinphi*sinth*sinpsi - cospsi*cosphi) * x1minc1 + (cosphi*sinpsi-sinphi*sinth*cospsi)* x2minc2
      //    val dr10 = (-sinpsi*cosphi+cospsi*sinth*sinphi)*x1minc1+ (-sinpsi*sinth*sinphi-cospsi*cosphi)*x2minc2
      //    val dr11 = (-sinth*sinphi*x0minc0 + costh*sinpsi*sinphi*x1minc1+cospsi*costh*sinphi*x2minc2)
      //    val dr12 = costh*cosphi*x0minc0+(-sinphi*cospsi+cosphi*sinpsi*sinth)*x1minc1+(cosphi*sinth*cospsi+sinpsi*sinphi)*x2minc2
      //    val dr20 = cospsi*costh*x1minc1
      //    val dr21 = -costh*x0minc0 + -sinth*sinpsi*x1minc1 + (-sinth*cospsi)*x2minc2
      //    val dr22 = 0.

      DenseMatrix(
        (dr00, dr01, dr02),
        (dr10, dr11, dr12),
        (dr20, dr21, dr22)
      ).map(_.toFloat)
  }
}


class RotationTransform3D(rotMatrix: MatrixNxN[_3D], centre: Point[_3D] = Point(0, 0, 0)) extends Transformation[_3D] with CanInvert[_3D] with CanDifferentiate[_3D] {
  def apply(pt: Point[_3D]): Point[_3D] = {
    val ptCentered = pt - centre
    val rotCentered = rotMatrix * ptCentered
    centre + Vector(rotCentered(0), rotCentered(1), rotCentered(2))
  }

  def takeDerivative(x: Point[_3D]): MatrixNxN[_3D] = {
    rotMatrix
  }

  override def inverse: RotationTransform3D = {
    new RotationTransform3D(MatrixNxN.inv(rotMatrix), centre)
  }
}


case class RotationSpace2D(centre: Point[_2D]) extends TransformationSpace[_2D] with DifferentiableTransforms[_2D] {

  override type T = RotationTransform2D

  def parametersDimensionality: Int = 1

  override def identityTransformParameters = DenseVector(0f)


  def rotationParametersToParameterVector(phi: Double): ParameterVector = {
    DenseVector(phi.toFloat)
  }

  override def transformForParameters(p: ParameterVector): RotationTransform2D = {
    require(p.length == 1)

    val rotMatrix = MatrixNxN(
      (math.cos(p(0)).toFloat, -math.sin(p(0)).toFloat),
      (math.sin(p(0)).toFloat, math.cos(p(0)).toFloat)
    )
    new RotationTransform2D(rotMatrix, centre)
  }


  override def takeDerivativeWRTParameters(p: ParameterVector) = {
    x: Point[_2D] =>
      val sa = math.sin(p(0))
      val ca = math.cos(p(0))
      val cx = centre(0)
      val cy = centre(1)

      DenseMatrix(
        -sa * (x(0) - cx) - ca * (x(1) - cy),
        ca * (x(0) - cx) - sa * (x(1) - cy)
      ).map(_.toFloat)
  }
}


class RotationTransform2D(rotMatrix: MatrixNxN[_2D], centre: Point[_2D] = Point(0, 0)) extends Transformation[_2D] with CanInvert[_2D] with CanDifferentiate[_2D] {
  def apply(pt: Point[_2D]): Point[_2D] = {
    val ptCentered = pt - centre
    val rotCentered = rotMatrix * ptCentered
    centre + Vector(rotCentered(0), rotCentered(1))
  }

  def takeDerivative(x: Point[_2D]): MatrixNxN[_2D] = {
    rotMatrix
  }

  override def inverse: RotationTransform2D = {
    new RotationTransform2D(MatrixNxN.inv(rotMatrix), centre)
  }
}


case class ScalingSpace3D() extends TransformationSpace[_3D] with DifferentiableTransforms[_3D] {

  override type T = ScalingTransformation3D

  def parametersDimensionality: Int = 1

  override def identityTransformParameters = DenseVector(1f)

  override def transformForParameters(p: ParameterVector): ScalingTransformation3D = {
    require(p.length == 1)
    new ScalingTransformation3D(p(0))
  }

  override def takeDerivativeWRTParameters(p: ParameterVector) = {
    x: Point[_3D] => DenseMatrix(x(0), x(1), x(2))
  }
}


class ScalingTransformation3D(s: Float) extends Transformation[_3D] with CanInvert[_3D] with CanDifferentiate[_3D] {
  def apply(x: Point[_3D]): Point[_3D] = Point(x(0) * s, x(1) * s, x(2) * s)

  def takeDerivative(x: Point[_3D]): MatrixNxN[_3D] = MatrixNxN.eye[_3D] * s

  override def inverse: ScalingTransformation3D = {
    if (s == 0) new ScalingTransformation3D(0) else new ScalingTransformation3D(1.0f / s)
  }
}


case class ScalingSpace2D() extends TransformationSpace[_2D] with DifferentiableTransforms[_2D] {

  override type T = ScalingTransformation2D

  def parametersDimensionality: Int = 1

  override def identityTransformParameters = DenseVector(1f)

  override def transformForParameters(p: ParameterVector): ScalingTransformation2D = {
    require(p.length == 1)
    new ScalingTransformation2D(p(0))
  }

  override def takeDerivativeWRTParameters(p: ParameterVector) = {
    x: Point[_2D] => DenseMatrix(x(0), x(1))
  }
}


class ScalingTransformation2D(s: Float) extends Transformation[_2D] with CanInvert[_2D] with CanDifferentiate[_2D] {
  def apply(x: Point[_2D]): Point[_2D] = Point(x(0) * s, x(1) * s)

  def takeDerivative(x: Point[_2D]): MatrixNxN[_2D] = MatrixNxN.eye[_2D] * s

  override def inverse: ScalingTransformation2D = {
    if (s == 0) new ScalingTransformation2D(0) else new ScalingTransformation2D(1.0f / s)
  }
}


case class RigidTransformationSpace3D(center: Point[_3D] = Point(0, 0, 0))
  extends ProductTransformationSpace[_3D, TranslationTransform3D, RotationTransform3D](TranslationSpace3D(), RotationSpace3D(center)) {

  override def transformForParameters(p: ParameterVector): RigidTransformation3D = {
    val (outerParams, innerParams) = splitProductParameterVector(p)
    new RigidTransformation3DRotThenTrans(TranslationSpace3D().transformForParameters(outerParams), RotationSpace3D(center).transformForParameters(innerParams))
  }
}

// there are different possibilities to define rigid transformations. Either we first do a translation and then a rotation,
// or vice versa. We support both (and the inverse is always the other case).
trait RigidTransformation[D <: Dim] extends ProductTransformation[D] with CanInvert[D]
trait RigidTransformation3D extends RigidTransformation[_3D]
trait RigidTransformation2D extends RigidTransformation[_2D]

object RigidTransformation3D {
  def apply(translationTransform: TranslationTransform3D, rotationTransform: RotationTransform3D): RigidTransformation3D = new RigidTransformation3DRotThenTrans(translationTransform, rotationTransform)
  def apply(rotationTransform: RotationTransform3D, translationTransform: TranslationTransform3D): RigidTransformation3D = new RigidTransformation3DTransThenRot(rotationTransform, translationTransform)
}

private class RigidTransformation3DRotThenTrans(translationTransform: TranslationTransform3D, rotationTransform: RotationTransform3D)
  extends ProductTransformation[_3D](translationTransform, rotationTransform) with RigidTransformation3D {

  def inverse: RigidTransformation[_3D] = new RigidTransformation3DTransThenRot(rotationTransform.inverse, translationTransform.inverse)
}

private class RigidTransformation3DTransThenRot(rotationTransform: RotationTransform3D, translationTransform: TranslationTransform3D)
  extends ProductTransformation[_3D](translationTransform, rotationTransform) with RigidTransformation3D {
  def inverse: RigidTransformation[_3D] = new RigidTransformation3DRotThenTrans(translationTransform.inverse, rotationTransform.inverse)
}


object RigidTransformations2D {
  def apply(translationTransform: TranslationTransform2D, rotationTransform: RotationTransform2D): RigidTransformation2D = new RigidTransformation2DRotThenTrans(translationTransform, rotationTransform)
  def apply(rotationTransform: RotationTransform2D, translationTransform: TranslationTransform2D): RigidTransformation2D = new RigidTransformation2DTransThenRot(rotationTransform, translationTransform)
}

case class RigidTransformationSpace2D(center: Point[_2D] = Point(0, 0))
  extends ProductTransformationSpace[_2D, TranslationTransform2D, RotationTransform2D](TranslationSpace2D(), RotationSpace2D(center)) {


  override def transformForParameters(p: ParameterVector): RigidTransformation[_2D] = {
    val (outerParams, innerParams) = splitProductParameterVector(p)
    new RigidTransformation2DRotThenTrans(TranslationSpace2D().transformForParameters(outerParams), RotationSpace2D(center).transformForParameters(innerParams))
  }
}


private class RigidTransformation2DRotThenTrans(translationTransform: TranslationTransform2D, rotationTransform: RotationTransform2D)
  extends ProductTransformation(translationTransform, rotationTransform) with RigidTransformation2D {

  def inverse: RigidTransformation[_2D] = new RigidTransformation2DTransThenRot(rotationTransform.inverse, translationTransform.inverse)
}

private class RigidTransformation2DTransThenRot(rotationTransform: RotationTransform2D, translationTransform: TranslationTransform2D)
  extends ProductTransformation(translationTransform, rotationTransform) with RigidTransformation2D {
  def inverse: RigidTransformation[_2D] = new RigidTransformation2DRotThenTrans(translationTransform.inverse, rotationTransform.inverse)
}
