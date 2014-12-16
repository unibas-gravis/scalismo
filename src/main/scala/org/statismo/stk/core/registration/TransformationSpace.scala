package org.statismo.stk.core
package registration

import TransformationSpace.ParameterVector
import breeze.linalg.DenseVector
import breeze.linalg.DenseMatrix
import org.statismo.stk.core.geometry._
import java.util.Arrays

trait Transformation[D <: Dim] extends (Point[D] => Point[D]) {}
trait ParametricTransformation[D <: Dim] extends Transformation[D] {
  val parameters: TransformationSpace.ParameterVector
}

trait CanInvert[D <: Dim] {
  self: Transformation[D] =>
  def inverse: Transformation[D]
}

trait CanDifferentiate[D <: Dim] {
  self: Transformation[D] =>
  def takeDerivative(x: Point[D]): MatrixNxN[D]
}

trait TransformationSpace[D <: Dim] {

  type T <: ParametricTransformation[D]

  type JacobianImage = Point[D] => DenseMatrix[Float]

  def parametersDimensionality: Int

  def takeDerivativeWRTParameters(alpha: ParameterVector): JacobianImage

  def transformForParameters(p: ParameterVector): T

  def identityTransformParameters: DenseVector[Float]
}

object TransformationSpace {
  type ParameterVector = DenseVector[Float]
}

trait DifferentiableTransforms[D <: Dim] { self: TransformationSpace[D] =>
  override type T <: ParametricTransformation[D] with CanDifferentiate[D]

  def product(that: TransformationSpace[D] with DifferentiableTransforms[D]) = {
    new ProductTransformationSpace(this, that)
  }
}

class ProductTransformationSpace[D <: Dim, OT <: ParametricTransformation[D] with CanDifferentiate[D], IT <: ParametricTransformation[D] with CanDifferentiate[D]](outer: TransformationSpace[D] with DifferentiableTransforms[D], inner: TransformationSpace[D] with DifferentiableTransforms[D]) extends TransformationSpace[D] with DifferentiableTransforms[D] {

  override type T = ProductTransformation[D]

  def parametersDimensionality = outer.parametersDimensionality + inner.parametersDimensionality

  def identityTransformParameters = DenseVector.vertcat(outer.identityTransformParameters, inner.identityTransformParameters)

  override def transformForParameters(p: ParameterVector) = {
    val (outerParams, innerParams) = splitProductParameterVector(p)
    val v = outer.transformForParameters(outerParams)

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
   
    val pthisD = DenseVector(p.data.take(outer.parametersDimensionality)) 
    val pthatD = DenseVector(p.data.drop(outer.parametersDimensionality))
   
    (pthisD, pthatD)
  }
}

case class ProductTransformation[D <: Dim](outerTransform: ParametricTransformation[D] with CanDifferentiate[D], innerTransform: ParametricTransformation[D] with CanDifferentiate[D]) extends ParametricTransformation[D] with CanDifferentiate[D] {

  override def apply(x: Point[D]) = {
    (outerTransform compose innerTransform)(x)
  }

  override def takeDerivative(x: Point[D]) = {
    outerTransform.takeDerivative(innerTransform(x)) * innerTransform.takeDerivative(x)
  }

  override val parameters = DenseVector(outerTransform.parameters.data ++ innerTransform.parameters.data)

}

class TranslationSpace[D <: Dim: DimOps] extends TransformationSpace[D] with DifferentiableTransforms[D] {

  override type T = TranslationTransform[D]

  def parametersDimensionality: Int = implicitly[DimOps[D]].toInt
  override def identityTransformParameters = DenseVector.zeros(parametersDimensionality)

  override def transformForParameters(p: ParameterVector): TranslationTransform[D] = TranslationTransform[D](Vector[D](p.data))

  override def takeDerivativeWRTParameters(p: ParameterVector) = { x: Point[D] => DenseMatrix.eye[Float](parametersDimensionality) }
}

object TranslationSpace {
  def apply[D <: Dim: DimOps] = new TranslationSpace[D]

}

case class TranslationTransform[D <: Dim: DimOps](t: Vector[D]) extends ParametricTransformation[D] with CanInvert[D] with CanDifferentiate[D] {
  def apply(pt: Point[D]): Point[D] = pt + t
  override def takeDerivative(x: Point[D]): MatrixNxN[D] = MatrixNxN.eye[D]
  override def inverse: TranslationTransform[D] = new TranslationTransform(t * (-1f))
  val parameters = t.toBreezeVector
}

abstract class RotationSpace[D <: Dim: DimOps] extends TransformationSpace[D] with DifferentiableTransforms[D] {

  def centre: Point[D]

  override type T = RotationTransform[D]

  //  We use Euler angles, hence a 0 angle means no rotation (identity)
  override def identityTransformParameters = DenseVector.zeros[Float](parametersDimensionality)
}

private class RotationSpace2D(val centre: Point[_2D]) extends RotationSpace[_2D] {

  override def parametersDimensionality: Int = 1

  override def transformForParameters(p: ParameterVector): RotationTransform[_2D] = {
    require(p.length == parametersDimensionality)

    val rotMatrix = MatrixNxN(
      (math.cos(p(0)).toFloat, -math.sin(p(0)).toFloat),
      (math.sin(p(0)).toFloat, math.cos(p(0)).toFloat))

    RotationTransform[_2D](rotMatrix, centre)
  }

  override def takeDerivativeWRTParameters(p: ParameterVector) = {

    val df = (x: Point[_2D]) => {
      val sa = math.sin(p(0))
      val ca = math.cos(p(0))
      val cx = centre(0)
      val cy = centre(1)

      DenseMatrix(
        -sa * (x(0) - cx) - ca * (x(1) - cy),
        ca * (x(0) - cx) - sa * (x(1) - cy)).map(_.toFloat)
    }
    df
  }
}

private class RotationSpace3D(val centre: Point[_3D]) extends RotationSpace[_3D] {
  override def parametersDimensionality: Int = 3

  override def transformForParameters(p: ParameterVector): RotationTransform[_3D] = {
    require(p.length == parametersDimensionality)

    val rotMatrix = {
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

    RotationTransform[_3D](rotMatrix, centre)
  }

  override def takeDerivativeWRTParameters(p: ParameterVector) = {

    val df = (x: Point[_3D]) => {
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
    df
  }
}

object RotationSpace {

  trait Create[D <: Dim] {
    def createRotationSpace(centre: Point[D]): RotationSpace[D]
  }

  implicit object createRotationSpace2D extends Create[_2D] {
    override def createRotationSpace(centre: Point[_2D]): RotationSpace[_2D] = new RotationSpace2D(centre)
  }

  implicit object createRotationSpace3D extends Create[_3D] {
    override def createRotationSpace(centre: Point[_3D]): RotationSpace[_3D] = new RotationSpace3D(centre)
  }

  def apply[D <: Dim](centre: Point[D])(implicit evDim: DimOps[D], evCreateRot: Create[D]) = {
    evCreateRot.createRotationSpace(centre)
  }

  // with origin as default centre
  def apply[D <: Dim]()(implicit evDim: DimOps[D], evCreateRot: Create[D]) = {
    val origin = Point[D](DenseVector.zeros[Float](implicitly[DimOps[D]].toInt).data)
    evCreateRot.createRotationSpace(origin)

  }
}

abstract class RotationTransform[D <: Dim: DimOps] extends ParametricTransformation[D] with CanInvert[D] with CanDifferentiate[D] {
  override def inverse: RotationTransform[D]
}

private case class RotationTransform3D(rotMatrix: MatrixNxN[_3D], centre: Point[_3D] = Point(0, 0, 0)) extends RotationTransform[_3D] {
  def apply(pt: Point[_3D]): Point[_3D] = {
    val ptCentered = pt - centre
    val rotCentered = rotMatrix * ptCentered
    centre + Vector(rotCentered(0).toFloat, rotCentered(1).toFloat, rotCentered(2).toFloat)
  }

  val parameters = LandmarkRegistration.rotMatrixToEulerAngles(rotMatrix.toBreezeMatrix.map(_.toDouble)).map(_.toFloat)

  def takeDerivative(x: Point[_3D]): MatrixNxN[_3D] = {
    rotMatrix
  }

  override def inverse: RotationTransform3D = {
    new RotationTransform3D(MatrixNxN.inv(rotMatrix), centre)
  }
}

private case class RotationTransform2D(rotMatrix: MatrixNxN[_2D], centre: Point[_2D] = Point(0, 0)) extends RotationTransform[_2D] {
  def apply(pt: Point[_2D]): Point[_2D] = {
    val ptCentered = pt - centre
    val rotCentered = rotMatrix * ptCentered
    centre + Vector(rotCentered(0).toFloat, rotCentered(1).toFloat)

  }
  val parameters = DenseVector(LandmarkRegistration.rotationMatrixToAngle2D(rotMatrix.toBreezeMatrix.map(_.toDouble)).toFloat)
  def takeDerivative(x: Point[_2D]): MatrixNxN[_2D] = {
    rotMatrix
  }

  override def inverse: RotationTransform2D = {
    new RotationTransform2D(MatrixNxN.inv(rotMatrix), centre)
  }
}

object RotationTransform {

  trait Create[D <: Dim] {
    def createRotationTransform(rotMatrix: MatrixNxN[D], centre: Point[D]): RotationTransform[D]
  }

  implicit object createRotationTransform2D extends Create[_2D] {
    override def createRotationTransform(rotMatrix: MatrixNxN[_2D], centre: Point[_2D]): RotationTransform[_2D] = new RotationTransform2D(rotMatrix, centre)
  }

  implicit object createRotationTransform3D extends Create[_3D] {
    override def createRotationTransform(rotMatrix: MatrixNxN[_3D], centre: Point[_3D]): RotationTransform[_3D] = new RotationTransform3D(rotMatrix, centre)
  }

  def apply[D <: Dim](rotMatrix: MatrixNxN[D], centre: Point[D])(implicit evDim: DimOps[D], evCreateRot: Create[D]) = {
    evCreateRot.createRotationTransform(rotMatrix, centre)
  }

  def apply[D <: Dim](rotMatrix: MatrixNxN[D])(implicit evDim: DimOps[D], evCreateRot: Create[D]) = {
    val centre = Point[D](DenseVector.zeros[Float](implicitly[DimOps[D]].toInt).data)
    evCreateRot.createRotationTransform(rotMatrix, centre)
  }

}

abstract class ScalingSpace[D <: Dim: DimOps] extends TransformationSpace[D] with DifferentiableTransforms[D] {

  override type T = ScalingTransformation[D]

  def parametersDimensionality: Int = 1

  override def identityTransformParameters = DenseVector(1f)

  override def transformForParameters(p: ParameterVector): ScalingTransformation[D] = {
    require(p.length == parametersDimensionality)
    ScalingTransformation[D](p(0))
  }
}

private class ScalingSpace3D extends ScalingSpace[_3D] {
  override def takeDerivativeWRTParameters(p: ParameterVector) = { x: Point[_3D] => DenseMatrix(x(0), x(1), x(2)) }
}

private class ScalingSpace2D extends ScalingSpace[_2D] {
  override def takeDerivativeWRTParameters(p: ParameterVector) = { x: Point[_2D] => DenseMatrix(x(0), x(1)) }
}

private class ScalingSpace1D extends ScalingSpace[_1D] {
  override def takeDerivativeWRTParameters(p: ParameterVector) = { x: Point[_1D] => DenseMatrix(x(0)) }
}

object ScalingSpace {

  trait Create[D <: Dim] {
    def createScalingSpace: ScalingSpace[D]
  }
  implicit object createScalingSpace1D extends Create[_1D] {
    override def createScalingSpace: ScalingSpace[_1D] = new ScalingSpace1D
  }

  implicit object createScalingSpace2D extends Create[_2D] {
    override def createScalingSpace: ScalingSpace[_2D] = new ScalingSpace2D
  }

  implicit object createScalingSpace3D extends Create[_3D] {
    override def createScalingSpace: ScalingSpace[_3D] = new ScalingSpace3D
  }

  def apply[D <: Dim: DimOps]()(implicit ev: Create[D]) = ev.createScalingSpace

}

class ScalingTransformation[D <: Dim: DimOps] private (s: Float) extends ParametricTransformation[D] with CanInvert[D] with CanDifferentiate[D] {
  def apply(x: Point[D]): Point[D] = (x.toVector * s).toPoint
  val parameters = DenseVector(s)
  def takeDerivative(x: Point[D]): MatrixNxN[D] = MatrixNxN.eye[D] * s

  override def inverse: ScalingTransformation[D] = {
    if (s == 0) new ScalingTransformation[D](0) else new ScalingTransformation[D](1.0f / s)
  }
}

object ScalingTransformation {
  def apply[D <: Dim: DimOps](s: Float) = new ScalingTransformation[D](s)
}

class RigidTransformationSpace[D <: Dim: DimOps: RotationSpace.Create] private (center: Point[D])
  extends ProductTransformationSpace[D, TranslationTransform[D], RotationTransform[D]](TranslationSpace[D], RotationSpace[D](center)) {

  override def transformForParameters(p: ParameterVector): RigidTransformation[D] = {
    val (outerParams, innerParams) = splitProductParameterVector(p)
    new RigidTransformationRotThenTrans[D](TranslationSpace[D].transformForParameters(outerParams), RotationSpace[D](center).transformForParameters(innerParams))
  }
}

object RigidTransformationSpace {

  def apply[D <: Dim: DimOps: RotationSpace.Create](center: Point[D]) = new RigidTransformationSpace[D](center)
  def apply[D <: Dim: DimOps: RotationSpace.Create]() = {
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

  def inverse = new RigidTransformationTransThenRot[D](rotationTransform.inverse, translationTransform.inverse)
}

private class RigidTransformationTransThenRot[D <: Dim: DimOps](rotationTransform: RotationTransform[D], translationTransform: TranslationTransform[D])
  extends ProductTransformation[D](rotationTransform, translationTransform) with RigidTransformation[D] {
  def inverse = new RigidTransformationRotThenTrans[D](translationTransform.inverse, rotationTransform.inverse)

}

case class AnisotropicScalingTransformation[D <: Dim: DimOps](s: Vector[D]) extends ParametricTransformation[D] with CanInvert[D] with CanDifferentiate[D] {
 
  def apply(x: Point[D]): Point[D] = Point((x.toBreezeVector :* s.toBreezeVector).data)

  val parameters = s.toBreezeVector
  def takeDerivative(x: Point[D]): MatrixNxN[D] = MatrixNxN[D](breeze.linalg.diag(s.toBreezeVector).data)

  override def inverse: AnisotropicScalingTransformation[D] = {
    val sinv = s.data.map(v => if (v == 0) 0 else 1.0 / v) map (_.toFloat)
    new AnisotropicScalingTransformation[D](Vector[D](sinv))
  }
}

case class AnisotropicScalingSpace[D <: Dim: DimOps]() extends TransformationSpace[D] with DifferentiableTransforms[D] {
  override type T = AnisotropicScalingTransformation[D]
  def parametersDimensionality: Int = implicitly[DimOps[D]].toInt

  override def identityTransformParameters = DenseVector.ones[Float](parametersDimensionality)

  override def transformForParameters(p: ParameterVector): AnisotropicScalingTransformation[D] = {
    require(p.length == parametersDimensionality)
    AnisotropicScalingTransformation[D](Vector(p.data.take(parametersDimensionality)))
  }

  override def takeDerivativeWRTParameters(p: ParameterVector) = {
    x: Point[D] => new DenseMatrix(parametersDimensionality, 1, x.data)
  }
}

trait AnisotropicSimilarityTransformation[D <: Dim] extends ProductTransformation[D] with CanInvert[D]

private  class RigidTransformationThenAnisotropicScaling[D <: Dim: DimOps](anisotropicScaling: AnisotropicScalingTransformation[D], rigidTransform: RigidTransformationTransThenRot[D])
  extends ProductTransformation[D](anisotropicScaling, rigidTransform) with AnisotropicSimilarityTransformation[D] {

  def inverse: AnisotropicScalingThenRigidTransformation[D] = new AnisotropicScalingThenRigidTransformation[D](rigidTransform.inverse, anisotropicScaling.inverse)
}

private class AnisotropicScalingThenRigidTransformation[D <: Dim: DimOps](rigidTransform: RigidTransformationRotThenTrans[D], anisotropicScaling: AnisotropicScalingTransformation[D])
  extends ProductTransformation[D](rigidTransform, anisotropicScaling) with AnisotropicSimilarityTransformation[D] {

  def inverse: RigidTransformationThenAnisotropicScaling[D] = new RigidTransformationThenAnisotropicScaling[D](anisotropicScaling.inverse, rigidTransform.inverse)
}

case class AnisotropicSimilarityTransformationSpace[D <: Dim: DimOps: RotationSpace.Create](center: Point[D])
  extends ProductTransformationSpace[D, RigidTransformation[D], AnisotropicScalingTransformation[D]](RigidTransformationSpace[D](center), AnisotropicScalingSpace[D]()) {

  override def transformForParameters(p: ParameterVector): AnisotropicSimilarityTransformation[D] = {
    val (rigidP, scalingP) = splitProductParameterVector(p)
    val rigid = RigidTransformationSpace[D](center).transformForParameters(rigidP).asInstanceOf[RigidTransformationRotThenTrans[D]]
    new AnisotropicScalingThenRigidTransformation[D](rigid, AnisotropicScalingSpace[D]().transformForParameters(scalingP))

  }
}

/** scales first then translates **/
case class SimilarityTransformationSpace1D()
  extends ProductTransformationSpace[_1D, TranslationTransform[_1D], ScalingTransformation[_1D]](TranslationSpace[_1D], ScalingSpace[_1D]()) {

  override def transformForParameters(p: ParameterVector): AnisotropicSimilarityTransformation[_1D] = {
    val (transP, scalingP) = splitProductParameterVector(p)
    val trans = TranslationSpace[_1D].transformForParameters(transP)

    new ScalingThenTranslation1D(trans, ScalingSpace[_1D].transformForParameters(scalingP))
  }
}

//trait SimilarityTransform1D extends ProductTransformation[_1D] with CanInvert[_1D]

private class ScalingThenTranslation1D(translation: TranslationTransform[_1D], scaling: ScalingTransformation[_1D])
  extends ProductTransformation[_1D](translation, scaling) with AnisotropicSimilarityTransformation[_1D] {

  def inverse: TranslationThenScaling1D = new TranslationThenScaling1D(scaling.inverse, translation.inverse)
}

private class TranslationThenScaling1D(scaling: ScalingTransformation[_1D], translation: TranslationTransform[_1D])
  extends ProductTransformation[_1D](scaling, translation) with AnisotropicSimilarityTransformation[_1D] {

  def inverse: ScalingThenTranslation1D = new ScalingThenTranslation1D(translation.inverse, scaling.inverse)
}


