package scalismo.transformations

import breeze.linalg.{DenseMatrix, DenseVector}
import scalismo.common.{EuclideanSpace, EuclideanSpace1D, EuclideanSpace2D, EuclideanSpace3D, Field, RealSpace}
import scalismo.geometry.{_1D, _2D, _3D, EuclideanVector, NDSpace, Point, SquareMatrix}
import scalismo.registration.LandmarkRegistration
import scalismo.transformations.ParametricTransformation.JacobianField
import scalismo.transformations.TransformationSpace.ParameterVector

import scala.annotation.implicitNotFound

/**
 * D-dimensional Rotation transform that is parametric, invertible and differentiable.
 */
abstract class Rotation[D: NDSpace] extends RigidTransformation[D] {

  override def inverse: Rotation[D]

  def rotationMatrix: SquareMatrix[D]

  def center: Point[D]
}

/** Factory for [[Rotation]] instances. */
object Rotation {

  def apply(phi: Double, center: Point[_2D]): Rotation[_2D] = {

    new Rotation2D(phi, center)
  }

  /**
   *  Factory method to create a 3-dimensional rotation transform around a center when
   *  given the Euler angles according to the x-convention
   *
   *  @param phi rotation around the Z axis
   *  @param theta rotation around the Y axis
   *  @param psi rotation around the X axis
   *
   */
  def apply(phi: Double, theta: Double, psi: Double, center: Point[_3D]): Rotation[_3D] = {
    Rotation3D(phi, theta, psi, center)
  }
}

case class Rotation2D(phi: Double, val center: Point[_2D]) extends Rotation[_2D] {

  val rotationMatrix = SquareMatrix(
    (math.cos(phi), -math.sin(phi)),
    (math.sin(phi), math.cos(phi))
  )

  override val f = (pt: Point[_2D]) => {
    val ptCentered = pt - center
    val rotCentered = rotationMatrix * ptCentered
    center + EuclideanVector(rotCentered(0), rotCentered(1))
  }
  override def domain = EuclideanSpace2D

  val parameters = DenseVector(phi)

  override def derivativeWRTPosition: Point[_2D] => SquareMatrix[_2D] = { x =>
    rotationMatrix
  }

  override def inverse: Rotation2D = {
    new Rotation2D(-phi, center)
  }

  override def numberOfParameters: Int = 1

  override def derivativeWRTParameters: JacobianField[_2D] = {
    val df = (x: Point[_2D]) => {
      val sa = math.sin(parameters(0))
      val ca = math.cos(parameters(0))
      val cx = center(0)
      val cy = center(1)

      DenseMatrix(-sa * (x(0) - cx) - ca * (x(1) - cy), ca * (x(0) - cx) - sa * (x(1) - cy))
    }
    Field(domain, df)
  }
}

case class Rotation3D(val rotationMatrix: SquareMatrix[_3D], val center: Point[_3D]) extends Rotation[_3D] {

  lazy val (phi, theta, psi): (Double, Double, Double) = RotationSpace3D.rotMatrixToEulerAngles(rotationMatrix)

  override val f = (pt: Point[_3D]) => {
    val ptCentered = pt - center
    val rotCentered = rotationMatrix * ptCentered
    center + EuclideanVector(rotCentered(0), rotCentered(1), rotCentered(2))
  }

  override val domain = RealSpace[_3D]

  override def derivativeWRTPosition: Point[_3D] => SquareMatrix[_3D] = { x =>
    rotationMatrix
  }

  override def inverse: Rotation3D = {
    Rotation3D(SquareMatrix.inv(rotationMatrix), center)
  }

  override def numberOfParameters: Int = 3

  override def derivativeWRTParameters: JacobianField[_3D] = {
    val df = (x: Point[_3D]) => {
      val cospsi = Math.cos(psi)
      val sinpsi = Math.sin(psi)
      val costh = Math.cos(theta)
      val sinth = Math.sin(theta)
      val cosphi = Math.cos(phi)
      val sinphi = Math.sin(phi)

      val x0minc0 = x(0) - center(0)
      val x1minc1 = x(1) - center(1)
      val x2minc2 = x(2) - center(2)

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

      DenseMatrix((dr00, dr01, dr02), (dr10, dr11, dr12), (dr20, dr21, dr22))
    }
    Field(domain, df)
  }

  override def parameters: DenseVector[Double] = DenseVector(phi, theta, psi)
}

object Rotation3D {
  def apply(phi: Double, theta: Double, psi: Double, center: Point[_3D]): Rotation[_3D] = {
    Rotation3D(RotationSpace3D.eulerAnglesToRotMatrix(phi, theta, psi), center)
  }
}

/**
 * Parametric transformation space producing rotation transforms around a rotation centre.
 *
 */
abstract class RotationSpace[D: NDSpace] extends TransformationSpaceWithDifferentiableTransforms[D] {

  /** Center of rotation. All rotations generated by this parametric space will be around this indicated Point*/
  def center: Point[D]

  override type T[D] = Rotation[D]

  /**
   * Returns a rotation transform corresponding to the given parameters.
   *
   *  Rotation parameters for _2D : a scalar indicating the rotation angle in radians
   *  Rotation parameters for _3D : Euler angles in x-convention (rotation around Z axis first, around Y axis second, around X axis third)
   */
  override def transformationForParameters(p: ParameterVector): Rotation[D]

  override def identityTransformation = {
    transformationForParameters(DenseVector.zeros[Double](numberOfParameters))
  }
}

case class RotationSpace2D(val center: Point[_2D]) extends RotationSpace[_2D] {
  override val domain = EuclideanSpace2D
  override def numberOfParameters: Int = 1

  override def transformationForParameters(p: ParameterVector): Rotation[_2D] = {
    require(p.length == numberOfParameters)

    Rotation2D(p(0), center)
  }

}

case class RotationSpace3D(val center: Point[_3D]) extends RotationSpace[_3D] {

  override val domain = EuclideanSpace3D

  override def numberOfParameters: Int = 3

  override def transformationForParameters(p: ParameterVector): Rotation[_3D] = {
    require(p.length == numberOfParameters)

    Rotation3D(p(0), p(1), p(2), center)
  }

}

object RotationSpace3D {
  def rotMatrixToEulerAngles(rotMat: SquareMatrix[_3D]): (Double, Double, Double) = {
    // have to determine the Euler angles (phi, theta, psi) from the retrieved rotation matrix
    // this follows a pdf document entitled : "Computing Euler angles from a rotation matrix" by Gregory G. Slabaugh (see pseudo-code)

    if (Math.abs(Math.abs(rotMat(2, 0)) - 1) > 0.0001) {
      val theta1 = Math.asin(-rotMat(2, 0))

      val psi1 = Math.atan2(rotMat(2, 1) / Math.cos(theta1), rotMat(2, 2) / Math.cos(theta1))

      val phi1 = Math.atan2(rotMat(1, 0) / Math.cos(theta1), rotMat(0, 0) / Math.cos(theta1))

      (phi1, theta1, psi1)
    } else {
      /* Gimbal lock, we simply set phi to be 0 */
      val phi = 0.0
      if (Math.abs(rotMat(2, 0) + 1) < 0.0001) { // if R(2,0) == -1
        val theta = Math.PI / 2.0
        val psi = phi + Math.atan2(rotMat(0, 1), rotMat(0, 2))
        (phi, theta, psi)
      } else {

        val theta = -Math.PI / 2.0
        val psi = -phi + Math.atan2(-rotMat(0, 1), -rotMat(0, 2))
        (phi, theta, psi)
      }
    }
  }

  private[scalismo] def eulerAnglesToRotMatrix(phi: Double, theta: Double, psi: Double): SquareMatrix[_3D] = {
    val rotMatrix = {
      // rotation matrix according to the "x-convention"
      val cospsi = Math.cos(psi)
      val sinpsi = Math.sin(psi)

      val costh = Math.cos(theta)
      val sinth = Math.sin(theta)

      val cosphi = Math.cos(phi)
      val sinphi = Math.sin(phi)

      SquareMatrix(
        (costh * cosphi, sinpsi * sinth * cosphi - cospsi * sinphi, sinpsi * sinphi + cospsi * sinth * cosphi),
        (costh * sinphi, cospsi * cosphi + sinpsi * sinth * sinphi, cospsi * sinth * sinphi - sinpsi * cosphi),
        (-sinth, sinpsi * costh, cospsi * costh)
      )
    }
    rotMatrix
  }
}

/** Factory for [[RotationSpace]] instances. */
object RotationSpace {

  /**
   * Factory method to create a D dimensional parametric transformation space generating rotations around the indicated centre
   *  Only _2D and _3D dimensions are supported
   */
  def apply(center: Point[_3D]) = {
    RotationSpace3D(center)
  }

  def apply(center: Point[_2D]) = {
    RotationSpace2D(center)
  }

}
