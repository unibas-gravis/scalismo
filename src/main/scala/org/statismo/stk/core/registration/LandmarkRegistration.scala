package org.statismo.stk.core
package registration

import registration.TransformationSpace.{ ParameterVector }
import breeze.linalg.{ svd, DenseVector, DenseMatrix, Axis }
import breeze.stats.{mean, variance}
import org.statismo.stk.core.geometry._
import breeze.linalg.diag
import breeze.linalg._

object LandmarkRegistration {

  private val origin2D = Point(0f, 0f)
  private val origin3D = Point(0f, 0f, 0f)
  
  private def rigidSimilarity3DCommon(landmarks: IndexedSeq[(Point[_3D], Point[_3D])], similarityFlag: Boolean = false) = {
    val (t, rotMat, s) = computeRigidNDTransformParams(landmarks, similarityFlag)
    // assert(center.size == 2)
    assert(t.size == 3)
    assert(rotMat.rows == 3 && rotMat.cols == 3)

    // have to determine the Euler angles (phi, theta, psi) from the retrieved rotation matrix
    // this follows a pdf document entitled : "Computing Euler angles from a rotation matrix" by Gregory G. Slabaugh (see pseudo-code)

    val rotparams =
      if (Math.abs(Math.abs(rotMat(2, 0)) - 1) > 0.0001) {
        val theta1 = Math.asin(-rotMat(2, 0))

        val psi1 = Math.atan2(rotMat(2, 1) / Math.cos(theta1), rotMat(2, 2) / Math.cos(theta1))

        val phi1 = Math.atan2(rotMat(1, 0) / Math.cos(theta1), rotMat(0, 0) / Math.cos(theta1))

        DenseVector(phi1, theta1, psi1)
      } else {
        /* Gimbal lock, we simply set phi to be 0 */
        val phi = 0.0
        if (Math.abs(rotMat(2, 0) + 1) < 0.0001) { // if R(2,0) == -1
          val theta = Math.PI / 2.0
          val psi = phi + Math.atan2(rotMat(0, 1), rotMat(0, 2))
          DenseVector(phi, theta, psi)
        } else {
          val theta = -Math.PI / 2.0
          val psi = -phi + Math.atan2(-rotMat(0, 1), -rotMat(0, 2))
          DenseVector(phi, theta, psi)
        }
      }

    (t, rotparams, s)
  }

  def rigid3DLandmarkRegistration(landmarks: IndexedSeq[(Point[_3D], Point[_3D])]): RegistrationResult[_3D] = {
    val (t, rotparams, _) = rigidSimilarity3DCommon(landmarks)
    val optimalParameters = DenseVector.vertcat(t, rotparams).map(_.toFloat)
    val rigidSpace = RigidTransformationSpace[_3D]()
    RegistrationResult(rigidSpace.transformForParameters(optimalParameters), optimalParameters)
  }

  def similarity3DLandmarkRegistration(landmarks: IndexedSeq[(Point[_3D], Point[_3D])]): RegistrationResult[_3D] = {
    val (t, rotparams, s) = rigidSimilarity3DCommon(landmarks, true)
    val optimalParameters = DenseVector.vertcat(DenseVector.vertcat(t, rotparams).map(_.toFloat), DenseVector(s.toFloat))
    val similritySpace = RigidTransformationSpace[_3D]().product(ScalingSpace[_3D])
    RegistrationResult(similritySpace.transformForParameters(optimalParameters), optimalParameters)
  }

  private def rigidSimilarity2DCommon(landmarks: IndexedSeq[(Point[_2D], Point[_2D])], similarityFlag: Boolean = false) = {
    val (t, rotMat, s) = computeRigidNDTransformParams(landmarks, similarityFlag)
    assert(t.size == 2)
    assert(rotMat.rows == 2 && rotMat.cols == 2)
    // we can compute the angle from the form of the rotation matrix
    // the acos cannot distinguish between angles in the interval [0,pi] and [-pi, 0]. We double
    // check with the sin in the rotation matrix and correct the sign accordingly
    val phiUpToSign = math.acos(rotMat(0, 0))
    val phi = if (math.abs(math.sin(phiUpToSign) - rotMat(1, 0)) > 0.0001) -phiUpToSign else phiUpToSign

    (t, phi, s)
  }

  def similarity2DLandmarkRegistration(landmarks: IndexedSeq[(Point[_2D], Point[_2D])]): RegistrationResult[_2D] = {
    val (t, phi, s) = rigidSimilarity2DCommon(landmarks, true)
    val optimalParameters = DenseVector.vertcat(DenseVector.vertcat(t, DenseVector(phi)).map(_.toFloat), DenseVector(s.toFloat))
    val similartiySpace = RigidTransformationSpace[_2D](origin2D).product(ScalingSpace[_2D])
    RegistrationResult(similartiySpace.transformForParameters(optimalParameters), optimalParameters)
  }

  def rigid2DLandmarkRegistration(landmarks: IndexedSeq[(Point[_2D], Point[_2D])]): RegistrationResult[_2D] = {
    val (t, phi, s) = rigidSimilarity2DCommon(landmarks)
    val optimalParameters = DenseVector.vertcat(t, DenseVector(phi)).map(_.toFloat)
    val rigidSpace = RigidTransformationSpace[_2D](origin2D)
    RegistrationResult(rigidSpace.transformForParameters(optimalParameters), optimalParameters)
  }

  private def computeRigidNDTransformParams[D <: Dim](landmarks: IndexedSeq[(Point[D], Point[D])], similarityFlag: Boolean = false): (DenseVector[Double], DenseMatrix[Double], Double) = {

    //  see Umeyama: Least squares estimation of transformation parameters between two point patterns

    val n = landmarks.size

    if (n == 0) throw new Exception("Empty set of landmarks provided")
    val dimensionality = landmarks(0)._1.dimensionality

    if (n < dimensionality)
      throw new Exception(s"Not sufficiently many landmarks provided ($n, should be $dimensionality)")

    val X = DenseMatrix.zeros[Double](n, dimensionality)
    val Y = DenseMatrix.zeros[Double](n, dimensionality)

    for (((x, y), i) <- landmarks.zipWithIndex) {
      X(i, ::) := DenseVector(x.data.map(_.toDouble)).t
      Y(i, ::) := DenseVector(y.data.map(_.toDouble)).t
    }

    val mu_x = mean(X.t, Axis._1)
    val sigma2_x = (0 until n).map(i => (X(i, ::).t - mu_x) dot (X(i, ::).t - mu_x)).reduce(_ + _) / n

    val mu_y = mean(Y.t, Axis._1)
    val Sigma_xy = (Y.t - (mu_y * DenseVector.ones[Double](n).t)) * (X.t - mu_x * DenseVector.ones[Double](n).t).t

    val (uMat, dMat, vTMat) = svd(Sigma_xy)

    val S = DenseMatrix.eye[Double](dimensionality)
    if (breeze.linalg.det(Sigma_xy) < 0) {
      println("*** detSigmaxy <0 Flipping last value of S***")
      S(dimensionality - 1, dimensionality - 1) = -1
    }
    val R = uMat * S * vTMat

    val trDS = diag(S) dot dMat

    /**
     * In the computation of the scaling factor, we added a division by the number of points (not indicated
     * in the paper), as the paper version did not seem to give the right result (or we did an error above for which we compensate here  :)
     */
    val c = (1 / (n * sigma2_x)) * trDS

    val t = if (similarityFlag) mu_y - (R * mu_x) * c else mu_y - R * mu_x

    return (t, R, c)
  }

}