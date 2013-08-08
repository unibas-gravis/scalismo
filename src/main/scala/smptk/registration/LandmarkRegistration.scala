package smptk
package registration


import registration.TransformationSpace.{ ParameterVector }
import breeze.linalg.{ svd, DenseVector, DenseMatrix, mean, variance, Axis }
import smptk.geometry._

object LandmarkRegistration {

  def rigid3DLandmarkRegistration(landmarks: IndexedSeq[(Point[ThreeD], Point[ThreeD])], center: Point[ThreeD] = Point3D(0.0, 0.0, 0.0)): RegistrationResult[ThreeD] = {
    val (t, rotMat) = computeRigidNDTransformParams(landmarks, center)
    // assert(center.size == 2)
    assert(t.size == 3)
    assert(rotMat.rows == 3 && rotMat.cols == 3)
 
    // have to determine the Euler angles (phi, theta, psi) from the retrieved rotation matrix 
    // this follows a pdf document entitled : "Computing Euler angles from a rotation matrix" by Gregory G. Slabaugh (see pseudo-code)

    val rotparams =
      if ( Math.abs(Math.abs(rotMat(2, 0))-1) > 0.0001 ) { 
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

    val optimalParameters = DenseVector.vertcat(t, rotparams)
    val rigidSpace = RigidTransformationSpace3D(center)
    RegistrationResult(rigidSpace(optimalParameters), optimalParameters)
  }

  def rigid2DLandmarkRegistration(landmarks: IndexedSeq[(Point[TwoD], Point[TwoD])], center: Point[TwoD] = Point2D(0.0, 0.0)): RegistrationResult[TwoD] = {
    val (t, rotMat) = computeRigidNDTransformParams(landmarks, center)
    // assert(center.size == 2)
    assert(t.size == 2)
    assert(rotMat.rows == 2 && rotMat.cols == 2)

    // we can compute the angle from the form of the rotation matrix
    // the acos cannot distinguish between angles in the interval [0,pi] and [-pi, 0]. We double 
    // check with the sin in the rotation matrix and correct the sign accordingly    
    val phiUpToSign = math.acos(rotMat(0, 0))
    val phi = if (math.abs(math.sin(phiUpToSign) - rotMat(1, 0)) > 0.0001) -phiUpToSign else phiUpToSign

    // val centerCV = CoordVector2D(0f, 0f)
    val optimalParameters = DenseVector.vertcat(t, DenseVector(phi))

    val rigidSpace = RigidTransformationSpace2D(center)
    RegistrationResult(rigidSpace(optimalParameters), optimalParameters)
  }

  private def computeRigidNDTransformParams[D <: Dim](landmarks: IndexedSeq[(Point[D], Point[D])], center: Point[D]): (DenseVector[Double], DenseMatrix[Double]) = {

    //  see Umeyama: Least squares estimation of transformation parameters between two point patterns

    val n = landmarks.size

    if (n == 0) throw new Exception("Empty set of landmarks provided")
    val dimensionality = landmarks(0)._1.dimensionality

    if (n < dimensionality)
      throw new Exception(s"Not sufficiently many landmarks provided ($n, should be $dimensionality)")

    val X = DenseMatrix.zeros[Double](n, dimensionality)
    val Y = DenseMatrix.zeros[Double](n, dimensionality)

    for (((x, y), i) <- landmarks.zipWithIndex) {
      // create a matrix with the point coordinates. The roation in the method by Umeyama is computed
      // with respect to the center of rotation 0 (origin). To allow for a non-zero center, we translate
      // both point clouds by the center before applying his method.
      X(i, ::) := DenseVector(x.data) - DenseVector(center.data)
      Y(i, ::) := DenseVector(y.data) - DenseVector(center.data)
    }

    val mu_x = mean(X.t, Axis._1)
    val sigma2_x = variance(X.t, Axis._1)
    val mu_y = mean(Y.t, Axis._1)
    val sigma2_y = variance(Y.t, Axis._1)
    val Sigma_xy = (Y.t - (mu_y * DenseVector.ones[Double](n).t)) * (X.t - mu_x * DenseVector.ones[Double](n).t).t

    val (uMat, dMat, vTMat) = svd(Sigma_xy)

    val S = DenseMatrix.eye[Double](dimensionality)
    if (breeze.linalg.det(Sigma_xy) < 0) S(-1, -1) = -1
    val R = uMat * S * vTMat
    val t = mu_y - R * mu_x

    return (t, R)
  }

}