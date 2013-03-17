package smptk
package registration

import image.CoordVector
import image.Geometry.{ CoordVector2D, Point2D }
import registration.TransformationSpace.{ ParameterVector }
import breeze.linalg.{ svd, DenseVector, DenseMatrix, mean, variance, Axis }



object LandmarkRegistration {

  def rigid2DLandmarkRegistration(landmarks: IndexedSeq[(Point2D, Point2D)], center: Point2D = CoordVector2D(0., 0.)) 
  : RegistrationResult[CoordVector2D] = {
    val (t, rotMat) = computeRigidNDTransformParams(landmarks, center)
    // assert(center.size == 2)
    assert(t.size == 2)
    assert(rotMat.rows == 2 && rotMat.cols == 2)

    // we can compute the angle from the form of the rotation matrix
    // the acos cannot distinguish between angles in the interval [0,pi] and [-pi, 0]. We double 
    // check with the sin in the rotation matrix and correct the sign accordingly    
    val phiUpToSign = math.acos(rotMat(0, 0))
    val phi = if (math.abs(math.sin(phiUpToSign) - rotMat(1, 0)) > 0.0001) -phiUpToSign   else phiUpToSign

    // val centerCV = CoordVector2D(0f, 0f)
    val optimalParameters = DenseVector.vertcat(t, DenseVector(phi))

    val rigidSpace = RigidTransformationSpace2D(center)
    RegistrationResult(rigidSpace(optimalParameters), optimalParameters)
  }

  private def computeRigidNDTransformParams[CV[A] <: CoordVector[A]](landmarks: IndexedSeq[(CV[Double], CV[Double])], center: CV[Double]): (DenseVector[Double], DenseMatrix[Double]) = {

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
      X(i, ::) := DenseVector(x.toArray) - DenseVector(center.toArray)
      Y(i, ::) := DenseVector(y.toArray) - DenseVector(center.toArray)
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