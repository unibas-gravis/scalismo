/*
 * Copyright 2015 University of Basel, Graphics and Vision Research Group
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package scalismo.registration

import breeze.linalg.svd.SVD
import breeze.linalg.{ Axis, DenseMatrix, DenseVector, svd, _ }
import breeze.stats.mean
import scalismo.geometry._

object LandmarkRegistration {

  private val origin2D = Point(0f, 0f)
  private val origin3D = Point(0f, 0f, 0f)

  def rotMatrixToEulerAngles(rotMat: DenseMatrix[Double]) = {
    // have to determine the Euler angles (phi, theta, psi) from the retrieved rotation matrix
    // this follows a pdf document entitled : "Computing Euler angles from a rotation matrix" by Gregory G. Slabaugh (see pseudo-code)

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
  }

  private def rigidSimilarity3DCommon(landmarks: IndexedSeq[(Point[_3D], Point[_3D])], center: Point[_3D], similarityFlag: Boolean = false) = {
    val (t, rotMat, s) = computeRigidNDTransformParams(landmarks, center, similarityFlag)
    assert(t.size == 3)
    assert(rotMat.rows == 3 && rotMat.cols == 3)

    val rotparams = rotMatrixToEulerAngles(rotMat)
    (t, rotparams, s)
  }

  /**
   * Returns a rigid transformation mapping the original landmarks into the target. Attention : correspondence between landmarks is
   * inferred based on the Landmark identifier and not their order in the sequence.
   *
   * @param originalLms original set of landmarks to be transformed
   * @param targetLms target landmarks to be mapped to
   */

  def rigid3DLandmarkRegistration(originalLms: Seq[Landmark[_3D]], targetLms: Seq[Landmark[_3D]], center: Point[_3D] = origin3D): RigidTransformation[_3D] = {
    val commonLmNames = targetLms.map(_.id) intersect originalLms.map(_.id)
    val landmarksPairs = commonLmNames.map(name => (originalLms.find(_.id == name).get.point, targetLms.find(_.id == name).get.point))
    LandmarkRegistration.rigid3DLandmarkRegistration(landmarksPairs.toIndexedSeq, center)
  }

  def rigid3DLandmarkRegistration(landmarks: IndexedSeq[(Point[_3D], Point[_3D])], center: Point[_3D]): RigidTransformation[_3D] = {
    val (t, rotparams, _) = rigidSimilarity3DCommon(landmarks, center)
    val optimalParameters = DenseVector.vertcat(t, rotparams)
    val rigidSpace = RigidTransformationSpace[_3D](center)
    rigidSpace.transformForParameters(optimalParameters)
  }

  def similarity3DLandmarkRegistration(landmarks: IndexedSeq[(Point[_3D], Point[_3D])], center: Point[_3D]): ParametricTransformation[_3D] = {
    val (t, rotparams, s) = rigidSimilarity3DCommon(landmarks, center, similarityFlag = true)
    val optimalParameters = DenseVector.vertcat(DenseVector.vertcat(t, rotparams), DenseVector(s))
    val similaritySpace = RigidTransformationSpace[_3D](center).product(ScalingSpace[_3D])
    similaritySpace.transformForParameters(optimalParameters)
  }

  def rotationMatrixToAngle2D(rotMat: DenseMatrix[Double]) = {
    // we can compute the angle from the form of the rotation matrix
    // the acos cannot distinguish between angles in the interval [0,pi] and [-pi, 0]. We double
    // check with the sin in the rotation matrix and correct the sign accordingly
    val phiUpToSign = math.acos(rotMat(0, 0))
    if (math.abs(math.sin(phiUpToSign) - rotMat(1, 0)) > 0.0001) -phiUpToSign else phiUpToSign
  }

  private def rigidSimilarity2DCommon(landmarks: IndexedSeq[(Point[_2D], Point[_2D])], center: Point[_2D] = origin2D, similarityFlag: Boolean = false) = {
    val (t, rotMat, s) = computeRigidNDTransformParams(landmarks, center, similarityFlag)
    assert(t.size == 2)
    assert(rotMat.rows == 2 && rotMat.cols == 2)

    val phi = rotationMatrixToAngle2D(rotMat)
    (t, phi, s)
  }

  def similarity2DLandmarkRegistration(landmarks: IndexedSeq[(Point[_2D], Point[_2D])], center: Point[_2D] = origin2D): ParametricTransformation[_2D] = {
    val (t, phi, s) = rigidSimilarity2DCommon(landmarks, similarityFlag = true)
    val optimalParameters = DenseVector.vertcat(DenseVector.vertcat(t, DenseVector(phi)), DenseVector(s))
    val similaritySpace = RigidTransformationSpace[_2D](center).product(ScalingSpace[_2D])
    similaritySpace.transformForParameters(optimalParameters)
  }

  def rigid2DLandmarkRegistration(landmarks: IndexedSeq[(Point[_2D], Point[_2D])], center: Point[_2D] = origin2D): RigidTransformation[_2D] = {
    val (t, phi, _) = rigidSimilarity2DCommon(landmarks)
    val optimalParameters = DenseVector.vertcat(t, DenseVector(phi))
    val rigidSpace = RigidTransformationSpace[_2D](center)
    rigidSpace.transformForParameters(optimalParameters)
  }

  private def computeRigidNDTransformParams[D <: Dim](landmarks: IndexedSeq[(Point[D], Point[D])], center: Point[D], similarityFlag: Boolean = false): (DenseVector[Double], DenseMatrix[Double], Double) = {

    //  see Umeyama: Least squares estimation of transformation parameters between two point patterns

    val n = landmarks.size

    if (n == 0) throw new Exception("Empty set of landmarks provided")
    val dimensionality = landmarks(0)._1.dimensionality

    if (n < dimensionality)
      throw new Exception(s"Not sufficiently many landmarks provided ($n, should be $dimensionality)")

    val X = DenseMatrix.zeros[Double](n, dimensionality)
    val Y = DenseMatrix.zeros[Double](n, dimensionality)

    for (((x, y), i) <- landmarks.zipWithIndex) {
      X(i, ::) := (DenseVector(x.toArray) - center.toBreezeVector).t
      Y(i, ::) := (DenseVector(y.toArray) - center.toBreezeVector).t
    }

    val mu_x = mean(X.t, Axis._1)
    val sigma2_x = (0 until n).map(i => (X(i, ::).t - mu_x) dot (X(i, ::).t - mu_x)).reduce(_ + _) / n

    val mu_y = mean(Y.t, Axis._1)
    val Sigma_xy = (Y.t - (mu_y * DenseVector.ones[Double](n).t)) * (X.t - mu_x * DenseVector.ones[Double](n).t).t

    val SVD(uMat, dMat, vTMat) = svd(Sigma_xy)

    val S = DenseMatrix.eye[Double](dimensionality)
    if (breeze.linalg.det(Sigma_xy) < 0) {
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

    (t, R, c)
  }
}
