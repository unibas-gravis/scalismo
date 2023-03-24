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
import breeze.linalg.{svd, Axis, DenseMatrix, DenseVector, _}
import breeze.stats.mean
import scalismo.geometry._
import scalismo.transformations._

object LandmarkRegistration {

  private val origin2D = Point(0f, 0f)
  private val origin3D = Point(0f, 0f, 0f)

  private def rigidSimilarity3DCommon(landmarks: Seq[(Point[_3D], Point[_3D])],
                                      center: Point[_3D],
                                      similarityFlag: Boolean = false
  ) = {
    val (t, rotMat, s) = computeRigidNDTransformParams(landmarks, center, similarityFlag)
    assert(t.size == 3)
    assert(rotMat.rows == 3 && rotMat.cols == 3)

    val rotparams = RotationSpace3D.rotMatrixToEulerAngles(SquareMatrix(rotMat.toArray))
    (t, rotparams, s)
  }

  /**
   * Returns a rigid transformations mapping the original landmarks into the target. Attention : correspondence between
   * landmarks is inferred based on the Landmark identifier and not their order in the sequence.
   *
   * @param originalLms
   *   original set of landmarks to be transformed
   * @param targetLms
   *   target landmarks to be mapped to
   */
  def rigid3DLandmarkRegistration(originalLms: Seq[Landmark[_3D]],
                                  targetLms: Seq[Landmark[_3D]],
                                  center: Point[_3D]
  ): TranslationAfterRotation[_3D] = {
    val commonLmNames = targetLms.map(_.id) intersect originalLms.map(_.id)
    val landmarksPairs =
      commonLmNames.map(name => (originalLms.find(_.id == name).get.point, targetLms.find(_.id == name).get.point))
    LandmarkRegistration.rigid3DLandmarkRegistration(landmarksPairs.toIndexedSeq, center)
  }

  /**
   * Returns a rigid transformation mapping the original landmarks (first elements of the tuples) into the corresponding
   * target points (second elements of the tuples).
   *
   * @param landmarks
   *   sequence of corresponding landmarks
   * @param center
   *   center of rotation to be used for the rigid transformations
   */
  def rigid3DLandmarkRegistration(landmarks: Seq[(Point[_3D], Point[_3D])],
                                  center: Point[_3D]
  ): TranslationAfterRotation[_3D] = {
    val (t, (phi, theta, psi), _) = rigidSimilarity3DCommon(landmarks, center)
    TranslationAfterRotation(
      Translation3D(EuclideanVector3D(t(0), t(1), t(2))),
      Rotation3D(phi, theta, psi, center)
    )
  }

  /**
   * Returns a rigid transformations mapping the original landmarks (first elements of the tuples) into the
   * corresponding target points (second elements of the tuples).
   *
   * @param landmarks
   *   sequence of corresponding landmarks
   * @param center
   *   center of rotation to be used for the rigid transformations
   */
  def rigid2DLandmarkRegistration(landmarks: Seq[(Point[_2D], Point[_2D])],
                                  center: Point[_2D]
  ): TranslationAfterRotation[_2D] = {
    val (t, rotparams, _) = rigidSimilarity2DCommon(landmarks, center)
    TranslationAfterRotation2D(Translation2D(EuclideanVector2D(t(0), t(1))), Rotation2D(rotparams, center))
  }

  /**
   * Returns a rigid transformations mapping the original landmarks (first elements of the tuples) into the
   * corresponding target points (second elements of the tuples).
   *
   * @param landmarks
   *   sequence of corresponding landmarks
   * @param center
   *   \- center of the rotation
   */
  def rigid2DLandmarkRegistration(originalLms: Seq[Landmark[_2D]],
                                  targetLms: Seq[Landmark[_2D]],
                                  center: Point[_2D]
  ): TranslationAfterRotation[_2D] = {
    val commonLmNames = targetLms.map(_.id) intersect originalLms.map(_.id)
    val landmarksPairs =
      commonLmNames.map(name => (originalLms.find(_.id == name).get.point, targetLms.find(_.id == name).get.point))
    LandmarkRegistration.rigid2DLandmarkRegistration(landmarksPairs.toIndexedSeq, center)
  }

  /**
   * Returns a similarity transformation mapping the original landmarks (first elements of the tuples) into the
   * corresponding target points (second elements of the tuples).
   *
   * @param landmarks
   *   sequence of corresponding landmarks
   * @param center
   *   \- center of the rotation
   */
  def similarity3DLandmarkRegistration(landmarks: Seq[(Point[_3D], Point[_3D])],
                                       center: Point[_3D]
  ): TranslationAfterScalingAfterRotation[_3D] = {

    val (t, (phi, theta, psi), s) = rigidSimilarity3DCommon(landmarks, center, similarityFlag = true)

    TranslationAfterScalingAfterRotation3D(
      Translation3D(EuclideanVector3D(t(0), t(1), t(2))),
      Scaling3D(s),
      Rotation3D(phi, theta, psi, center)
    )
  }

  /**
   * Returns a similarity transformation mapping the original landmarks (first elements of the tuples) into the
   * corresponding target points (second elements of the tuples).
   *
   * @param landmarks
   *   sequence of corresponding landmarks
   * @param center
   *   \- center of the rotation
   */
  def similarity3DLandmarkRegistration(originalLms: Seq[Landmark[_3D]],
                                       targetLms: Seq[Landmark[_3D]],
                                       center: Point[_3D]
  ): TranslationAfterScalingAfterRotation[_3D] = {
    val commonLmNames = targetLms.map(_.id) intersect originalLms.map(_.id)
    val landmarksPairs =
      commonLmNames.map(name => (originalLms.find(_.id == name).get.point, targetLms.find(_.id == name).get.point))
    LandmarkRegistration.similarity3DLandmarkRegistration(landmarksPairs.toIndexedSeq, center)
  }

  /**
   * Returns a similarity transformation mapping the original landmarks (first elements of the tuples) into the
   * corresponding target points (second elements of the tuples).
   *
   * @param landmarks
   *   sequence of corresponding landmarks
   * @param center
   *   \- center of the rotation
   */
  def similarity2DLandmarkRegistration(originalLms: Seq[Landmark[_2D]],
                                       targetLms: Seq[Landmark[_2D]],
                                       center: Point[_2D]
  ): TranslationAfterScalingAfterRotation[_2D] = {
    val commonLmNames = targetLms.map(_.id) intersect originalLms.map(_.id)
    val landmarksPairs =
      commonLmNames.map(name => (originalLms.find(_.id == name).get.point, targetLms.find(_.id == name).get.point))
    LandmarkRegistration.similarity2DLandmarkRegistration(landmarksPairs.toIndexedSeq, center)
  }

  def similarity2DLandmarkRegistration(landmarks: Seq[(Point[_2D], Point[_2D])],
                                       center: Point[_2D]
  ): TranslationAfterScalingAfterRotation[_2D] = {
    val (t, phi, s) = rigidSimilarity2DCommon(landmarks, similarityFlag = true, center = center)

    TranslationAfterScalingAfterRotation(
      Translation(EuclideanVector2D(t(0), t(1))),
      Scaling2D(s),
      Rotation(phi, center)
    )
  }

  def rotationMatrixToAngle2D(rotMat: DenseMatrix[Double]) = {
    // we can compute the angle from the form of the rotation matrix
    // the acos cannot distinguish between angles in the interval [0,pi] and [-pi, 0]. We double
    // check with the sin in the rotation matrix and correct the sign accordingly
    val phiUpToSign = math.acos(rotMat(0, 0))
    if (math.abs(math.sin(phiUpToSign) - rotMat(1, 0)) > 0.0001) -phiUpToSign else phiUpToSign
  }

  private def rigidSimilarity2DCommon(landmarks: Seq[(Point[_2D], Point[_2D])],
                                      center: Point[_2D],
                                      similarityFlag: Boolean = false
  ) = {
    val (t, rotMat, s) = computeRigidNDTransformParams(landmarks, center, similarityFlag)
    assert(t.size == 2)
    assert(rotMat.rows == 2 && rotMat.cols == 2)

    val phi = rotationMatrixToAngle2D(rotMat)
    (t, phi, s)
  }

  private def computeRigidNDTransformParams[D](
    landmarks: Seq[(Point[D], Point[D])],
    center: Point[D],
    similarityFlag: Boolean
  ): (DenseVector[Double], DenseMatrix[Double], Double) = {

    //  see Umeyama: Least squares estimation of transformations parameters between two point patterns

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
    val sigma2_x = (0 until n)
      .map(i => {
        val xit: DenseVector[Double] = (X(i, ::).t - mu_x)
        xit dot xit
      })
      .reduce(_ + _) / n

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
     * In the computation of the scaling factor, we added a division by the number of points (not indicated in the
     * paper), as the paper version did not seem to give the right result (or we did an error above for which we
     * compensate here :)
     */
    val c = (1 / (n * sigma2_x)) * trDS

    val t = if (similarityFlag) mu_y - (R * mu_x) * c else mu_y - R * mu_x

    (t, R, c)
  }
}
