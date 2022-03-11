/*
 * Copyright 2016 University of Basel, Graphics and Vision Research Group
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

package scalismo.sampling.parameters

import breeze.linalg.DenseVector
import scalismo.geometry.{_3D, EuclideanVector, EuclideanVector3D, Point}
import scalismo.registration.GaussianProcessTransformation3D
import scalismo.sampling.ParameterConversion
import scalismo.statisticalmodel.LowRankGaussianProcess
import scalismo.transformations.{
  RigidTransformation,
  Rotation3D,
  Scaling3D,
  SimilarityTransformation,
  Transformation,
  Translation3D,
  TranslationAfterRotation3D,
  TranslationAfterScalingAfterRotation3D
}

/**
 * Represents shape parameters. Intended to be used in a Metropolis Hastings sampling chain
 */
case class ShapeParameters(coefficients: DenseVector[Double])

object ShapeParameters {
  implicit object shapeParameterConversion extends ParameterConversion[DenseVector[Double], ShapeParameters] {
    override def to(a: DenseVector[Double]): ShapeParameters = ShapeParameters(a)

    override def from(b: ShapeParameters): DenseVector[Double] = b.coefficients
  }
}

/**
 * Represents translation parameters, represented by a translation vector.
 */
case class TranslationParameters(translationVector: EuclideanVector[_3D])

object TranslationParameters {
  implicit object translationParameterConversion
      extends ParameterConversion[(Double, Double, Double), TranslationParameters] {
    override def from(t: TranslationParameters): (Double, Double, Double) =
      (t.translationVector.x, t.translationVector.y, t.translationVector.z)

    override def to(t: (Double, Double, Double)): TranslationParameters =
      TranslationParameters(EuclideanVector3D(t._1, t._2, t._3))
  }
}

/**
 * Represents rotation parameters, represented by the three euler angles.
 *
 * Note that to complete a rotation, a rotation center needs to be known. As this is usually not
 * changed in a sampling run, we do not consider it as a parameter
 *
 * @param angles - 3 Euler angles
 */
case class RotationParameters(angles: (Double, Double, Double))

object RotationParameters {
  implicit object rotationParametersConversion
      extends ParameterConversion[(Double, Double, Double), RotationParameters] {
    override def from(t: RotationParameters): (Double, Double, Double) = (t.angles._1, t.angles._2, t.angles._3)

    override def to(t: (Double, Double, Double)): RotationParameters = RotationParameters(angles = t)
  }

}

/**
 * Represents isotropic scaling by a given positive scalefactor
 */
case class ScaleParameter(scale: Double)

/**
 * Combination of translation, rotation and shape parameters
 */
case class PoseAndShapeParameters(translationParameters: TranslationParameters,
                                  rotationParameters: RotationParameters,
                                  shapeParameters: ShapeParameters)

object PoseAndShapeParameters {
  implicit object PoseAndShapeParametersConversion
      extends ParameterConversion[(TranslationParameters, RotationParameters, ShapeParameters), PoseAndShapeParameters] {
    override def from(t: PoseAndShapeParameters): (TranslationParameters, RotationParameters, ShapeParameters) =
      (t.translationParameters, t.rotationParameters, t.shapeParameters)

    override def to(t: (TranslationParameters, RotationParameters, ShapeParameters)): PoseAndShapeParameters =
      PoseAndShapeParameters(t._1, t._2, t._3)
  }

}

/**
 * Standard transformations that describe the given parameters. These are useful to implement evaluators for model fitting
 */
object StandardFittingParameters {
  def poseTransformationForParameters(translationParameters: TranslationParameters,
                                      rotationParameters: RotationParameters,
                                      centerOfRotation: Point[_3D]): RigidTransformation[_3D] = {
    TranslationAfterRotation3D(Translation3D(translationParameters.translationVector),
                               Rotation3D(rotationParameters.angles, centerOfRotation))
  }

  def poseAndSizeTransformationForParameters(translationParameters: TranslationParameters,
                                             scaleParameter: ScaleParameter,
                                             rotationParameters: RotationParameters,
                                             centerOfRotation: Point[_3D]): SimilarityTransformation[_3D] = {
    TranslationAfterScalingAfterRotation3D(Translation3D(translationParameters.translationVector),
                                           Scaling3D(scaleParameter.scale),
                                           Rotation3D(rotationParameters.angles, centerOfRotation))
  }

  def poseAndShapeTransformationForParameters(
    translationParameters: TranslationParameters,
    rotationParameters: RotationParameters,
    centerOfRotation: Point[_3D],
    shapeParameters: ShapeParameters,
    gp: LowRankGaussianProcess[_3D, EuclideanVector[_3D]]
  ): Transformation[_3D] = {
    val poseTransformation =
      poseTransformationForParameters(translationParameters, rotationParameters, centerOfRotation)
    val gpTransformation = GaussianProcessTransformation3D(gp, shapeParameters.coefficients)
    poseTransformation.compose(gpTransformation)
  }

  def sizePoseAndShapeTransformationForParameters(
    translationParameters: TranslationParameters,
    scaleParameter: ScaleParameter,
    rotationParameters: RotationParameters,
    centerOfRotation: Point[_3D],
    shapeParameters: ShapeParameters,
    gp: LowRankGaussianProcess[_3D, EuclideanVector[_3D]]
  ): Transformation[_3D] = {
    val poseAndSizeTransformation = poseAndSizeTransformationForParameters(translationParameters,
                                                                           scaleParameter,
                                                                           rotationParameters,
                                                                           centerOfRotation)
    val gpTransformation = GaussianProcessTransformation3D(gp, shapeParameters.coefficients)
    poseAndSizeTransformation.compose(gpTransformation)
  }

}
