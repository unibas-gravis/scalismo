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
package scalismo.statisticalmodel.asm

import breeze.linalg.{convert, DenseVector}
import scalismo.common.{PointId, UnstructuredPointsDomain, UnstructuredPointsDomain3D}
import scalismo.geometry.{_3D, Point}
import scalismo.image.DiscreteScalarImage
import scalismo.mesh.TriangleMesh
import scalismo.numerics.Sampler
import scalismo.registration.{LandmarkRegistration, RigidTransformation, RigidTransformationSpace, Transformation}
import scalismo.statisticalmodel.{MultivariateNormalDistribution, StatisticalMeshModel}
import scalismo.utils.Random

import scala.collection.immutable
import scala.util.{Failure, Try}

object ActiveShapeModel {
  type TrainingData = Iterator[(DiscreteScalarImage[_3D, Float], Transformation[_3D])]

  /**
   * Train an active shape model using an existing PCA model
   */
  def trainModel(statisticalModel: StatisticalMeshModel,
                 trainingData: TrainingData,
                 preprocessor: ImagePreprocessor,
                 featureExtractor: FeatureExtractor,
                 sampler: TriangleMesh[_3D] => Sampler[_3D]): ActiveShapeModel = {

    val sampled = sampler(statisticalModel.referenceMesh).sample.map(_._1).to[immutable.IndexedSeq]
    val pointIds = sampled.map(statisticalModel.referenceMesh.pointSet.findClosestPoint(_).id)

    // preprocessed images can be expensive in terms of memory, so we go through them one at a time.
    val imageFeatures = trainingData.flatMap {
      case (image, transform) =>
        val (pimg, mesh) = (preprocessor(image), statisticalModel.referenceMesh.transform(transform))
        pointIds.map { pointId =>
          featureExtractor(pimg, mesh.pointSet.point(pointId), mesh, pointId)
        }
    }.toIndexedSeq

    // the structure is "wrongly nested" now, like: {img1:{pt1,pt2}, img2:{pt1,pt2}} (flattened).
    // We merge the corresponding points together, then estimate an MVD.
    val pointsLength = pointIds.length
    val imageRange = (0 until imageFeatures.length / pointsLength).toIndexedSeq
    val pointFeatures = (0 until pointsLength).toIndexedSeq.map { pointIndex =>
      val featuresForPoint = imageRange.flatMap { imageIndex =>
        imageFeatures(imageIndex * pointsLength + pointIndex).map(convert(_, Double))
      }
      MultivariateNormalDistribution.estimateFromData(featuresForPoint)
    }

    val profiles = new Profiles(pointIds.zip(pointFeatures).map { case (i, d) => Profile(i, d) })
    ActiveShapeModel(statisticalModel, profiles, preprocessor, featureExtractor)
  }
}

/**
 * Class of instances sampled from an Active Shape Model. A sample is therefore comprised of both a shape sampled from
 * the Shape Model, and a set of sample features at the profile points.
 *
 */
case class ASMSample(mesh: TriangleMesh[_3D],
                     featureField: DiscreteFeatureField[_3D, UnstructuredPointsDomain[_3D]],
                     featureExtractor: FeatureExtractor)

case class ActiveShapeModel(statisticalModel: StatisticalMeshModel,
                            profiles: Profiles,
                            preprocessor: ImagePreprocessor,
                            featureExtractor: FeatureExtractor) {

  /**
   * Returns the mean mesh of the shape model, along with the mean feature profiles at the profile points
   */
  def mean(): ASMSample = {
    val smean = statisticalModel.mean
    val meanProfilePoints = profiles.data.map(p => smean.pointSet.point(p.pointId))
    val meanFeatures = profiles.data.map(_.distribution.mean)
    val featureField = DiscreteFeatureField[_3D, UnstructuredPointsDomain[_3D]](
      new UnstructuredPointsDomain3D(meanProfilePoints),
      meanFeatures
    )
    ASMSample(smean, featureField, featureExtractor)
  }

  /**
   * Returns a random sample mesh from the shape model, along with randomly sampled feature profiles at the profile points
   */
  def sample()(implicit rand: Random): ASMSample = {
    val sampleMesh = statisticalModel.sample()
    val randomProfilePoints = profiles.data.map(p => sampleMesh.pointSet.point(p.pointId))
    val randomFeatures = profiles.data.map(_.distribution.sample)
    val featureField = DiscreteFeatureField[_3D, UnstructuredPointsDomain[_3D]](
      new UnstructuredPointsDomain3D(randomProfilePoints),
      randomFeatures
    )
    ASMSample(sampleMesh, featureField, featureExtractor)
  }

  /**
   * Utility function that allows to randomly sample different feature profiles, while keeping the profile points
   * Meant to allow to easily inspect/debug the feature distribution
   */
  def sampleFeaturesOnly()(implicit rand: Random): ASMSample = {
    val smean = statisticalModel.mean
    val meanProfilePoints = profiles.data.map(p => smean.pointSet.point(p.pointId))
    val randomFeatures = profiles.data.map(_.distribution.sample)
    val featureField = DiscreteFeatureField[_3D, UnstructuredPointsDomain[_3D]](
      new UnstructuredPointsDomain3D(meanProfilePoints),
      randomFeatures
    )
    ASMSample(smean, featureField, featureExtractor)
  }

  /**
   * Returns an Active Shape Model where both the statistical shape Model and the profile points distributions are correctly transformed
   * according to the provided rigid transformation
   *
   */
  def transform(rigidTransformation: RigidTransformation[_3D]): ActiveShapeModel = {
    val transformedModel = statisticalModel.transform(rigidTransformation)
    this.copy(statisticalModel = transformedModel)
  }

  private def noTransformations =
    ModelTransformations(
      statisticalModel.coefficients(statisticalModel.mean),
      RigidTransformationSpace[_3D]()
        .transformForParameters(RigidTransformationSpace[_3D]().identityTransformParameters)
    )

  /**
   * Perform an ASM fitting for the given target image.
   * This is logically equivalent to calling <code>fitIterator(...).last</code>
   *
   * @param targetImage target image to fit to.
   * @param searchPointSampler sampler that defines the strategy where profiles are to be sampled.
   * @param iterations maximum number of iterations for the fitting.
   * @param config fitting configuration (thresholds). If omitted, uses [[FittingConfiguration.Default]]
   * @param startingTransformations initial transformations to apply to the statistical model. If omitted, no transformations are applied (i.e. the fitting starts from the mean shape, with no rigid transformation)
   * @return fitting result after the given number of iterations
   */
  def fit(targetImage: DiscreteScalarImage[_3D, Float],
          searchPointSampler: SearchPointSampler,
          iterations: Int,
          config: FittingConfiguration = FittingConfiguration.Default,
          startingTransformations: ModelTransformations = noTransformations): Try[FittingResult] = {

    // we're manually looping the iterator here because we're only interested in the last result -- no need to keep all intermediates.
    val it = fitIterator(targetImage, searchPointSampler, iterations, config, startingTransformations)
    if (!it.hasNext) {
      Failure(new IllegalStateException("iterator was empty"))
    } else {
      var result = it.next()
      while (it.hasNext) {
        result = it.next()
      }
      result
    }
  }

  /**
   * Perform iterative ASM fitting for the given target image. This is essentially the same as the [[fit]] method, except that it returns the full iterator, so every step can be examined.
   * @see [[fit()]] for a description of the parameters.
   *
   */
  def fitIterator(targetImage: DiscreteScalarImage[_3D, Float],
                  searchPointSampler: SearchPointSampler,
                  iterations: Int,
                  config: FittingConfiguration = FittingConfiguration.Default,
                  initialTransform: ModelTransformations = noTransformations): Iterator[Try[FittingResult]] = {
    fitIteratorPreprocessed(preprocessor(targetImage), searchPointSampler, iterations, config, initialTransform)
  }

  /**
   * Perform iterative ASM fitting for the given preprocessed image. This is essentially the same as the [[fitIterator]] method, except that it uses the already preprocessed image.
   * @see [[fit()]] for a description of the parameters.
   *
   */
  def fitIteratorPreprocessed(
    image: PreprocessedImage,
    searchPointSampler: SearchPointSampler,
    iterations: Int,
    config: FittingConfiguration = FittingConfiguration.Default,
    initialTransform: ModelTransformations = noTransformations
  ): Iterator[Try[FittingResult]] = {
    require(iterations > 0, "number of iterations must be strictly positive")

    new Iterator[Try[FittingResult]] {
      var lastResult: Option[Try[FittingResult]] = None
      var nextCount = 0

      override def hasNext = nextCount < iterations && (lastResult.isEmpty || lastResult.get.isSuccess)

      override def next() = {
        val mesh = lastResult
          .map(_.get.mesh)
          .getOrElse(
            statisticalModel.instance(initialTransform.coefficients).transform(initialTransform.rigidTransform)
          )
        lastResult = Some(fitOnce(image, searchPointSampler, config, mesh, initialTransform.rigidTransform))
        nextCount += 1
        lastResult.get
      }
    }
  }

  private def fitOnce(image: PreprocessedImage,
                      sampler: SearchPointSampler,
                      config: FittingConfiguration,
                      mesh: TriangleMesh[_3D],
                      poseTransform: RigidTransformation[_3D]): Try[FittingResult] = {
    val refPtIdsWithTargetPt = findBestCorrespondingPoints(image, mesh, sampler, config, poseTransform)

    if (refPtIdsWithTargetPt.isEmpty) {
      Failure(
        new IllegalStateException("No point correspondences found. You may need to relax the configuration thresholds.")
      )
    } else
      Try {

        val refPtsWithTargetPts = refPtIdsWithTargetPt.map {
          case (refPtId, tgtPt) => (statisticalModel.referenceMesh.pointSet.point(refPtId), tgtPt)
        }
        val bestRigidTransform =
          LandmarkRegistration.rigid3DLandmarkRegistration(refPtsWithTargetPts, poseTransform.rotation.center)

        val refPtIdsWithTargetPtAtModelSpace = refPtIdsWithTargetPt.map {
          case (refPtId, tgtPt) => (refPtId, bestRigidTransform.inverse(tgtPt))
        }
        val bestReconstruction = statisticalModel.posterior(refPtIdsWithTargetPtAtModelSpace, 1e-5).mean
        val coeffs = statisticalModel.coefficients(bestReconstruction)

        val boundedCoeffs = coeffs.map { c =>
          Math.min(config.modelCoefficientBounds, Math.max(-config.modelCoefficientBounds, c))
        }
        val resultMesh = statisticalModel.instance(boundedCoeffs).transform(bestRigidTransform)
        val transformations = ModelTransformations(boundedCoeffs, bestRigidTransform)
        FittingResult(transformations, resultMesh)
      }
  }

  private def refPoint(profileId: ProfileId): Point[_3D] =
    statisticalModel.referenceMesh.pointSet.point(profiles(profileId).pointId)

  private def findBestCorrespondingPoints(
    img: PreprocessedImage,
    mesh: TriangleMesh[_3D],
    sampler: SearchPointSampler,
    config: FittingConfiguration,
    poseTransform: RigidTransformation[_3D]
  ): IndexedSeq[(PointId, Point[_3D])] = {

    val matchingPts = profiles.ids.par.map { index =>
      (profiles(index).pointId,
       findBestMatchingPointAtPoint(img, mesh, index, sampler, config, profiles(index).pointId, poseTransform))
    }

    val matchingPtsWithinDist = matchingPts.filter(_._2.isDefined).map(p => (p._1, p._2.get))
    matchingPtsWithinDist.toIndexedSeq

  }

  private def findBestMatchingPointAtPoint(image: PreprocessedImage,
                                           mesh: TriangleMesh[_3D],
                                           profileId: ProfileId,
                                           searchPointSampler: SearchPointSampler,
                                           config: FittingConfiguration,
                                           pointId: PointId,
                                           poseTransform: RigidTransformation[_3D]): Option[Point[_3D]] = {
    val sampledPoints = searchPointSampler(mesh, pointId)

    val pointsWithFeatureDistances = (for (point <- sampledPoints) yield {
      val featureVectorOpt = featureExtractor(image, point, mesh, pointId)
      featureVectorOpt.map { fv =>
        (point, featureDistance(profileId, fv))
      }
    }).flatten

    if (pointsWithFeatureDistances.isEmpty) {
      // none of the sampled points returned a valid feature vector
      None
    } else {
      val (bestPoint, bestFeatureDistance) = pointsWithFeatureDistances.minBy { case (pt, dist) => dist }

      if (bestFeatureDistance <= config.featureDistanceThreshold) {
        val refPoint = this.refPoint(profileId)

        /** Attention: checking for the deformation vector's pdf needs to be done in the model space !**/
        val inversePoseTransform = poseTransform.inverse
        val bestPointDistance = statisticalModel.gp
          .marginal(pointId)
          .mahalanobisDistance((inversePoseTransform(bestPoint) - refPoint).toBreezeVector)

        if (bestPointDistance <= config.pointDistanceThreshold) {
          Some(bestPoint)
        } else {

          // point distance above user-set threshold
          None
        }
      } else {
        // feature distance above user-set threshold
        None
      }
    }
  }

  private def featureDistance(pid: ProfileId, features: DenseVector[Double]): Double = {
    val mvdAtPoint = profiles(pid).distribution
    mvdAtPoint.mahalanobisDistance(features)
  }

}

/**
 * Fitting Configuration, specifying thresholds and bounds.
 * @param featureDistanceThreshold threshold for the feature distance. If the mahalanobis distance of a candidate point's features to the corresponding profile's mean is larger than this value, then that candidate point will be ignored during fitting.
 * @param pointDistanceThreshold threshold for point distance: If the mahalanobis distance of a candidate point to its corresponding marginal distribution is larger than this value, then that candidate point will be ignored during fitting.
 * @param modelCoefficientBounds bounds to apply on the model coefficients. In other words, by setting this to n, all coefficients of the fitting result will be restricted to the interval [-n, n].
 */
case class FittingConfiguration(featureDistanceThreshold: Double,
                                pointDistanceThreshold: Double,
                                modelCoefficientBounds: Double)

object FittingConfiguration {
  lazy val Default =
    FittingConfiguration(featureDistanceThreshold = 5.0, pointDistanceThreshold = 5.0, modelCoefficientBounds = 3.0)
}

/**
 * Transformations to apply to a statistical shape model.
 *
 * Sample usage: <code>val mesh = ssm.instance(t.coefficients).transform(t.rigidTransform)</code>
 *
 * @param coefficients model coefficients to apply. These determine the shape transformation.
 * @param rigidTransform rigid transformation to apply. These determine translation and rotation.
 */
case class ModelTransformations(coefficients: DenseVector[Double], rigidTransform: RigidTransformation[_3D])

/**
 * Fitting results.
 *
 * Note that the fields are redundant: the mesh is completely determined by the transformations.
 * It's essentially provided for user convenience, because it would be very likely to be (re-)constructed anyway from the transformations.
 *
 * @param transformations transformations to apply to the model
 * @param mesh the mesh resulting from applying these transformations
 */
case class FittingResult(transformations: ModelTransformations, mesh: TriangleMesh[_3D])
