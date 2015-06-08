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

import breeze.linalg.DenseVector
import scalismo.common._
import scalismo.common.UnstructuredPointsDomain3D
import scalismo.geometry.{ Point, _3D }
import scalismo.image.DiscreteScalarImage
import scalismo.mesh.TriangleMesh
import scalismo.numerics.Sampler
import scalismo.registration.{ RigidTransformationSpace, LandmarkRegistration, RigidTransformation, Transformation }
import scalismo.statisticalmodel.{ MultivariateNormalDistribution, StatisticalMeshModel }

import scala.collection.immutable
import scala.util.{ Failure, Try }

object ActiveShapeModel {
  type TrainingData = Iterator[(DiscreteScalarImage[_3D, Float], Transformation[_3D])]

  /**
   * Train an active shape model using an existing PCA model
   */
  def trainModel(statisticalModel: StatisticalMeshModel, trainingData: TrainingData, preprocessor: ImagePreprocessor, featureExtractor: FeatureExtractor, sampler: TriangleMesh => Sampler[_3D]): ActiveShapeModel = {

    val sampled = sampler(statisticalModel.referenceMesh).sample.map(_._1).to[immutable.IndexedSeq]
    val (points, pointIds) = sampled.map(statisticalModel.referenceMesh.findClosestPoint).unzip

    // preprocessed images can be expensive in terms of memory, so we go through them one at a time.
    val imageFeatures = trainingData.flatMap {
      case (image, transform) =>
        val (pimg, mesh) = (preprocessor(image), statisticalModel.referenceMesh.transform(transform))
        pointIds.map { pointId => featureExtractor(pimg, mesh, mesh.point(pointId)) }
    }.toIndexedSeq

    // the structure is "wrongly nested" now, like: {img1:{pt1,pt2}, img2:{pt1,pt2}} (flattened).
    // We merge the corresponding points together, then estimate an MVD.
    val pointsLength = pointIds.length
    val imageRange = (0 until imageFeatures.length / pointsLength).toIndexedSeq
    val pointFeatures = (0 until pointsLength).toIndexedSeq.map { pointIndex =>
      val featuresForPoint = imageRange.flatMap { imageIndex =>
        imageFeatures(imageIndex * pointsLength + pointIndex)
      }
      MultivariateNormalDistribution.estimateFromData(featuresForPoint)
    }

    val profiles = new Profiles(new UnstructuredPointsDomain3D(points), pointFeatures)
    ActiveShapeModel(statisticalModel, profiles, preprocessor, featureExtractor, pointIds)
  }
}

/**
 * Class of instances sampled from an Active Shape Model. A sample is therefore comprised of both a shape sampled from
 * the Shape Model, and a set of sample features at the profile points.
 *
 */
case class ASMSample(mesh: TriangleMesh, featureField: DiscreteFeatureField[_3D], featureExtractor: FeatureExtractor)

case class ActiveShapeModel(statisticalModel: StatisticalMeshModel, profiles: Profiles, preprocessor: ImagePreprocessor, featureExtractor: FeatureExtractor, pointIds: immutable.IndexedSeq[Int]) {

  // type aliases, only to clarify intent:
  /** Index into the mesh's points, i.e. the nth point on a mesh */
  type MeshPointIndex = Int
  /** Index into the ASM profiles or pointIds, i.e. the nth profile */
  type ProfileIndex = Int

  /**
   * Returns the mean mesh of the shape model, along with the mean feature profiles at the profile points
   */
  def mean(): ASMSample = {
    val meanProfilePoints = pointIds.map(id => statisticalModel.mean.point(id))
    val meanFeatures = profiles.values.map(_.mean).toIndexedSeq
    val featureField = DiscreteFeatureField(new UnstructuredPointsDomain3D(meanProfilePoints), meanFeatures)
    ASMSample(statisticalModel.mean, featureField, featureExtractor)
  }

  /**
   * Returns a random sample mesh from the shape model, along with randomly sampled feature profiles at the profile points
   */
  def sample(): ASMSample = {
    val sampleMesh = statisticalModel.sample
    val randomProfilePoints = pointIds.map(id => sampleMesh.point(id))
    val randomFeatures = profiles.values.map(_.sample()).toIndexedSeq
    val featureField = DiscreteFeatureField(new UnstructuredPointsDomain3D(randomProfilePoints), randomFeatures)
    ASMSample(sampleMesh, featureField, featureExtractor)
  }

  /**
   * Utility function that allows to randomly sample different feature profiles, while keeping the profile points
   * Meant to allow to easily inspect/debug the feature distribution
   */
  def sampleFeaturesOnly(): ASMSample = {
    val meanProfilePoints = pointIds.map(id => statisticalModel.mean.point(id))
    val randomFeatures = profiles.values.map(_.sample()).toIndexedSeq
    val featureField = DiscreteFeatureField(new UnstructuredPointsDomain3D(meanProfilePoints), randomFeatures)
    ASMSample(statisticalModel.mean, featureField, featureExtractor)
  }

  /**
   * Returns an Active Shape Model where both the statisitical shape Model and the profile points distributions are correctly transformed
   * according to the provided rigid transformation
   *
   */
  def transform(rigidTransformation: RigidTransformation[_3D]): ActiveShapeModel = {
    val transformedModel = statisticalModel.transform(rigidTransformation)
    val newDomain = new UnstructuredPointsDomain3D(profiles.domain.points.map(rigidTransformation).toIndexedSeq)
    val transformedProfiles = Profiles(newDomain, profiles.data)
    this.copy(statisticalModel = transformedModel, profiles = transformedProfiles)
  }

  private def noTransformations = ModelTransformations(statisticalModel.coefficients(statisticalModel.mean), RigidTransformationSpace[_3D]().transformForParameters(RigidTransformationSpace[_3D]().identityTransformParameters))

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
  def fit(targetImage: DiscreteScalarImage[_3D, Float], searchPointSampler: SearchPointSampler, iterations: Int, config: FittingConfiguration = FittingConfiguration.Default, startingTransformations: ModelTransformations = noTransformations): Try[FittingResult] = {

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
  def fitIterator(targetImage: DiscreteScalarImage[_3D, Float], searchPointSampler: SearchPointSampler, iterations: Int, config: FittingConfiguration = FittingConfiguration.Default, startingTransformations: ModelTransformations = noTransformations): Iterator[Try[FittingResult]] = {
    fitIteratorPreprocessed(preprocessor(targetImage), searchPointSampler, iterations, config, startingTransformations)
  }

  /**
   * Perform iterative ASM fitting for the given preprocessed image. This is essentially the same as the [[fitIterator]] method, except that it uses the already preprocessed image.
   * @see [[fit()]] for a description of the parameters.
   *
   */
  def fitIteratorPreprocessed(image: PreprocessedImage, searchPointSampler: SearchPointSampler, iterations: Int, config: FittingConfiguration = FittingConfiguration.Default, startingTransformations: ModelTransformations = noTransformations): Iterator[Try[FittingResult]] = {
    require(iterations > 0, "number of iterations must be strictly positive")

    new Iterator[Try[FittingResult]] {
      var lastResult: Option[Try[FittingResult]] = None
      var nextCount = 0

      override def hasNext = nextCount < iterations && (lastResult.isEmpty || lastResult.get.isSuccess)

      override def next() = {
        val mesh = lastResult.map(_.get.mesh).getOrElse(statisticalModel.instance(startingTransformations.coefficients).transform(startingTransformations.rigidTransform))
        lastResult = Some(fitOnce(image, searchPointSampler, config, mesh))
        nextCount += 1
        lastResult.get
      }
    }
  }

  private def fitOnce(image: PreprocessedImage, sampler: SearchPointSampler, config: FittingConfiguration, mesh: TriangleMesh): Try[FittingResult] = {
    val refPtIdsWithTargetPt = findBestCorrespondingPoints(image, mesh, sampler, config)

    if (refPtIdsWithTargetPt.isEmpty) {
      Failure(new IllegalStateException("No point correspondences found. You may need to relax the configuration thresholds."))
    } else Try {

      val refPtsWithTargetPts = refPtIdsWithTargetPt.map { case (refPtId, tgtPt) => (statisticalModel.referenceMesh.point(refPtId), tgtPt) }
      val bestRigidTransform = LandmarkRegistration.rigid3DLandmarkRegistration(refPtsWithTargetPts)

      val refPtIdsWithTargetPtAtModelSpace = refPtIdsWithTargetPt.map { case (refPtId, tgtPt) => (refPtId, bestRigidTransform.inverse(tgtPt)) }
      val bestReconstruction = statisticalModel.posterior(refPtIdsWithTargetPtAtModelSpace, 1e-5f).mean
      val coeffs = statisticalModel.coefficients(bestReconstruction)

      val boundedCoeffs = coeffs.map { c => Math.min(config.modelCoefficientBounds, Math.max(-config.modelCoefficientBounds, c)) }
      val resultMesh = statisticalModel.instance(boundedCoeffs).transform(bestRigidTransform)
      val transformations = ModelTransformations(boundedCoeffs, bestRigidTransform)
      FittingResult(transformations, resultMesh)
    }
  }

  private def refPoint(profileIndex: ProfileIndex): Point[_3D] = profiles.domain.point(profileIndex)

  private def refId(profileIndex: ProfileIndex): MeshPointIndex = pointIds(profileIndex)

  private def findBestCorrespondingPoints(img: PreprocessedImage, mesh: TriangleMesh, sampler: SearchPointSampler, config: FittingConfiguration): IndexedSeq[(MeshPointIndex, Point[_3D])] = {
    val matchingPts = pointIds.indices.par.map { sp =>
      (refId(sp), findBestMatchingPointAtPoint(img, mesh, sp, sampler, config))
    }

    val matchingPtsWithinDist = matchingPts.filter(_._2.isDefined).map(p => (p._1, p._2.get))
    matchingPtsWithinDist.toIndexedSeq
  }

  private def findBestMatchingPointAtPoint(image: PreprocessedImage, mesh: TriangleMesh, profileIndex: ProfileIndex, searchPointSampler: SearchPointSampler, config: FittingConfiguration): Option[Point[_3D]] = {
    val refId = this.refId(profileIndex)
    val sampledPoints = searchPointSampler(mesh, refId)

    val pointsWithFeatureDistances = (for (point <- sampledPoints) yield {
      val featureVectorOpt = featureExtractor(image, mesh, point)
      featureVectorOpt.map { fv => (point, featureDistance(profileIndex, fv)) }
    }).flatten

    if (pointsWithFeatureDistances.isEmpty) {
      // none of the sampled points returned a valid feature vector
      None
    } else {
      val (bestPoint, bestFeatureDistance) = pointsWithFeatureDistances.minBy { case (pt, dist) => dist }

      if (bestFeatureDistance <= config.featureDistanceThreshold) {
        val refPoint = this.refPoint(profileIndex)
        val bestPointDistance = statisticalModel.gp.marginal(refId).mahalanobisDistance(bestPoint - refPoint)
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

  private def featureDistance(index: ProfileIndex, features: DenseVector[Float]): Double = {
    val mvdAtPoint = profiles(index)
    mvdAtPoint.mahalanobisDistance(features)
  }

}

/**
 * Fitting Configuration, specifying thresholds and bounds.
 * @param featureDistanceThreshold threshold for the feature distance. If the mahalanobis distance of a candidate point's features to the corresponding profile's mean is larger than this value, then that candidate point will be ignored during fitting.
 * @param pointDistanceThreshold threshold for point distance: If the mahalanobis distance of a candidate point to its corresponding marginal distribution is larger than this value, then that candidate point will be ignored during fitting.
 * @param modelCoefficientBounds bounds to apply on the model coefficients. In other words, by setting this to n, all coefficients of the fitting result will be restricted to the interval [-n, n].
 */
case class FittingConfiguration(featureDistanceThreshold: Float, pointDistanceThreshold: Float, modelCoefficientBounds: Float)

object FittingConfiguration {
  lazy val Default = FittingConfiguration(featureDistanceThreshold = 5.0f, pointDistanceThreshold = 5.0f, modelCoefficientBounds = 3.0f)
}

/**
 * Transformations to apply to a statistical shape model.
 *
 * Sample usage: <code>val mesh = ssm.instance(t.coefficients).transform(t.rigidTransform)</code>
 *
 * @param coefficients model coefficients to apply. These determine the shape transformation.
 * @param rigidTransform rigid transformation to apply. These determine translation and rotation.
 */
case class ModelTransformations(coefficients: DenseVector[Float], rigidTransform: RigidTransformation[_3D])

/**
 * Fitting results.
 *
 * Note that the fields are redundant: the mesh is completely determined by the transformations.
 * It's essentially provided for user convenience, because it would be very likely to be (re-)constructed anyway from the transformations.
 *
 * @param transformations transformations to apply to the model
 * @param mesh the mesh resulting from applying these transformations
 */
case class FittingResult(transformations: ModelTransformations, mesh: TriangleMesh)
