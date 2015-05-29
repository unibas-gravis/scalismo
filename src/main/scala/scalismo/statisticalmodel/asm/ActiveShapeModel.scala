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
import scalismo.geometry.{ Point, _3D }
import scalismo.image.DiscreteScalarImage
import scalismo.mesh.TriangleMesh
import scalismo.numerics.Sampler
import scalismo.registration.{ LandmarkRegistration, RigidTransformation, Transformation }
import scalismo.statisticalmodel.{ MultivariateNormalDistribution, StatisticalMeshModel }

import scala.collection.immutable
import scala.util.{ Failure, Try }

object ActiveShapeModel {
  type TrainingData = Iterator[(DiscreteScalarImage[_3D, Float], Transformation[_3D])]
  // just a type alias to clarify intent
  type PointId = Int

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
        pointIds.map { pointId => featureExtractor(pimg, mesh, mesh.points(pointId)) }
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

    val profiles = new Profiles(SpatiallyIndexedDiscreteDomain.fromSeq[_3D](points), pointFeatures)
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

  import ActiveShapeModel.PointId

  def featureDistance(pointId: PointId, features: DenseVector[Float]): Double = {
    val mvdAtPoint = profiles(pointId)
    mvdAtPoint.mahalanobisDistance(features)
  }

  def fit(targetImage: DiscreteScalarImage[_3D, Float], searchPointSampler: SearchPointSampler, maxIterations: Int, config: FitConfiguration = FitConfiguration.Default, startingMesh: TriangleMesh = statisticalModel.mean): Try[FitResult] = {

    // we're manually looping the iterator here because we're only interested in the last result -- no need to keep all intermediates.
    val it = fitIterator(targetImage, startingMesh, searchPointSampler, config, maxIterations)
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

  def fitIterator(targetImage: DiscreteScalarImage[_3D, Float], startingMesh: TriangleMesh, searchPointSampler: SearchPointSampler, config: FitConfiguration, iterations: Int): Iterator[Try[FitResult]] = {
    fitIterator(preprocessor(targetImage), startingMesh, searchPointSampler, config, iterations)
  }

  def fitIterator(image: PreprocessedImage, startingMesh: TriangleMesh, searchPointSampler: SearchPointSampler, config: FitConfiguration, iterations: Int): Iterator[Try[FitResult]] = {
    require(iterations > 0, "number of iterations must be strictly positive")

    new Iterator[Try[FitResult]] {
      var lastResult: Option[Try[FitResult]] = None
      var nextCount = 0

      override def hasNext = nextCount < iterations && (lastResult.isEmpty || lastResult.get.isSuccess)

      override def next() = {
        val mesh = lastResult.map(_.get.mesh).getOrElse(startingMesh)
        lastResult = Some(fitOnce(image, mesh, searchPointSampler, config))
        nextCount += 1
        lastResult.get
      }
    }
  }

  /**
   * Returns the mean mesh of the shape model, along with the mean feature profiles at the profile points
   */
  def mean(): ASMSample = {
    val meanProfilePoints = pointIds.map(id => statisticalModel.mean(id))
    val meanFeatures = profiles.values.map(_.mean).toIndexedSeq
    val featureField = DiscreteFeatureField(meanProfilePoints zip meanFeatures)
    ASMSample(statisticalModel.mean, featureField, featureExtractor)
  }

  /**
   * Returns a random sample mesh from the shape model, along with randomly sampled feature profiles at the profile points
   */
  def sample(): ASMSample = {
    val sampleMesh = statisticalModel.sample
    val randomProfilePoints = pointIds.map(id => sampleMesh(id))
    val randomFeatures = profiles.values.map(_.sample()).toIndexedSeq
    val featureField = DiscreteFeatureField(randomProfilePoints zip randomFeatures)
    ASMSample(sampleMesh, featureField, featureExtractor)
  }

  /**
   * Utility function that allows to randomly sample different feature profiles, while keeping the profile points
   * Meant to allow to easily inspect/debug the feature distribution
   */

  def sampleFeaturesOnly(): ASMSample = {
    val meanProfilePoints = pointIds.map(id => statisticalModel.mean(id))
    val randomFeatures = profiles.values.map(_.sample()).toIndexedSeq
    val featureField = DiscreteFeatureField(meanProfilePoints zip randomFeatures)
    ASMSample(statisticalModel.mean, featureField, featureExtractor)
  }

  private def fitOnce(image: PreprocessedImage, mesh: TriangleMesh, sampler: SearchPointSampler, config: FitConfiguration): Try[FitResult] = {
    val refPtIdsWithTargetPt = findBestCorrespondingPoints(image, mesh, sampler, config)

    if (refPtIdsWithTargetPt.isEmpty) {
      Failure(new IllegalStateException("No point correspondences found. You may need to relax the configuration thresholds."))
    } else Try {

      val refPtsWithTargetPts = refPtIdsWithTargetPt.map { case (refPtId, tgtPt) => (statisticalModel.referenceMesh.points(refPtId), tgtPt) }
      val bestRigidTransform = LandmarkRegistration.rigid3DLandmarkRegistration(refPtsWithTargetPts)

      val refPtIdsWithTargetPtAtModelSpace = refPtIdsWithTargetPt.map { case (refPtId, tgtPt) => (refPtId, bestRigidTransform.inverse(tgtPt)) }

      val bestReconstruction = statisticalModel.posterior(refPtIdsWithTargetPtAtModelSpace, 1e-5f).mean
      val coeffs = statisticalModel.coefficients(bestReconstruction)

      val boundedCoeffs = coeffs.map { c => Math.min(config.modelCoefficientBounds, Math.max(-config.modelCoefficientBounds, c)) }
      val resultMesh = statisticalModel.instance(boundedCoeffs).transform(bestRigidTransform)
      FitResult(resultMesh, boundedCoeffs, bestRigidTransform)
    }
  }

  private def refPoint(profileIndex: Int): Point[_3D] = profiles.domain.points(profileIndex)

  private def refId(profileIndex: Int): PointId = pointIds(profileIndex)

  private def findBestCorrespondingPoints(img: PreprocessedImage, mesh: TriangleMesh, sampler: SearchPointSampler, config: FitConfiguration): IndexedSeq[(PointId, Point[_3D])] = {
    val matchingPts = pointIds.indices.par.map { sp =>
      (refId(sp), findBestMatchingPointAtPoint(img, mesh, sp, sampler, config))
    }

    val matchingPtsWithinDist = matchingPts.filter(_._2.isDefined).map(p => (p._1, p._2.get))
    matchingPtsWithinDist.toIndexedSeq
  }

  private def findBestMatchingPointAtPoint(image: PreprocessedImage, mesh: TriangleMesh, profileIndex: PointId, searchPointSampler: SearchPointSampler, config: FitConfiguration): Option[Point[_3D]] = {
    val refId = this.refId(profileIndex)
    val sampledPoints = searchPointSampler(mesh, refId)

    val pointsWithFeatureDistances = (for (point <- sampledPoints) yield {
      val featureVectorOpt = featureExtractor(image, mesh, point)
      featureVectorOpt.map { fv => (point, featureDistance(profileIndex, fv)) }
    }).flatten

    if (pointsWithFeatureDistances.isEmpty) {
      // none of the sampled points return a valid feature vector
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

}

case class FitConfiguration(featureDistanceThreshold: Float, pointDistanceThreshold: Float, modelCoefficientBounds: Float)

object FitConfiguration {
  lazy val Default = FitConfiguration(featureDistanceThreshold = 5.0f, pointDistanceThreshold = 5.0f, modelCoefficientBounds = 3.0f)
}

case class FitResult(mesh: TriangleMesh, coefficients: DenseVector[Float], transform: RigidTransformation[_3D])