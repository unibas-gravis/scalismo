package scalismo.statisticalmodel.asm

import breeze.linalg.DenseVector
import scalismo.common.SpatiallyIndexedDiscreteDomain
import scalismo.geometry.{ Point, _3D }
import scalismo.image.DiscreteScalarImage
import scalismo.mesh.TriangleMesh
import scalismo.numerics.{ Sampler, FixedPointsUniformMeshSampler3D }
import scalismo.registration.{ LandmarkRegistration, RigidTransformation, Transformation }
import scalismo.statisticalmodel.{ MultivariateNormalDistribution, StatisticalMeshModel }

import scala.collection.{ AbstractIterator, immutable }
import scala.util.{ Failure, Try }

object ActiveShapeModel {
  type TrainingData = immutable.IndexedSeq[(DiscreteScalarImage[_3D, Float], Transformation[_3D])]
  // just a type alias to clarify intent
  type PointId = Int

  /**
   * Train an active shape model using an existing PCA model
   */
  def trainModel(statisticalModel: StatisticalMeshModel, trainingData: TrainingData, featureExtractor: FeatureExtractor, sampler: TriangleMesh => Sampler[_3D]): ActiveShapeModel = {

    val sampled = sampler(statisticalModel.referenceMesh).sample.map(_._1).to[immutable.IndexedSeq]
    val (points, pointIds) = sampled.map(statisticalModel.referenceMesh.findClosestPoint).unzip

    // Feature images can be expensive in terms of memory, so we go through them one at a time.
    val imageFeatures = trainingData.map {
      case (image, transform) =>
        val (fei, mesh) = (featureExtractor(image), statisticalModel.referenceMesh.transform(transform))
        pointIds.map { pointId => fei(mesh)(mesh.points(pointId)) }
    }.flatten

    // the structure is "wrongly nested" now, like: {img1:{pt1,pt2}, img2:{pt1,pt2}} (flattened).
    // We merge the corresponding points together, then estimate an MVD.
    val pointsLength = pointIds.length
    val imageRange = (0 until trainingData.length).toIndexedSeq
    val pointFeatures = (0 until pointsLength).toIndexedSeq.map { pointIndex =>
      val featuresForPoint = imageRange.map { imageIndex =>
        imageFeatures(imageIndex * pointsLength + pointIndex)
      }
      MultivariateNormalDistribution.estimateFromData(featuresForPoint)
    }

    val profiles = new Profiles(SpatiallyIndexedDiscreteDomain.fromSeq[_3D](points), pointFeatures)
    new ActiveShapeModel(statisticalModel, profiles, featureExtractor, pointIds)
  }
}

case class ActiveShapeModel(statisticalModel: StatisticalMeshModel, profiles: Profiles, featureExtractor: FeatureExtractor, pointIds: immutable.IndexedSeq[Int]) {
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
    fitIterator(featureExtractor(targetImage), startingMesh, searchPointSampler, config, iterations)
  }

  def fitIterator(fe: FeatureImageGenerator, startingMesh: TriangleMesh, searchPointSampler: SearchPointSampler, config: FitConfiguration, iterations: Int): Iterator[Try[FitResult]] = {
    require(iterations > 0, "number of iterations must be strictly positive")

    new Iterator[Try[FitResult]] {
      var lastResult: Option[Try[FitResult]] = None
      var nextCount = 0
      override def hasNext = nextCount < iterations && (lastResult.isEmpty || lastResult.get.isSuccess)
      override def next() = {
        val mesh = lastResult.map(_.get.mesh).getOrElse(startingMesh)
        lastResult = Some(fitOnce(fe, mesh, searchPointSampler, config))
        nextCount += 1
        lastResult.get
      }
    }
  }

  private def fitOnce(image: FeatureImageGenerator, mesh: TriangleMesh, sampler: SearchPointSampler, config: FitConfiguration): Try[FitResult] = {
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

  private def findBestCorrespondingPoints(fe: FeatureImageGenerator, mesh: TriangleMesh, sampler: SearchPointSampler, config: FitConfiguration): IndexedSeq[(PointId, Point[_3D])] = {
    val matchingPts = (0 until pointIds.length).par.map { sp =>
      (refId(sp), findBestMatchingPointAtPoint(fe, mesh, sp, sampler, config))
    }

    val matchingPtsWithinDist = matchingPts.filter(_._2.isDefined).map(p => (p._1, p._2.get))
    matchingPtsWithinDist.toIndexedSeq
  }

  private def findBestMatchingPointAtPoint(featureImage: FeatureImageGenerator, mesh: TriangleMesh, profileIndex: PointId, searchPointSampler: SearchPointSampler, config: FitConfiguration): Option[Point[_3D]] = {
    val refId = this.refId(profileIndex)
    val sampledPoints = searchPointSampler(mesh, refId)

    val pointsWithFeatureDistances = for (point <- sampledPoints) yield {
      val featureVector = featureImage(mesh)(point)
      val distance = featureDistance(profileIndex, featureVector)
      (point, distance)
    }

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

case class FitConfiguration(featureDistanceThreshold: Float, pointDistanceThreshold: Float, modelCoefficientBounds: Float)

object FitConfiguration {
  lazy val Default = FitConfiguration(featureDistanceThreshold = 5.0f, pointDistanceThreshold = 5.0f, modelCoefficientBounds = 3.0f)
}

case class FitResult(mesh: TriangleMesh, coefficients: DenseVector[Float], transform: RigidTransformation[_3D])