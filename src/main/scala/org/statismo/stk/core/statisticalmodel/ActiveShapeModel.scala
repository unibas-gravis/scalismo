package org.statismo.stk.core.statisticalmodel

import org.statismo.stk.core.geometry.ThreeD
import org.statismo.stk.core.mesh.{ScalarMeshData, TriangleMesh}
import breeze.linalg.DenseVector
import org.statismo.stk.core.geometry.{Point, Vector}
import org.statismo.stk.core.image.{ContinuousScalarImage, ContinuousScalarImage3D, Interpolation}
import org.statismo.stk.core.common.{DiscreteDomain, PointData}
import org.statismo.stk.core.registration.Transformation
import org.statismo.stk.core.numerics.FixedPointsUniformMeshSampler3D
import org.statismo.stk.core.common.UnstructuredPointsDomain


case class ASMProfileDistributions(val domain: UnstructuredPointsDomain[ThreeD], val values: Array[MultivariateNormalDistribution]) extends PointData[ThreeD, MultivariateNormalDistribution] {
  require(domain.numberOfPoints == values.size)

}

case class ActiveShapeModel[FE <: ActiveShapeModel.FeatureExtractor](shapeModel : StatisticalMeshModel,
                       val intensityDistributions: ASMProfileDistributions,
                       val featureExtractor: FE)
  extends StatisticalMeshModel(shapeModel.mesh, shapeModel.gp) {



  def featureDistance(pt: Point[ThreeD], featureVec: DenseVector[Float]): Double = {
    val (_, ptId) = intensityDistributions.domain.findClosestPoint(pt)
    val distAtPoint = intensityDistributions(ptId)

    distAtPoint.mahalanobisDistance(featureVec)
  }


}

object ActiveShapeModel {

  case class ASMTrainingConfig(
                                val randomSeed: Int,
                                val numberOfSamplingPoints: Int)

  lazy val DefaultTrainingConfig = ASMTrainingConfig(randomSeed = 42, numberOfSamplingPoints = 500)

  case class ASMFittingConfig(
                               val maxCoefficientStddev: Double,
                               val maxIntensityStddev: Double,
                               val maxShapeStddev: Double
                               )

  lazy val DefaultFittingConfig = ASMFittingConfig( maxCoefficientStddev = 3,  maxIntensityStddev = 5,   maxShapeStddev = 5)


  type FeatureExtractor = (ContinuousScalarImage[ThreeD], TriangleMesh, Point[ThreeD]) => DenseVector[Float]
  type TrainingData = IndexedSeq[(ContinuousScalarImage3D, Transformation[ThreeD])]
  type SearchPointSampler = (ActiveShapeModel[_], TriangleMesh, Int) => Seq[Point[ThreeD]]


  /**
   * Train an active shape model using an existing pca model
   */
  def trainModel(model: StatisticalMeshModel, trainingData: TrainingData, featureExtractor: FeatureExtractor, config: ASMTrainingConfig): ActiveShapeModel[FeatureExtractor] = {

    val sampler = FixedPointsUniformMeshSampler3D(model.mesh, config.numberOfSamplingPoints, config.randomSeed)
    val profilePts = sampler.samplePoints

    val trainingDataSurfaces = for ((image, transform) <- trainingData) yield (image, model.mesh.warp(transform))

    // create for each point a multivariate normal distribution, which is estimated frm the
    // features extrated by the feature extractor
    val featureDistributions = for (pt <- profilePts) yield {
      val (_, ptId) = model.mesh.findClosestPoint(pt)

      // extract features for the given pointsfrom all training datasets
      val featureVectorsAtPt = for ((image, targetSurface) <- trainingDataSurfaces) yield {
        val surfacePt = targetSurface.points(ptId)
        featureExtractor(image, targetSurface, surfacePt)
      }
      MultivariateNormalDistribution.estimateFromData(featureVectorsAtPt)
    }
    val pointData = new ASMProfileDistributions(new UnstructuredPointsDomain[ThreeD](profilePts), featureDistributions.toArray)
    new ActiveShapeModel(model, pointData, featureExtractor)
  }


  def fitModel[FE <: FeatureExtractor](model: ActiveShapeModel[FE], targetImage: ContinuousScalarImage3D, maxNumIterations: Int, ptGenerator: SearchPointSampler, config: ASMFittingConfig): Iterator[TriangleMesh] = {

    fitModel(model, targetImage, maxNumIterations, ptGenerator, model.mean, config)
  }

  def fitModel[FE <: FeatureExtractor](model: ActiveShapeModel[FE], targetImage: ContinuousScalarImage3D, maxNumIterations: Int, ptGenerator: SearchPointSampler, startingMesh: TriangleMesh, config: ASMFittingConfig): Iterator[TriangleMesh] = {

    Iterator.iterate(startingMesh)((mesh: TriangleMesh) => fitIteration(model, targetImage, mesh, ptGenerator, config)    )
      .zipWithIndex
      .takeWhile {
      case (_, itNum) => (itNum < maxNumIterations)
    }
      .map {
      case (mesh, _) => mesh
    }
  }


  private[this] def fitIteration[FE <: ActiveShapeModel.FeatureExtractor](model: ActiveShapeModel[FE], targetImage: ContinuousScalarImage3D, startingShape: TriangleMesh, ptGenerator: SearchPointSampler, config: ASMFittingConfig): TriangleMesh = {

    val referencePoints = model.mesh.points.toIndexedSeq


    val refPtIdsWithTargetPt = findBestCorrespondingPoints(model, startingShape, targetImage, ptGenerator, config)

    // project mesh into the model
    val gpRegressionTrainingData = refPtIdsWithTargetPt.map {
      case (refId, targetPt) => (referencePoints(refId), targetPt - referencePoints(refId))
    }
    val coeffs = model.gp.coefficients(gpRegressionTrainingData, sigma2 = 1e-6)
    val uncorrectedMesh = model.instance(coeffs)
    val correctedCoeffs = coeffs.map {
      c => c match {
        case c if c > config.maxCoefficientStddev => config.maxCoefficientStddev
        case c if c >= -config.maxCoefficientStddev && c <= config.maxCoefficientStddev => c
        case _ => -config.maxCoefficientStddev
      }
    }
    val newFit = model.instance(correctedCoeffs.map(_.toFloat))
    newFit
  }

  /**
   * get for each point in the model the one that fits best the description. Start the search from the current fitting result (curFit)
   */
  private[this] def findBestCorrespondingPoints[FE <: ActiveShapeModel.FeatureExtractor](model: ActiveShapeModel[FE], curFit: TriangleMesh, targetImage: ContinuousScalarImage3D, ptGenerator: SearchPointSampler, config: ASMFittingConfig): IndexedSeq[(Int, Point[ThreeD])] = {
    val searchPts = model.intensityDistributions.domain.points
    val refPtsToSearchWithId = searchPts.map(pt => model.mesh.findClosestPoint(pt))
    val matchingPts = for ((pt, id) <- refPtsToSearchWithId.par) yield {
      (id, findBestMatchingPointAtPoint(model, curFit, id, targetImage, ptGenerator, config))
    }
    val matchingPtsWithinDist = for ((id, optPt) <- matchingPts if optPt.isDefined) yield (id, optPt.get)
    matchingPtsWithinDist.toIndexedSeq
  }

  /**
   * find the point in the target that is the best match at the given point
   * Retuns Some(Point) if its feature vector is close to a trained feature in terms of the mahalobis distance, otherwise None
   */
  private[this] def findBestMatchingPointAtPoint[FE <: ActiveShapeModel.FeatureExtractor](model: ActiveShapeModel[FE], curFit: TriangleMesh, ptId: Int, targetImage: ContinuousScalarImage3D, ptGenerator: SearchPointSampler, config: ASMFittingConfig): Option[Point[ThreeD]] = {
    val refPt = model.mesh.points(ptId)
    val curPt = curFit.points(ptId)
    val samplePts = ptGenerator(model, curFit, ptId)

    val ptsWithDists = for (imgPt <- samplePts) yield {
      val featureVector = model.featureExtractor(targetImage, curFit, imgPt)
      val dist = model.featureDistance(refPt, featureVector)
      (imgPt, dist)
    }
    val (minPt, minIntensityDist) = ptsWithDists.minBy {
      case (pt, dist) => dist
    }

    val shapeDistForPt = model.gp.marginal(refPt).mahalanobisDistance((minPt - refPt).toBreezeVector)
    if (minIntensityDist < config.maxIntensityStddev && shapeDistForPt < config.maxShapeStddev) Some(minPt) else None
  }


}
