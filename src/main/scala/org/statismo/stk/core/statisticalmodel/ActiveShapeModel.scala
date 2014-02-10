package org.statismo.stk.core.statisticalmodel

import org.statismo.stk.core.geometry.ThreeD
import org.statismo.stk.core.mesh.TriangleMesh
import breeze.linalg.DenseVector
import org.statismo.stk.core.geometry.{ Point, Vector }
import org.statismo.stk.core.image.{ContinuousScalarImage, ContinuousScalarImage3D, Interpolation}
import org.statismo.stk.core.io.ImageIO
import java.io.File
import org.statismo.stk.core.io.MeshIO
import org.statismo.stk.core.common.PointData



case class MVNormalPointData(val mesh: TriangleMesh, val values: Array[MultivariateNormalDistribution]) extends PointData[ThreeD, MultivariateNormalDistribution] {
  require(mesh.numberOfPoints == values.size)
  //val valueDimensionality = 1
  override val domain = mesh

}

class ActiveShapeModel(mesh: TriangleMesh, gp: LowRankGaussianProcess[ThreeD],
                       val intensityDistributions: PointData[ThreeD, MultivariateNormalDistribution],
                       val featureExtractor : ActiveShapeModel.FeatureExtractor)
  extends StatisticalMeshModel(mesh, gp) {

  require(intensityDistributions.domain == mesh)


  def featureDistance(ptId: Int, featureVec: DenseVector[Float]): Double = {
    val distAtPoint = intensityDistributions(ptId)

    distAtPoint.mahalanobisDistance(featureVec)
  }
}

object ActiveShapeModel {

  type FeatureExtractor = (ContinuousScalarImage[ThreeD], TriangleMesh, Point[ThreeD]) => DenseVector[Float]
  type TrainingData = IndexedSeq[(ContinuousScalarImage3D, TriangleMesh)]
  type SearchPointSampler = (ActiveShapeModel, TriangleMesh, Int) => Seq[Point[ThreeD]]




  /**
   * Train an active shape model using an existing pca model
   */
  def trainModel(model: StatisticalMeshModel, trainingData: TrainingData, featureExtractor : FeatureExtractor): ActiveShapeModel = {


    // sanity check of the data
    for ((image, surface) <- trainingData) {
      require(surface.numberOfPoints == model.mesh.numberOfPoints)
    }

    // create for each point a multivariate normal distribution, which is estimated frm the
    // features extrated by the feature extractor
    val featureDistributions = for ((pt, ptId) <- model.mesh.points.zipWithIndex) yield {

      // extract features for the given pointsfrom all training datasets
      val featureVectorsAtPt = for ((image, surface) <- trainingData) yield {
        val surfacePts = surface.points.toIndexedSeq
        featureExtractor(image, surface, surfacePts(ptId))
      }
      MultivariateNormalDistribution.estimateFromData(featureVectorsAtPt)
    }
    val pointData = new MVNormalPointData(model.mesh, featureDistributions.toArray)
    new ActiveShapeModel(model.mesh, model.gp, pointData, featureExtractor)
  }



  /**
   * TODO create an iterator from it
   */
  def fitModel(model: ActiveShapeModel, targetImage: ContinuousScalarImage3D, numIterations: Int, ptGenerator : SearchPointSampler): Seq[TriangleMesh] = {

    fitIteration(model, targetImage, Seq(model.mean), 0, numIterations, ptGenerator)
  }

  private[this] def fitIteration(model: ActiveShapeModel, targetImage: ContinuousScalarImage3D, fittingResults: Seq[TriangleMesh], currIt: Int, numIterations: Int, ptGenerator : SearchPointSampler): Seq[TriangleMesh] = {

    println(s"in iteration $currIt")

    if (currIt >= numIterations) return fittingResults

    val referencePoints = model.mesh.points.toIndexedSeq
    val startingShape = fittingResults.last

    val (refInd, targetPoints) = findBestCorrespondingPoints(model, startingShape, targetImage, ptGenerator).unzip

    // compute the posterior
    val gpRegressionTrainingData = (0 until refInd.size) map (i => (referencePoints(i), targetPoints(i) - referencePoints(i)))
    val newFit = model.posterior(gpRegressionTrainingData, sigma2=0.00000001, meanOnly = true).mean

    fitIteration(model, targetImage, fittingResults :+ newFit, currIt + 1, numIterations, ptGenerator)
  }

  /**
    * get for each point in the model the one that fits best the description. Start the search from the current fitting result (curFit)
   */
  private[this] def findBestCorrespondingPoints(model: ActiveShapeModel, curFit: TriangleMesh, targetImage: ContinuousScalarImage3D, ptGenerator : SearchPointSampler): IndexedSeq[(Int, Point[ThreeD])] = {
    val refPts = model.mesh.points.toIndexedSeq
    val matchingPts = for ((pt, id) <- refPts.par.zipWithIndex) yield {
      (id, findBestMatchingPointAtPoint(model, curFit, id, targetImage, ptGenerator))
    }
    matchingPts.toIndexedSeq
  }

  /**
   * find the point in the target that is the best match at the given point
   */
  private[this] def findBestMatchingPointAtPoint(model: ActiveShapeModel, curFit: TriangleMesh, ptId: Int, targetImage: ContinuousScalarImage3D, ptGenerator : SearchPointSampler): Point[ThreeD] = {
    val refPt = model.mesh.points(ptId)
    val curPt = curFit.points(ptId)
    val samplePts = ptGenerator(model, curFit, ptId)

    val ptsWithDists = for (imgPt <- samplePts) yield {
      val featureVector = model.featureExtractor(targetImage, curFit, imgPt)
      val dist = model.featureDistance(ptId, featureVector)
      (imgPt, dist)
    }

    val (minPt, minDist) = ptsWithDists.minBy{case (pt, dist) => dist}

//    println(model.gp.marginal(refPt).mahalanobisDistance((minPt - refPt).toBreezeVector))
    if (model.gp.marginal(refPt).mahalanobisDistance((minPt - refPt).toBreezeVector) > 5) curPt else minPt

//    // return the current point if the mahalanobis distance of the feature is too large
//    //if (minDist < 5) minPt else curFit.points(ptId)
//
//    minPt
  }





}
