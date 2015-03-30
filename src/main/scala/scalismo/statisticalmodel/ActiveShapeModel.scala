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
package scalismo.statisticalmodel

import breeze.linalg.DenseVector
import ncsa.hdf.`object`.Group
import scalismo.common.{ DiscreteField, SpatiallyIndexedDiscreteDomain }
import scalismo.geometry.{ Point, Vector, _3D }
import scalismo.image.DifferentiableScalarImage
import scalismo.io.{ HDF5File, HDF5ReadWrite }
import scalismo.mesh.TriangleMesh
import scalismo.numerics.FixedPointsUniformMeshSampler3D
import scalismo.registration.Transformation

import scala.util.Try

case class ActiveShapeModel[FE <: ActiveShapeModel.FeatureExtractor](shapeModel: StatisticalMeshModel,
    profileDistributions: ActiveShapeModel.ProfileDistributions,
    featureExtractor: FE) {

  def featureDistance(pt: Point[_3D], featureVec: DenseVector[Float]): Double = {
    val (_, ptId) = profileDistributions.domain.findClosestPoint(pt)
    val distAtPoint = profileDistributions(ptId)

    distAtPoint.mahalanobisDistance(featureVec)
  }
}

object ActiveShapeModel {

  case class ProfileDistributions(domain: SpatiallyIndexedDiscreteDomain[_3D], data: IndexedSeq[MultivariateNormalDistribution])
      extends DiscreteField[_3D, MultivariateNormalDistribution] {
    require(domain.numberOfPoints == data.size)

    def apply(i: Int) = data(i)

    def isDefinedAt(i: Int) = data.isDefinedAt(i)

    def values = data.iterator
  }

  case class TrainingConfiguration(
    randomSeed: Int,
    numberOfSamplingPoints: Int)

  lazy val DefaultTrainingConfiguration = TrainingConfiguration(randomSeed = 42, numberOfSamplingPoints = 500)

  case class FittingConfiguration(
    maxCoefficientStddev: Double,
    maxIntensityStddev: Double,
    maxShapeStddev: Double)

  lazy val DefaultFittingConfiguration = FittingConfiguration(maxCoefficientStddev = 3, maxIntensityStddev = 5, maxShapeStddev = 5)

  type FeatureExtractor = (DifferentiableScalarImage[_3D], TriangleMesh, Point[_3D]) => DenseVector[Float]
  type TrainingData = IndexedSeq[(DifferentiableScalarImage[_3D], Transformation[_3D])]
  type SearchPointSampler = (ActiveShapeModel[_], TriangleMesh, Int) => Seq[Point[_3D]]

  /** The classical feature extractor for active shape modesl */
  case class NormalDirectionFeatureExtractor(numPointsForProfile: Int, profileSpacing: Double) extends ActiveShapeModel.FeatureExtractor {

    def apply(img: DifferentiableScalarImage[_3D], mesh: TriangleMesh, pt: Point[_3D]): DenseVector[Float] = {
      val normal: Vector[_3D] = mesh.normalAtPoint(pt)
      val unitNormal = normal * (1.0 / normal.norm)
      require(math.abs(unitNormal.norm - 1.0) < 1e-5)

      val gradImg = img.differentiate

      val samples = for (i <- (-1 * numPointsForProfile / 2) until (numPointsForProfile / 2)) yield {
        val samplePt = pt + unitNormal * i * profileSpacing
        if (gradImg.isDefinedAt(samplePt)) {
          gradImg(samplePt) dot unitNormal
          //img(samplePt)
        } else
          999f // background value
      }

      val s = samples.map(math.abs).sum
      val featureVec = if (s == 0) samples else samples.map(d => d / s)
      DenseVector(featureVec.toArray)

    }
  }

  object NormalDirectionFeatureExtractor {
    implicit val featureExtractorHDF5Serializer = new HDF5ReadWrite[NormalDirectionFeatureExtractor] {

      override def write(fe: NormalDirectionFeatureExtractor, h5file: HDF5File, group: Group): Try[Unit] = {
        val groupName = group.getFullName + "/" + "NormalDirectionFeatureExtractor"
        for {
          _ <- h5file.writeInt(s"$groupName/numPointsForProfile", fe.numPointsForProfile)
          _ <- h5file.writeFloat(s"$groupName/profileSpacing", fe.profileSpacing.toFloat)
        } yield ()
      }

      override def read(h5file: HDF5File, group: Group): Try[NormalDirectionFeatureExtractor] = {
        val groupName = group.getFullName + "/" + "NormalDirectionFeatureExtractor"
        for {
          numPointsForProfile <- h5file.readInt(s"$groupName/numPointsForProfile")
          profileSpacing <- h5file.readFloat(s"$groupName/profileSpacing")
        } yield NormalDirectionFeatureExtractor(numPointsForProfile, profileSpacing)
      }
    }
  }

  case class NormalDirectionSearchStrategy(numberOfPoints: Int, searchDistance: Double) extends SearchPointSampler {
    def apply(model: ActiveShapeModel[_], curFit: TriangleMesh, ptId: Int): Seq[Point[_3D]] = {

      val curFitPt = curFit.points.toIndexedSeq(ptId)
      val interval = searchDistance * 2 / numberOfPoints

      val normalUnnormalized = curFit.normalAtPoint(curFitPt)
      val normal = normalUnnormalized * (1.0 / normalUnnormalized.norm)
      def samplePtsAlongNormal: Seq[Point[_3D]] = {
        //val interval = distToSearch * 2 / numPts.toFloat
        for (i <- -numberOfPoints / 2 until numberOfPoints / 2) yield {
          curFitPt + normal * i * interval
        }
      }

      samplePtsAlongNormal
    }
  }

  /**
   * Train an active shape model using an existing pca model
   */
  def trainModel[FE <: FeatureExtractor](model: StatisticalMeshModel, trainingData: TrainingData, featureExtractor: FE, config: TrainingConfiguration): ActiveShapeModel[FE] = {

    val sampler = FixedPointsUniformMeshSampler3D(model.referenceMesh, config.numberOfSamplingPoints, config.randomSeed)
    val profilePts = sampler.samplePoints

    val trainingDataSurfaces = for ((image, transform) <- trainingData) yield (image, model.referenceMesh.transform(transform))

    // create for each point a multivariate normal distribution, which is estimated frm the
    // features extrated by the feature extractor
    val featureDistributions = for (pt <- profilePts) yield {
      val (_, ptId) = model.referenceMesh.findClosestPoint(pt)

      // extract features for the given pointsfrom all training datasets
      val featureVectorsAtPt = for ((image, targetSurface) <- trainingDataSurfaces) yield {
        val surfacePt = targetSurface.points.toIndexedSeq(ptId)
        featureExtractor(image, targetSurface, surfacePt)
      }
      MultivariateNormalDistribution.estimateFromData(featureVectorsAtPt)
    }
    val pointData = new ProfileDistributions(SpatiallyIndexedDiscreteDomain.fromSeq[_3D](profilePts), featureDistributions)
    new ActiveShapeModel(model, pointData, featureExtractor)
  }

  def fitModel[FE <: FeatureExtractor](asm: ActiveShapeModel[FE], targetImage: DifferentiableScalarImage[_3D], maxNumIterations: Int, ptGenerator: SearchPointSampler, config: FittingConfiguration): Iterator[TriangleMesh] = {

    fitModel(asm, targetImage, maxNumIterations, ptGenerator, asm.shapeModel.mean, config)
  }

  def fitModel[FE <: FeatureExtractor](model: ActiveShapeModel[FE], targetImage: DifferentiableScalarImage[_3D], maxNumIterations: Int, ptGenerator: SearchPointSampler, startingMesh: TriangleMesh, config: FittingConfiguration): Iterator[TriangleMesh] = {

    Iterator.iterate(startingMesh)((mesh: TriangleMesh) => fitIteration(model, targetImage, mesh, ptGenerator, config))
      .zipWithIndex
      .takeWhile {
        case (_, itNum) => itNum < maxNumIterations
      }
      .map {
        case (mesh, _) => mesh
      }
  }

  private[this] def fitIteration[FE <: ActiveShapeModel.FeatureExtractor](asm: ActiveShapeModel[FE], targetImage: DifferentiableScalarImage[_3D], startingShape: TriangleMesh, ptGenerator: SearchPointSampler, config: FittingConfiguration): TriangleMesh = {

    val refPtIdsWithTargetPt = findBestCorrespondingPoints(asm, startingShape, targetImage, ptGenerator, config)
    val coeffs = asm.shapeModel.coefficients(refPtIdsWithTargetPt, sigma2 = 1e-6)

    val boundedCoeffs = coeffs.map { c => Math.min(config.maxCoefficientStddev, Math.max(-config.maxCoefficientStddev, c)).toFloat }
    asm.shapeModel.instance(boundedCoeffs)
  }

  /**
   * get for each point in the model the one that fits best the description. Start the search from the current fitting result (curFit)
   */
  private[this] def findBestCorrespondingPoints[FE <: ActiveShapeModel.FeatureExtractor](asm: ActiveShapeModel[FE], curFit: TriangleMesh, targetImage: DifferentiableScalarImage[_3D], ptGenerator: SearchPointSampler, config: FittingConfiguration): IndexedSeq[(Int, Point[_3D])] = {
    val searchPts = asm.profileDistributions.domain.points
    val refPtsToSearchWithId = searchPts.map(pt => asm.shapeModel.referenceMesh.findClosestPoint(pt))
    val matchingPts = refPtsToSearchWithId.toIndexedSeq.par.map {
      case (pt, id) =>
        (id, findBestMatchingPointAtPoint(asm, curFit, id, targetImage, ptGenerator, config))
    }

    val matchingPtsWithinDist = matchingPts.filter(_._2.isDefined).map(p => (p._1, p._2.get))
    matchingPtsWithinDist.toIndexedSeq
  }

  /**
   * find the point in the target that is the best match at the given point
   * Retuns Some(Point) if its feature vector is close to a trained feature in terms of the mahalobis distance, otherwise None
   */
  private[this] def findBestMatchingPointAtPoint[FE <: ActiveShapeModel.FeatureExtractor](asm: ActiveShapeModel[FE], curFit: TriangleMesh, ptId: Int, targetImage: DifferentiableScalarImage[_3D], ptGenerator: SearchPointSampler, config: FittingConfiguration): Option[Point[_3D]] = {
    val refPt = asm.shapeModel.referenceMesh.points.toIndexedSeq(ptId)
    val samplePts = ptGenerator(asm, curFit, ptId)

    val ptsWithDists = for (imgPt <- samplePts) yield {
      val featureVector = asm.featureExtractor(targetImage, curFit, imgPt)
      val dist = asm.featureDistance(refPt, featureVector)
      (imgPt, dist)
    }
    val (minPt, minIntensityDist) = ptsWithDists.minBy {
      case (pt, dist) => dist
    }

    val shapeDistForPt = asm.shapeModel.marginal(ptId).mahalanobisDistance(minPt - refPt)
    if (minIntensityDist < config.maxIntensityStddev && shapeDistForPt < config.maxShapeStddev) Some(minPt) else None
  }

}
