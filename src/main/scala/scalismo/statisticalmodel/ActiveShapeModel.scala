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

import breeze.linalg.{ Axis, DenseVector }
import ncsa.hdf.`object`.Group
import scalismo.common.{ DiscreteField, SpatiallyIndexedDiscreteDomain }
import scalismo.geometry.{ Point, Vector, _3D }
import scalismo.image.{ DiscreteScalarImage, DifferentiableScalarImage }
import scalismo.image.filter.{ GaussianFilter3D, BoxedFilter }
import scalismo.io.{ ActiveShapeModelIO, HDF5File, HDF5ReadWrite }
import scalismo.mesh.TriangleMesh
import scalismo.numerics.FixedPointsUniformMeshSampler3D
import scalismo.registration.Transformation
import scalismo.statisticalmodel.ActiveShapeModel.FeatureExtractor.NormalDirectionFeatureExtractor

import scala.collection.parallel.ParSeq
import scala.util.Try

case class ActiveShapeModel[FE <: ActiveShapeModel.FeatureExtractor](shapeModel: StatisticalMeshModel,
    profileDistributions: ActiveShapeModel.ProfileDistributions,
    featureExtractor: FE) {

  def featureDistance(ptId: Int, featureVec: DenseVector[Float]): Double = {
    val distAtPoint = profileDistributions(ptId)
    distAtPoint.mahalanobisDistance(featureVec)
  }

  def featureDistance(pt: Point[_3D], featureVec: DenseVector[Float]): Double = {
    featureDistance(profileDistributions.domain.findClosestPoint(pt)._2, featureVec)
  }
}

object ActiveShapeModel {
  def bugfix(oldModel: ActiveShapeModel[NormalDirectionFeatureExtractor], limit: Option[Int] = None): ActiveShapeModel[NormalDirectionFeatureExtractor] = {
    val oldFe = oldModel.featureExtractor
    val newFe = NormalDirectionFeatureExtractor(oldFe.numPointsForProfile - 2, oldFe.profileSpacing, oldFe.smoothing)
    val newProfileDistributions = {
      val d = oldModel.profileDistributions
      val newData = d.data.map { oldMvd =>
        val om = oldMvd.mean
        val oc = oldMvd.cov
        val nm = om.slice(1, om.length)
        val nc = oc.delete(0, Axis._0).delete(0, Axis._1)
        MultivariateNormalDistribution(nm, nc)
      }
      if (!limit.isDefined) {
        ProfileDistributions(d.domain, newData)
      } else {
        val count = limit.get
        ProfileDistributions(SpatiallyIndexedDiscreteDomain(d.domain.points.toIndexedSeq.take(count), count), newData.take(count))
      }
    }
    ActiveShapeModel(oldModel.shapeModel, newProfileDistributions, newFe)
  }

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

  type TrainingData = IndexedSeq[(DiscreteScalarImage[_3D, Float], Transformation[_3D])]

  // TODO: document
  //  trait FeatureExtractor extends Function3[DifferentiableScalarImage[_3D], TriangleMesh, Point[_3D], DenseVector[Float]] {
  //  }
  trait FeatureExtractor {
    def getInstance(image: DiscreteScalarImage[_3D, Float]): FeatureExtractorInstance
  }

  trait FeatureExtractorInstance {
    def extractFeatures(mesh: TriangleMesh, point: Point[_3D]): DenseVector[Float]
  }

  object FeatureExtractor {

    val TypeAttributeName = "type"

    /** The classical feature extractor for active shape models */
    case class NormalDirectionFeatureExtractor(numPointsForProfile: Int, profileSpacing: Double, smoothing: Int, isOldVersionWithRangeBug: Boolean = false) extends ActiveShapeModel.FeatureExtractor {
      override def getInstance(image: DiscreteScalarImage[_3D, Float]): FeatureExtractorInstance = new FeatureExtractorInstance {
        val gradImg = smoothing match {
          case 0 => image.interpolate(1).differentiate
          case 1 => image.interpolate(1).convolve(BoxedFilter(profileSpacing.toFloat), 3).differentiate
          case 2 => image.interpolate(1).convolve(GaussianFilter3D(profileSpacing), 3).differentiate
          case _ => throw new IllegalArgumentException
        }

        override def extractFeatures(mesh: TriangleMesh, point: Point[_3D]): DenseVector[Float] = {
          val normal: Vector[_3D] = mesh.normalAtPoint(point)
          val unitNormal = normal * (1.0 / normal.norm)
          require(math.abs(unitNormal.norm - 1.0) < 1e-5)

          //        println(s"unit normal: $unitNormal")
          val range = if (isOldVersionWithRangeBug) {
            (-1 * numPointsForProfile / 2) until (numPointsForProfile / 2)
          } else {
            (-1 * numPointsForProfile / 2) to (numPointsForProfile / 2)
          }
          val samples = for (i <- range) yield {
            val samplePt = point + unitNormal * i * profileSpacing
            //println(s"sample Point: $samplePt")
            if (gradImg.isDefinedAt(samplePt)) {
              //            println(s"image at $samplePt: ${img(samplePt)}")
              //            println(s"gradient at $samplePt: ${gradImg(samplePt)}")
              //            val i = img(samplePt);
              val r = gradImg(samplePt) dot unitNormal
              //            println(s"result at $samplePt: $r")
              r
            } else
              999f // background value
          }

          val s = samples.map(math.abs).sum
          val featureVec = if (s == 0) samples else samples.map(d => d / s)
          DenseVector(featureVec.toArray)
        }
      }
    }

    object NormalDirectionFeatureExtractor {
      implicit val featureExtractorHDF5Serializer = new HDF5ReadWrite[NormalDirectionFeatureExtractor] {
        val NumberOfPoints = "numberOfPoints"
        val Spacing = "spacing"
        val Type = "builtin::NormalGradient"
        override def write(fe: NormalDirectionFeatureExtractor, h5file: HDF5File, group: Group): Try[Unit] = {
          // FIXME
          if (group.getName != ActiveShapeModelIO.Names.Group.FeatureExtractor) {
            writeOld(fe, h5file, group)
          } else {
            val groupName = group.getFullName
            for {
              _ <- h5file.writeStringAttribute(groupName, TypeAttributeName, Type)
              _ <- h5file.writeInt(s"$groupName/$NumberOfPoints", fe.numPointsForProfile)
              _ <- h5file.writeInt(s"$groupName/Smoothing", fe.smoothing)
              _ <- h5file.writeFloat(s"$groupName/$Spacing", fe.profileSpacing.toFloat)
            } yield ()
          }
        }

        def writeOld(fe: NormalDirectionFeatureExtractor, h5file: HDF5File, group: Group): Try[Unit] = {
          val groupName = group.getFullName + "/" + "NormalDirectionFeatureExtractor"
          for {
            _ <- h5file.writeInt(s"$groupName/numPointsForProfile", fe.numPointsForProfile)
            _ <- h5file.writeFloat(s"$groupName/profileSpacing", fe.profileSpacing.toFloat)
          } yield ()
        }

        override def read(h5file: HDF5File, group: Group): Try[NormalDirectionFeatureExtractor] = {
          if (group.getName != ActiveShapeModelIO.Names.Group.FeatureExtractor) {
            readOld(h5file, group)
          } else {
            val groupName = group.getFullName
            for {
              numPointsForProfile <- h5file.readInt(s"$groupName/$NumberOfPoints")
              smoothing <- h5file.readInt(s"$groupName/Smoothing")
              profileSpacing <- h5file.readFloat(s"$groupName/$Spacing")
            } yield NormalDirectionFeatureExtractor(numPointsForProfile, profileSpacing, smoothing)
          }
        }

        def readOld(h5file: HDF5File, group: Group): Try[NormalDirectionFeatureExtractor] = {
          val groupName = group.getFullName + "/" + "NormalDirectionFeatureExtractor"
          for {
            numPointsForProfile <- h5file.readInt(s"$groupName/numPointsForProfile")
            profileSpacing <- h5file.readFloat(s"$groupName/profileSpacing")
          } yield NormalDirectionFeatureExtractor(numPointsForProfile, profileSpacing, smoothing = 0, isOldVersionWithRangeBug = true)
        }
      }
    }
  }

  // TODO: document
  trait SearchPointSampler extends Function3[ActiveShapeModel[_], TriangleMesh, Int, Seq[Point[_3D]]] {

  }

  object SearchPointSampler {
    case class NormalDirectionSearchPointSampler(numberOfPoints: Int, searchDistance: Double) extends SearchPointSampler {
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
  }

  /**
   * Train an active shape model using an existing pca model
   */
  def trainModel[FE <: FeatureExtractor](model: StatisticalMeshModel, trainingData: TrainingData, featureExtractor: FE, config: TrainingConfiguration): ActiveShapeModel[FE] = {

    val sampler = FixedPointsUniformMeshSampler3D(model.referenceMesh, config.numberOfSamplingPoints, config.randomSeed)
    val profilePts = sampler.samplePoints

    val feAndSurfaces = for ((image, transform) <- trainingData) yield (featureExtractor.getInstance(image), model.referenceMesh.transform(transform))

    // create for each point a multivariate normal distribution, which is estimated frm the
    // features extrated by the feature extractor

    val featureDistributions = for (pt <- profilePts) yield {
      val (_, ptId) = model.referenceMesh.findClosestPoint(pt)

      // extract features for the given pointsfrom all training datasets
      val featureVectorsAtPt = for ((fe, targetSurface) <- feAndSurfaces) yield {
        val surfacePt = targetSurface.points.toIndexedSeq(ptId)
        fe.extractFeatures(targetSurface, surfacePt)
        //featureExtractor(image, targetSurface, surfacePt)
      }
      MultivariateNormalDistribution.estimateFromData(featureVectorsAtPt)
    }
    val pointData = new ProfileDistributions(SpatiallyIndexedDiscreteDomain.fromSeq[_3D](profilePts), featureDistributions)
    new ActiveShapeModel(model, pointData, featureExtractor)
  }

  def fitModel[FE <: FeatureExtractor](asm: ActiveShapeModel[FE], targetImage: DiscreteScalarImage[_3D, Float], maxIterations: Int, searchPointSampler: SearchPointSampler, config: FittingConfiguration): Iterator[TriangleMesh] = {

    fitModel(asm, targetImage, maxIterations, searchPointSampler, asm.shapeModel.mean, config)
  }

  private[ActiveShapeModel] case class SearchPoint(refPoint: Point[_3D], refId: Int, profileId: Int)
  type SearchPoints = ParSeq[SearchPoint]

  def fitModel[FE <: FeatureExtractor](model: ActiveShapeModel[FE], targetImage: DiscreteScalarImage[_3D, Float], maxIterations: Int, searchPointSampler: SearchPointSampler, startingMesh: TriangleMesh, config: FittingConfiguration): Iterator[TriangleMesh] = {

    // pre-calculate the points to check, so that we don't re-calculate them every time
    val searchPoints = model.profileDistributions.domain.points.toIndexedSeq.map { pt =>
      val (refPt, refId) = model.shapeModel.referenceMesh.findClosestPoint(pt)
      val (_, profId) = model.profileDistributions.domain.findClosestPoint(pt)
      // FIXME: pt or refPt?
      SearchPoint(refPt, refId, profId)
    }.par

    val fe = model.featureExtractor.getInstance(targetImage)

    Iterator.iterate(startingMesh)((mesh: TriangleMesh) => fitIteration(model, fe, mesh, searchPoints, searchPointSampler, config))
      .zipWithIndex
      .takeWhile {
        case (_, itNum) => itNum < maxIterations
      }
      .map {
        case (mesh, _) => mesh
      }
  }

  private[this] def fitIteration[FE <: FeatureExtractor](asm: ActiveShapeModel[FE], fe: FeatureExtractorInstance, startingMesh: TriangleMesh, searchPoints: SearchPoints, searchPointSampler: SearchPointSampler, config: FittingConfiguration): TriangleMesh = {
    val refPtIdsWithTargetPt = findBestCorrespondingPoints(asm, fe, startingMesh, searchPoints, searchPointSampler, config)
    val coeffs = asm.shapeModel.coefficients(refPtIdsWithTargetPt, sigma2 = 1e-6)

    val boundedCoeffs = coeffs.map { c => Math.min(config.maxCoefficientStddev, Math.max(-config.maxCoefficientStddev, c)).toFloat }
    if (Debug) println(s"Found ${refPtIdsWithTargetPt.size} constraints")
    val avgabs = boundedCoeffs.toArray.toIndexedSeq.take(10).map { c => Math.abs(c) }.sum / 10
    if (Debug) println(s"bounded coefficients (avgabs: $avgabs): $boundedCoeffs")
    asm.shapeModel.instance(boundedCoeffs)
  }

  /**
   * for each profile point in the model, get the point in mesh that best fits the corresponding profile.
   */
  private[this] def findBestCorrespondingPoints[FE <: FeatureExtractor](asm: ActiveShapeModel[FE], fe: FeatureExtractorInstance, mesh: TriangleMesh, searchPoints: SearchPoints, searchPointSampler: SearchPointSampler, config: FittingConfiguration): IndexedSeq[(Int, Point[_3D])] = {
    val matchingPts = searchPoints.map { sp =>
      (sp.refId, findBestMatchingPointAtPoint(asm, fe, mesh, sp, searchPointSampler, config))
    }

    val matchingPtsWithinDist = matchingPts.filter(_._2.isDefined).map(p => (p._1, p._2.get))
    matchingPtsWithinDist.toIndexedSeq
  }

  /**
   * find the point in the target that is the best match at the given point
   * Retuns Some(Point) if its feature vector is close to a trained feature in terms of the mahalanobis distance, otherwise None
   */
  private[this] def findBestMatchingPointAtPoint[FE <: FeatureExtractor](asm: ActiveShapeModel[FE], fe: FeatureExtractorInstance, mesh: TriangleMesh, searchPoint: SearchPoint, searchPointSampler: SearchPointSampler, config: FittingConfiguration): Option[Point[_3D]] = {
    val samplePts = searchPointSampler(asm, mesh, searchPoint.refId)

    val ptsWithDists = for (imgPt <- samplePts) yield {
      val featureVector = fe.extractFeatures(mesh, imgPt)
      val dist = asm.featureDistance(searchPoint.profileId, featureVector)
      //      if (Debug) {
      //        println(s"$imgPt features $featureVector")
      //        println(s"$imgPt featureDistance $dist")
      //      }
      (imgPt, dist)
    }
    val (minPt, minIntensityDist) = ptsWithDists.minBy {
      case (pt, dist) => dist
    }

    val points = mesh.points.toIndexedSeq

    val shapeDistForPt = asm.shapeModel.marginal(searchPoint.refId).mahalanobisDistance(minPt - searchPoint.refPoint)
    if (minIntensityDist < config.maxIntensityStddev && shapeDistForPt < config.maxShapeStddev) {
      if (Debug) println(s"constraint OK: ${searchPoint.refId} @ ${searchPoint.refPoint} ${points(searchPoint.refId)} -> $minPt (featureDistance $minIntensityDist, shape Distance $shapeDistForPt)")
      Some(minPt)
    } else {
      if (Debug) println(s"constraint XX: ${searchPoint.refId} @ ${searchPoint.refPoint} ${points(searchPoint.refId)} -> $minPt (featureDistance $minIntensityDist, shape Distance $shapeDistForPt)")
      None
    }
  }

  var Debug = false
}
