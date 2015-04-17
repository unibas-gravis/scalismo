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
package scalismo.io

import scalismo.common.SpatiallyIndexedDiscreteDomain
import scalismo.geometry.{ _3D, Point }
import scalismo.statisticalmodel.ActiveShapeModel.ProfileDistributions
import scalismo.statisticalmodel.{ MultivariateNormalDistribution, ActiveShapeModel }

import scala.util.{ Success, Try }
import java.io.File
import ncsa.hdf.`object`.Group
import breeze.linalg.{ DenseMatrix, DenseVector }

object ActiveShapeModelIO {

  object Names {
    object Group {
      val ActiveShapeModel = "/activeShapeModel"
      val FeatureExtractor = "featureExtractor"
      val Profiles = "profiles"
    }
    object Attribute {
      val NumberOfPoints = "numberOfPoints"
      val ProfileLength = "profileLength"
      val Comment = "comment"
    }
    object Item {
      val Points = "points"
      val Means = "means"
      val Covariances = "covariances"
    }
  }

  def writeActiveShapeModel[FE <: ActiveShapeModel.FeatureExtractor: HDF5Write](asm: ActiveShapeModel[FE], file: File): Try[Unit] = {
    val feWriter = implicitly[HDF5Write[FE]]
    for {
      _ <- StatismoIO.writeStatismoMeshModel(asm.shapeModel, file)
      h5 <- HDF5Utils.openFileForWriting(file)
      asmGroup <- h5.createGroup(Names.Group.ActiveShapeModel)
      feGroup <- h5.createGroup(asmGroup, Names.Group.FeatureExtractor)
      profilesGroup <- h5.createGroup(asmGroup, Names.Group.Profiles)
      _ <- writeProfiles(asm.profileDistributions, h5, profilesGroup)
      _ <- feWriter.write(asm.featureExtractor, h5, feGroup)

    } yield ()
  }

  private def writeProfiles(distributions: ProfileDistributions, h5file: HDF5File, group: Group): Try[Unit] = {
    val numberOfPoints = distributions.domain.numberOfPoints
    val profileLength = if (numberOfPoints > 0) distributions.data.head.mean.size else 0
    val points = new NDArray(Array[Long](numberOfPoints, 3), distributions.domain.points.toIndexedSeq.flatten(_.data).toArray)
    val means = new NDArray(Array[Long](numberOfPoints, profileLength), distributions.data.map(_.mean.toArray).flatten.toArray)
    val covariances = new NDArray(Array[Long](numberOfPoints * profileLength, profileLength), distributions.data.map(_.cov.toArray).flatten.toArray)
    val groupName = group.getFullName

    for {
      _ <- h5file.writeIntAttribute(groupName, Names.Attribute.NumberOfPoints, numberOfPoints)
      _ <- h5file.writeIntAttribute(groupName, Names.Attribute.ProfileLength, profileLength)
      _ <- h5file.writeStringAttribute(groupName, Names.Attribute.Comment, s"${Names.Item.Covariances} consists of $numberOfPoints concatenated ${profileLength}x$profileLength matrices")
      _ <- h5file.writeNDArray(s"$groupName/${Names.Item.Points}", points)
      _ <- h5file.writeNDArray(s"$groupName/${Names.Item.Means}", means)
      _ <- h5file.writeNDArray(s"$groupName/${Names.Item.Covariances}", covariances)
    } yield ()
  }

  def readActiveShapeModel[FE <: ActiveShapeModel.FeatureExtractor: HDF5Read](fn: File): Try[ActiveShapeModel[FE]] = {
    val feReader = implicitly[HDF5Read[FE]]

    for {
      shapeModel <- StatismoIO.readStatismoMeshModel(fn)
      h5file <- HDF5Utils.openFileForReading(fn)
      asmGroup <- h5file.getGroup(Names.Group.ActiveShapeModel)
      feGroup <- h5file.getGroup(asmGroup, Names.Group.FeatureExtractor)
      profilesGroup <- h5file.getGroup(asmGroup, Names.Group.Profiles)
      featureExtractor <- feReader.read(h5file, feGroup)
      profileDistributions <- readProfiles(h5file, profilesGroup)
    } yield new ActiveShapeModel(shapeModel, profileDistributions, featureExtractor)
  }

  private[this] def readProfiles(h5file: HDF5File, group: Group): Try[ProfileDistributions] = {
    val groupName = group.getFullName
    val ptDim = 3
    for {
      profileLength <- h5file.readIntAttribute(groupName, Names.Attribute.ProfileLength)
      profilePtsArray <- h5file.readNDArray[Float](s"$groupName/${Names.Item.Points}")
      pts = profilePtsArray.data.grouped(ptDim).map(data => Point(data(0), data(1), data(2))).toIndexedSeq
      covArray <- h5file.readNDArray[Float](s"$groupName/${Names.Item.Covariances}")
      (m, n) = (covArray.dims(0).toInt, covArray.dims(1).toInt)
      covMats = covArray.data.grouped(n * n).map(data => DenseMatrix.create(n, n, data))
      meanArray <- h5file.readNDArray[Float](s"$groupName/${Names.Item.Means}")
      meanVecs = meanArray.data.grouped(n).map(data => DenseVector(data))
    } yield {
      val dists = meanVecs.zip(covMats).map { case (m, c) => new MultivariateNormalDistribution(m, c) }.toArray
      ProfileDistributions(SpatiallyIndexedDiscreteDomain.fromSeq[_3D](pts), dists)
    }

  }

  def readASMOld[FE <: ActiveShapeModel.FeatureExtractor: HDF5Read](fn: File): Try[ActiveShapeModel[FE]] = {
    val featureExtractorReader = implicitly[HDF5Read[FE]]

    for {
      shapeModel <- StatismoIO.readStatismoMeshModel(fn)
      h5file <- HDF5Utils.openFileForReading(fn)
      asmGroup <- h5file.getGroup("/ASMModel")
      profileDistributions <- readProfileDistributionsOld(h5file, asmGroup)
      featureExtractor <- featureExtractorReader.read(h5file, asmGroup)
    } yield new ActiveShapeModel(shapeModel, profileDistributions, featureExtractor)
  }

  private[this] def readProfileDistributionsOld(h5file: HDF5File, group: Group): Try[ProfileDistributions] = {
    val groupName = group.getName
    val ptDim = 3
    for {
      profileDim <- h5file.readInt(s"$groupName/profileDimension")
      profilePtsArray <- h5file.readArray[Float](s"$groupName/profilePoints")
      pts = profilePtsArray.grouped(ptDim).map(data => Point(data(0), data(1), data(2))).toIndexedSeq
      covArray <- h5file.readNDArray[Float](s"$groupName/cov")
      (m, n) = (covArray.dims(0).toInt, covArray.dims(1).toInt)
      covMats = covArray.data.grouped(n * n).map(data => DenseMatrix.create(n, n, data))
      meanArray <- h5file.readArray[Float](s"$groupName/mean")
      meanVecs = meanArray.grouped(n).map(data => DenseVector(data))
    } yield {
      val dists = meanVecs.zip(covMats).map { case (m, c) => new MultivariateNormalDistribution(m, c) }.toArray
      ProfileDistributions(SpatiallyIndexedDiscreteDomain.fromSeq[_3D](pts), dists)
    }

  }

  def writeASMOld[FE <: ActiveShapeModel.FeatureExtractor: HDF5Write](asm: ActiveShapeModel[FE], fn: File): Try[Unit] = {
    val featureExtractorWriter = implicitly[HDF5Write[FE]]
    for {
      statismoStatus <- StatismoIO.writeStatismoMeshModel(asm.shapeModel, fn)
      h5file <- HDF5Utils.openFileForWriting(fn)
      asmGroup <- h5file.createGroup("/ASMModel")
      _ <- writeProfileDistributionsOld(h5file, asmGroup, asm.profileDistributions)
      _ <- featureExtractorWriter.write(asm.featureExtractor, h5file, asmGroup)
    } yield Success(())
  }

  private[this] def writeProfileDistributionsOld(h5file: HDF5File, group: Group, distributions: ProfileDistributions): Try[Unit] = {
    val numEntries = distributions.domain.numberOfPoints
    val distDim = if (numEntries > 0) distributions.data(0).mean.size else 0
    val ptArray = distributions.domain.points.toIndexedSeq.flatten(_.data).toArray
    val meanArray = distributions.data.map(_.mean.data).flatten.toArray
    val covArray = distributions.data.map(_.cov.data).flatten.toArray
    val groupName = group.getFullName
    for {
      _ <- h5file.writeInt(s"$groupName/numberOfProfilePoints", numEntries)
      _ <- h5file.writeInt(s"$groupName/profileDimension", distDim)
      _ <- h5file.writeArray(s"$groupName/profilePoints", ptArray)
      _ <- h5file.writeArray(s"$groupName/mean", meanArray)
      covNDArray = new NDArray(Array[Long](numEntries * distDim, distDim), covArray)
      _ <- h5file.writeNDArray(s"$groupName/cov", covNDArray)
    } yield ()
  }

}
