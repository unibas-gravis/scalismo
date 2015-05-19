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

import java.io.File

import breeze.linalg.{ DenseMatrix, DenseVector }
import ncsa.hdf.`object`.Group
import scalismo.common.SpatiallyIndexedDiscreteDomain
import scalismo.geometry._3D
import scalismo.mesh.TriangleMesh
import scalismo.statisticalmodel.MultivariateNormalDistribution
import scalismo.statisticalmodel.asm.{ ActiveShapeModel, FeatureExtractorSerializer, Profiles }

import scala.collection.immutable
import scala.util.Try

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
      val PointIds = "pointIds"
      val Means = "means"
      val Covariances = "covariances"
    }

  }

  def writeActiveShapeModel(asm: ActiveShapeModel, file: File): Try[Unit] = {
    for {
      feWriter <- FeatureExtractorSerializer.get(asm.featureExtractor.identifier)
      _ <- StatismoIO.writeStatismoMeshModel(asm.statisticalModel, file)
      h5 <- HDF5Utils.openFileForWriting(file)
      asmGroup <- h5.createGroup(Names.Group.ActiveShapeModel)
      feGroup <- h5.createGroup(asmGroup, Names.Group.FeatureExtractor)
      profilesGroup <- h5.createGroup(asmGroup, Names.Group.Profiles)
      _ <- writeProfiles(h5, profilesGroup, asm.profiles, asm.pointIds)
      _ <- feWriter.saveHdf5(asm.featureExtractor, h5, feGroup)

    } yield ()
  }

  private def writeProfiles(h5file: HDF5File, group: Group, profiles: Profiles, pointIds: IndexedSeq[Int]): Try[Unit] = Try {
    val numberOfPoints = profiles.domain.numberOfPoints
    val profileLength = if (numberOfPoints > 0) profiles.data.head.mean.size else 0
    val means = new NDArray(Array[Long](numberOfPoints, profileLength), profiles.data.map(_.mean.toArray).flatten.toArray)
    val covariances = new NDArray(Array[Long](numberOfPoints * profileLength, profileLength), profiles.data.map(_.cov.toArray).flatten.toArray)
    val groupName = group.getFullName

    val result = for {
      _ <- h5file.writeIntAttribute(groupName, Names.Attribute.NumberOfPoints, numberOfPoints)
      _ <- h5file.writeIntAttribute(groupName, Names.Attribute.ProfileLength, profileLength)
      _ <- h5file.writeStringAttribute(groupName, Names.Attribute.Comment, s"${Names.Item.Covariances} consists of $numberOfPoints concatenated ${profileLength}x$profileLength matrices")
      _ <- h5file.writeArray(s"$groupName/${Names.Item.PointIds}", pointIds.toArray)
      _ <- h5file.writeNDArray(s"$groupName/${Names.Item.Means}", means)
      _ <- h5file.writeNDArray(s"$groupName/${Names.Item.Covariances}", covariances)
    } yield ()
    result // this is a Try[Unit], so the return value is a Try[Try[Unit]]
  }.flatten

  def readActiveShapeModel(fn: File): Try[ActiveShapeModel] = {
    //val feReader = implicitly[HDF5Read[FE]]

    for {
      shapeModel <- StatismoIO.readStatismoMeshModel(fn)
      h5file <- HDF5Utils.openFileForReading(fn)
      asmGroup <- h5file.getGroup(Names.Group.ActiveShapeModel)
      feGroup <- h5file.getGroup(asmGroup, Names.Group.FeatureExtractor)
      profilesGroup <- h5file.getGroup(asmGroup, Names.Group.Profiles)
      feType <- h5file.readStringAttribute(feGroup.getFullName, FeatureExtractorSerializer.IdentifierAttributeName)
      feSerializer <- FeatureExtractorSerializer.get(feType)
      featureExtractor <- feSerializer.loadHdf5(h5file, feGroup)
      (profileDistributions, pointIds) <- readProfiles(h5file, profilesGroup, shapeModel.referenceMesh)
    } yield ActiveShapeModel(shapeModel, profileDistributions, featureExtractor, pointIds)
  }

  private[this] def readProfiles(h5file: HDF5File, group: Group, referenceMesh: TriangleMesh): Try[(Profiles, immutable.IndexedSeq[Int])] = {
    val groupName = group.getFullName
    for {
      profileLength <- h5file.readIntAttribute(groupName, Names.Attribute.ProfileLength)
      pointIds <- h5file.readArray[Int](s"$groupName/${Names.Item.PointIds}")
      pts = pointIds.map(id => referenceMesh.points(id)).toIndexedSeq
      covArray <- h5file.readNDArray[Float](s"$groupName/${Names.Item.Covariances}")
      (m, n) = (covArray.dims(0).toInt, covArray.dims(1).toInt)
      covMats = covArray.data.grouped(n * n).map(data => DenseMatrix.create(n, n, data))
      meanArray <- h5file.readNDArray[Float](s"$groupName/${Names.Item.Means}")
      meanVecs = meanArray.data.grouped(n).map(data => DenseVector(data))
    } yield {
      val dists = meanVecs.zip(covMats).map { case (m, c) => new MultivariateNormalDistribution(m, c) }.to[immutable.IndexedSeq]
      (Profiles(SpatiallyIndexedDiscreteDomain.fromSeq[_3D](pts), dists), pointIds.toIndexedSeq)
    }

  }
}
