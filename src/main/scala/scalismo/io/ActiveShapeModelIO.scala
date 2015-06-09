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

import java.io.{ IOException, File }

import breeze.linalg.{ DenseMatrix, DenseVector }
import ncsa.hdf.`object`.Group
import scalismo.common.UnstructuredPointsDomain
import scalismo.geometry._3D
import scalismo.mesh.TriangleMesh
import scalismo.statisticalmodel.MultivariateNormalDistribution
import scalismo.statisticalmodel.asm._

import scala.collection.immutable
import scala.util.{ Failure, Success, Try }

object ActiveShapeModelIO {

  object Names {

    object Group {
      val ActiveShapeModel = "/activeShapeModel"
      val FeatureExtractor = "featureExtractor"
      val ImagePreprocessor = "imagePreprocessor"
      val Profiles = "profiles"
    }

    object Attribute {
      val NumberOfPoints = "numberOfPoints"
      val ProfileLength = "profileLength"
      val Comment = "comment"
      val MajorVersion = "majorVersion"
      val MinorVersion = "minorVersion"
    }

    object Item {
      val PointIds = "pointIds"
      val Means = "means"
      val Covariances = "covariances"
    }

  }

  def writeActiveShapeModel(asm: ActiveShapeModel, file: File): Try[Unit] = {
    for {
      _ <- StatismoIO.writeStatismoMeshModel(asm.statisticalModel, file)
      h5 <- HDF5Utils.openFileForWriting(file)
      asmGroup <- h5.createGroup(Names.Group.ActiveShapeModel)
      feGroup <- h5.createGroup(asmGroup, Names.Group.FeatureExtractor)
      ppGroup <- h5.createGroup(asmGroup, Names.Group.ImagePreprocessor)
      profilesGroup <- h5.createGroup(asmGroup, Names.Group.Profiles)
      // for now, the version information is fixed to 1.0
      _ <- h5.writeIntAttribute(asmGroup.getFullName, Names.Attribute.MajorVersion, 1)
      _ <- h5.writeIntAttribute(asmGroup.getFullName, Names.Attribute.MinorVersion, 0)
      _ <- ImagePreprocessorIOHandlers.save(asm.preprocessor, h5, ppGroup)
      _ <- FeatureExtractorIOHandlers.save(asm.featureExtractor, h5, feGroup)
      _ <- writeProfilesAndPointIds(h5, profilesGroup, asm.profiles, asm.pointIds)
    } yield ()
  }

  private def writeProfilesAndPointIds(h5file: HDF5File, group: Group, profiles: Profiles, pointIds: IndexedSeq[Int]): Try[Unit] = Try {
    val numberOfPoints = profiles.domain.numberOfPoints
    val profileLength = if (numberOfPoints > 0) profiles.data.head.mean.size else 0
    val means = new NDArray(Array[Long](numberOfPoints, profileLength), profiles.data.flatMap(_.mean.toArray).toArray)
    val covariances = new NDArray(Array[Long](numberOfPoints * profileLength, profileLength), profiles.data.flatMap(_.cov.toArray).toArray)
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
    for {
      shapeModel <- StatismoIO.readStatismoMeshModel(fn)
      h5file <- HDF5Utils.openFileForReading(fn)
      asmGroup <- h5file.getGroup(Names.Group.ActiveShapeModel)
      asmVersionMajor <- h5file.readIntAttribute(asmGroup.getFullName, Names.Attribute.MajorVersion)
      asmVersionMinor <- h5file.readIntAttribute(asmGroup.getFullName, Names.Attribute.MinorVersion)
      _ <- {
        (asmVersionMajor, asmVersionMinor) match {
          case (1, 0) => Success(())
          case _ => Failure(new IOException(s"Unsupported ActiveShapeModel version: $asmVersionMajor.$asmVersionMinor"))
        }
      }
      feGroup <- h5file.getGroup(asmGroup, Names.Group.FeatureExtractor)
      ppGroup <- h5file.getGroup(asmGroup, Names.Group.ImagePreprocessor)
      preprocessor <- ImagePreprocessorIOHandlers.load(h5file, ppGroup)
      profilesGroup <- h5file.getGroup(asmGroup, Names.Group.Profiles)
      featureExtractor <- FeatureExtractorIOHandlers.load(h5file, feGroup)
      profilesAndPointIds <- readProfilesAndPointIds(h5file, profilesGroup, shapeModel.referenceMesh)
    } yield ActiveShapeModel(shapeModel, profilesAndPointIds._1, preprocessor, featureExtractor, profilesAndPointIds._2)
  }

  private[this] def readProfilesAndPointIds(h5file: HDF5File, group: Group, referenceMesh: TriangleMesh): Try[(Profiles, immutable.IndexedSeq[Int])] = {
    val groupName = group.getFullName
    for {
      profileLength <- h5file.readIntAttribute(groupName, Names.Attribute.ProfileLength)
      pointIds <- h5file.readArray[Int](s"$groupName/${Names.Item.PointIds}")
      pts = pointIds.map(id => referenceMesh.point(id)).toIndexedSeq
      covArray <- h5file.readNDArray[Float](s"$groupName/${Names.Item.Covariances}")
      (_, n) = (covArray.dims.head.toInt, covArray.dims(1).toInt)
      covMats = covArray.data.grouped(n * n).map(data => DenseMatrix.create(n, n, data))
      meanArray <- h5file.readNDArray[Float](s"$groupName/${Names.Item.Means}")
      meanVecs = meanArray.data.grouped(n).map(data => DenseVector(data))
    } yield {
      val dists = meanVecs.zip(covMats).map { case (m, c) => new MultivariateNormalDistribution(m, c) }.to[immutable.IndexedSeq]
      (Profiles(UnstructuredPointsDomain[_3D](pts), dists), pointIds.toIndexedSeq)
    }

  }
}
