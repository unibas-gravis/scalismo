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

import java.io.{File, IOException}

import breeze.linalg.{DenseMatrix, DenseVector}
import ncsa.hdf.`object`.Group
import scalismo.common.PointId
import scalismo.geometry._3D
import scalismo.mesh.TriangleMesh
import scalismo.statisticalmodel.{MultivariateNormalDistribution, PointDistributionModel, StatisticalMeshModel}
import scalismo.statisticalmodel.asm._

import scala.collection.immutable
import scala.util.{Failure, Success, Try}

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

    val pointModel = PointDistributionModel[_3D, TriangleMesh](asm.statisticalModel.gp)
    for {
      _ <- StatismoIO.writeStatismoPDM(pointModel, file)
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
      _ <- writeProfiles(h5, profilesGroup, asm.profiles)
    } yield ()
  }

  private def writeProfiles(h5file: HDF5Writer, group: Group, profiles: Profiles): Try[Unit] =
    Try {
      val numberOfPoints = profiles.data.length
      val profileLength = if (numberOfPoints > 0) profiles.data.head.distribution.mean.size else 0
      val means: NDArray[Float] = new NDArray(IndexedSeq[Long](numberOfPoints, profileLength),
                                              profiles.data.flatMap(_.distribution.mean.toArray).toArray.map(_.toFloat))
      val covariances: NDArray[Float] =
        new NDArray(IndexedSeq[Long](numberOfPoints * profileLength, profileLength),
                    profiles.data.flatMap(_.distribution.cov.toArray).toArray.map(_.toFloat))
      val groupName = group.getFullName

      val result = for {
        _ <- h5file.writeIntAttribute(groupName, Names.Attribute.NumberOfPoints, numberOfPoints)
        _ <- h5file.writeIntAttribute(groupName, Names.Attribute.ProfileLength, profileLength)
        _ <- h5file.writeStringAttribute(
          groupName,
          Names.Attribute.Comment,
          s"${Names.Item.Covariances} consists of $numberOfPoints concatenated ${profileLength}x$profileLength matrices"
        )
        _ <- h5file.writeArray(s"$groupName/${Names.Item.PointIds}", profiles.data.map(_.pointId.id).toArray)
        _ <- h5file.writeNDArray[Float](s"$groupName/${Names.Item.Means}", means)
        _ <- h5file.writeNDArray[Float](s"$groupName/${Names.Item.Covariances}", covariances)
      } yield ()
      result // this is a Try[Unit], so the return value is a Try[Try[Unit]]
    }.flatten

  def readActiveShapeModel(fn: File): Try[ActiveShapeModel] = {
    for {
      pdm <- StatismoIO.readStatismoPDM[_3D, TriangleMesh](fn)
      h5file <- HDF5Utils.openFileForReading(fn)
      asmGroup <- h5file.getGroup(Names.Group.ActiveShapeModel)
      asmVersionMajor <- h5file.readIntAttribute(asmGroup.getPath, Names.Attribute.MajorVersion)
      asmVersionMinor <- h5file.readIntAttribute(asmGroup.getPath, Names.Attribute.MinorVersion)
      _ <- {
        (asmVersionMajor, asmVersionMinor) match {
          case (1, 0) => Success(())
          case _      => Failure(new IOException(s"Unsupported ActiveShapeModel version: $asmVersionMajor.$asmVersionMinor"))
        }
      }
      feGroup <- h5file.getGroup(asmGroup, Names.Group.FeatureExtractor)
      ppGroup <- h5file.getGroup(asmGroup, Names.Group.ImagePreprocessor)
      preprocessor <- ImagePreprocessorIOHandlers.load(h5file, ppGroup)
      profilesGroup <- h5file.getGroup(asmGroup, Names.Group.Profiles)
      featureExtractor <- FeatureExtractorIOHandlers.load(h5file, feGroup)
      profiles <- readProfiles(h5file, profilesGroup, pdm.reference)
    } yield {
      val shapeModel = PointDistributionModel(pdm.gp)
      ActiveShapeModel(shapeModel, profiles, preprocessor, featureExtractor)
    }

  }

  private[this] def readProfiles(h5file: HDF5Reader, group: io.jhdf.api.Group, referenceMesh: TriangleMesh[_3D]): Try[Profiles] = {
    val groupName = group.getPath
    for {
      profileLength <- h5file.readIntAttribute(groupName, Names.Attribute.ProfileLength)
      pointIds <- h5file.readArray[Int](s"$groupName/${Names.Item.PointIds}")
      pts = pointIds.map(id => referenceMesh.pointSet.point(PointId(id))).toIndexedSeq
      covArray <- h5file.readNDArray[Float](s"$groupName/${Names.Item.Covariances}")
      (_, n) = (covArray.dims.head.toInt, covArray.dims(1).toInt)
      covMats = covArray.data.grouped(n * n).map(data => DenseMatrix.create(n, n, data))
      meanArray <- h5file.readNDArray[Float](s"$groupName/${Names.Item.Means}")
      meanVecs = meanArray.data.grouped(n).map(data => DenseVector(data))
    } yield {
      val dists = meanVecs
        .zip(covMats)
        .map { case (m, c) => new MultivariateNormalDistribution(m.map(_.toDouble), c.map(_.toDouble)) }
        .toIndexedSeq
      val profiles = dists.zip(pointIds).map { case (d, id) => Profile(PointId(id), d) }
      new Profiles(profiles)
    }
  }
}
