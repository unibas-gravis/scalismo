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
package scalismo.statisticalmodel.dataset

import java.io.File

import scalismo.geometry._
import scalismo.io.MeshIO
import scalismo.mesh._
import scalismo.mesh.kdtree.MeshVolumeMetrics
import scalismo.registration.{LandmarkRegistration, Transformation}
import scalismo.tetramesh.{TetrahedralMesh, TetrahedralMesh3D}
import scalismo.utils.Random

import scala.annotation.tailrec

private[dataset] case class CrossvalidationFold(trainingData: DataCollection, testingData: DataCollection)

private[dataset] case class CrossvalidationFoldMeshVolume(trainingData: DataCollectionOfMeshVolume, testingData: DataCollectionOfMeshVolume)

/**
 * A registered item in a dataset.
 *
 *  @param info A human-readable description of the processing the data item went through. Current implemented methods on data collections,
 *  such as [[DataCollection.gpa]] will increment this description
 *  @param transformation Transformation to apply to obtain the data item from the reference of the reference item of the dataset.
 *  This would typically be the transformation resulting from registering a reference mesh to the mesh represented by this data item.
 */
case class DataItem[D](info: String, transformation: Transformation[D])

/**
 * Data-structure for handling a dataset of registered 3D meshes. All pre-implemented operations such as building a
 * PCA model or performing a Generalized Procrustes Analysis require a DataCollection as input
 *
 * @param reference The reference mesh of the dataset. This is the mesh that was registered to all other items of the dataset.
 * @param dataItems Sequence of data items containing the required transformations to apply to the reference mesh in order to obtain
 * other elements of the dataset.
 */
case class DataCollection(reference: TriangleMesh[_3D], dataItems: Seq[DataItem[_3D]])(implicit random: Random) {

  val size: Int = dataItems.size

  private[dataset] def createCrossValidationFolds(nFolds: Int): Seq[CrossvalidationFold] = {

    val shuffledDataItems = random.scalaRandom.shuffle(dataItems)
    val foldSize = shuffledDataItems.size / nFolds
    val dataGroups = shuffledDataItems.grouped(foldSize).toSeq

    val folds = for (currFold <- 0 until nFolds) yield {
      val testingDataItems = dataGroups(currFold)
      val testingCollection = DataCollection(reference, testingDataItems)
      val trainingDataItems = (dataGroups.slice(0, currFold).flatten ++: dataGroups.slice(currFold + 1, dataGroups.size).flatten)
      val trainingCollection = DataCollection(reference, trainingDataItems)

      CrossvalidationFold(trainingCollection, testingCollection)
    }
    folds
  }

  private[dataset] def createLeaveOneOutFolds = createCrossValidationFolds(dataItems.size)

  /**
   * Returns a new DataCollection where the given function was applied to all data items
   */
  def mapItems(f: DataItem[_3D] => DataItem[_3D]): DataCollection = {
    new DataCollection(reference, dataItems.map(f))
  }

  /**
   * Returns the mean surface computed by transforming the reference with all the transformations in the datacollection
   */
  def meanSurface: TriangleMesh[_3D] = {
    val t = reference.transform(meanTransformation)
    t
  }

  /**
   * Returns the mean transformation from all the transformation in the datacollection
   */
  val meanTransformation: Transformation[_3D] = {

    Transformation {

      (pt: Point[_3D]) =>
        {
          var meanPoint = EuclideanVector3D(0, 0, 0)
          var i = 0
          while (i < dataItems.size) {
            meanPoint += dataItems(i).transformation(pt).toVector
            i += 1
          }
          (meanPoint / dataItems.size).toPoint

        }
    }
  }
}


/**
  * Data-structure for handling a dataset of registered 3D meshes. All pre-implemented operations such as building a
  * PCA model or performing a Generalized Procrustes Analysis require a DataCollection as input
  *
  * @param reference The reference mesh of the dataset. This is the mesh that was registered to all other items of the dataset.
  * @param dataItems Sequence of data items containing the required transformations to apply to the reference mesh in order to obtain
  * other elements of the dataset.
  */
case class DataCollectionOfMeshVolume(reference: TetrahedralMesh[_3D], dataItems: Seq[DataItem[_3D]])(implicit random: Random) {

  val size: Int = dataItems.size

  private[dataset] def createCrossValidationFolds(nFolds: Int): Seq[CrossvalidationFoldMeshVolume] = {

    val shuffledDataItems = random.scalaRandom.shuffle(dataItems)
    val foldSize = shuffledDataItems.size / nFolds
    val dataGroups = shuffledDataItems.grouped(foldSize).toSeq

    val folds = for (currFold <- 0 until nFolds) yield {
      val testingDataItems = dataGroups(currFold)
      val testingCollection = DataCollectionOfMeshVolume(reference, testingDataItems)
      val trainingDataItems = (dataGroups.slice(0, currFold).flatten ++: dataGroups.slice(currFold + 1, dataGroups.size).flatten)
      val trainingCollection = DataCollectionOfMeshVolume(reference, trainingDataItems)

      CrossvalidationFoldMeshVolume(trainingCollection, testingCollection)
    }
    folds
  }

  private[dataset] def createLeaveOneOutFolds = createCrossValidationFolds(dataItems.size)

  /**
    * Returns a new DataCollectionofMeshvolume where the given function was applied to all data items
    */
  def mapItems(f: DataItem[_3D] => DataItem[_3D]): DataCollectionOfMeshVolume = {
    new DataCollectionOfMeshVolume(reference, dataItems.map(f))
  }

  /**
    * Returns the mean surface computed by transforming the reference with all the transformations in the datacollection
    */
  def meanSurface: TetrahedralMesh[_3D] = {
    val t = reference.transform(meanTransformation)
    t
  }

  /**
    * Returns the mean transformation from all the transformation in the datacollection
    */
  val meanTransformation: Transformation[_3D] = {

    Transformation {

      (pt: Point[_3D]) =>
      {
        var meanPoint = EuclideanVector3D(0, 0, 0)
        var i = 0
        while (i < dataItems.size) {
          meanPoint += dataItems(i).transformation(pt).toVector
          i += 1
        }
        (meanPoint / dataItems.size).toPoint

      }
    }
  }
}



/**
 * Implements utility functions on [[DataCollection]] instances
 */
object DataCollection {

  /**
    * Builds a [[DataCollection]] instance from a reference mesh and a sequence of meshes in correspondence.
    * Returns a data collection containing the valid elements as well as the list of errors for invalid items.
    */
  def fromMeshSequence(referenceMesh: TriangleMesh[_3D], registeredMeshes: Seq[TriangleMesh[_3D]])(implicit rng: Random): (Option[DataCollection], Seq[Throwable]) = {
    val (transformations, errors) = DataUtils.partitionSuccAndFailedTries(registeredMeshes.map(DataUtils.meshToTransformation(referenceMesh, _)))
    val dc = DataCollection(referenceMesh, transformations.map(DataItem("from mesh", _)))
    if (dc.size > 0) (Some(dc), errors) else (None, errors)
  }



  /**
    * Builds a [[DataCollection]] instance from a reference mesh and a directory containing meshes in correspondence with the reference.
    * Only vtk and stl meshes are currently supported.
    *
    * @return a data collection containing the valid elements as well as the list of errors for invalid items.
    */
  def fromMeshDirectory(referenceMesh: TriangleMesh[_3D], meshDirectory: File)(implicit rng: Random): (Option[DataCollection], Seq[Throwable]) = {
    val meshFileNames = meshDirectory.listFiles().toSeq.filter(fn => fn.getAbsolutePath.endsWith(".vtk") || fn.getAbsolutePath.endsWith(".stl"))
    val (meshes, ioErrors) = DataUtils.partitionSuccAndFailedTries(for (meshFn <- meshFileNames) yield {
      MeshIO.readMesh(meshFn).map(m => TriangleMesh3D(m.pointSet, referenceMesh.triangulation))
    })
    val (dc, meshErrors) = fromMeshSequence(referenceMesh, meshes)
    (dc, ioErrors ++ meshErrors)
  }




  /**
    * Performs a Generalized Procrustes Analysis on the data collection.
    * This is done by repeatedly computing the mean of all meshes in the dataset and
    * aligning all items rigidly to the mean.
    *
    * The reference mesh is unchanged, only the transformations in the collection are adapted
    */
  def gpa(dc: DataCollection, maxIteration: Int = 3, haltDistance: Double = 1e-5)(implicit rng: Random): DataCollection = {
    gpaComputation(dc, dc.meanSurface, maxIteration, haltDistance)
  }

  @tailrec
  private def gpaComputation(dc: DataCollection, meanShape: TriangleMesh[_3D], maxIteration: Int, haltDistance: Double)(implicit rng: Random): DataCollection = {

    if (maxIteration == 0) return dc

    val referencePoints = dc.reference.pointSet.points.toIndexedSeq
    val numberOfPoints = referencePoints.size
    val referenceCenterOfMass = referencePoints.foldLeft(Point3D(0, 0, 0))((acc, pt) => acc + (pt.toVector / numberOfPoints))

    val meanShapePoints = meanShape.pointSet.points.toIndexedSeq

    // align all shape to it and create a transformation from the mean to the aligned shape
    val dataItemsWithAlignedTransform = dc.dataItems.par.map { dataItem =>
      val surface = dc.reference.transform(dataItem.transformation)
      val transform = LandmarkRegistration.rigid3DLandmarkRegistration(surface.pointSet.points.toIndexedSeq.zip(meanShapePoints), referenceCenterOfMass)

      DataItem("gpa -> " + dataItem.info, Transformation(transform.compose(dataItem.transformation)))
    }

    val newdc = DataCollection(dc.reference, dataItemsWithAlignedTransform.toIndexedSeq)
    val newMean = newdc.meanSurface

    if (MeshMetrics.procrustesDistance(meanShape, newMean) < haltDistance) {
      newdc
    } else {
      gpaComputation(newdc, newMean, maxIteration - 1, haltDistance)
    }
  }



}

  object DataCollectionOfMeshVolume{

    /**
      * Builds a [[DataCollection]] instance from a reference mesh volume and a sequence of meshes in correspondence.
      * Returns a data collection containing the valid elements as well as the list of errors for invalid items.
      */
    def fromMeshSequence(referenceMesh: TetrahedralMesh[_3D], registeredMeshes: Seq[TetrahedralMesh[_3D]])(implicit rng: Random): (Option[DataCollectionOfMeshVolume], Seq[Throwable]) = {
      val (transformations, errors) = DataUtils.partitionSuccAndFailedTries(registeredMeshes.map(DataUtils.meshVolumeToTransformation(referenceMesh, _)))
      val dc = DataCollectionOfMeshVolume(referenceMesh, transformations.map(DataItem("from mesh", _)))
      if (dc.size > 0) (Some(dc), errors) else (None, errors)
    }


    /**
      * Builds a [[DataCollection]] instance from a reference mesh volume and a directory containing meshe volumes in correspondence with the reference.
      * Only vtk and stl meshes are currently supported.
      *
      * @return a data collection containing the valid elements as well as the list of errors for invalid items.
      */
    def fromMeshDirectory(referenceMesh: TetrahedralMesh[_3D], meshDirectory: File)(implicit rng: Random): (Option[DataCollectionOfMeshVolume], Seq[Throwable]) = {
      val meshFileNames = meshDirectory.listFiles().toSeq.filter(fn => fn.getAbsolutePath.endsWith(".vtk") || fn.getAbsolutePath.endsWith(".stl"))
      val (meshes, ioErrors) = DataUtils.partitionSuccAndFailedTries(for (meshFn <- meshFileNames) yield {
        MeshIO.readMesh(meshFn).map(m => TetrahedralMesh3D(m.pointSet, referenceMesh.tetrahedralization))
      })
      val (dc, meshErrors) = fromMeshSequence(referenceMesh, meshes)
      (dc, ioErrors ++ meshErrors)
    }




    /**
      * Performs a Generalized Procrustes Analysis on the data collection.
      * This is done by repeatedly computing the mean of all meshes in the dataset and
      * aligning all items rigidly to the mean.
      *
      * The reference mesh is unchanged, only the transformations in the collection are adapted
      */
    def gpa(dc: DataCollectionOfMeshVolume, maxIteration: Int = 3, haltDistance: Double = 1e-5)(implicit rng: Random): DataCollectionOfMeshVolume = {
      gpaComputation(dc, dc.meanSurface, maxIteration, haltDistance)
    }


    @tailrec
    private def gpaComputation(dc: DataCollectionOfMeshVolume, meanShape: TetrahedralMesh[_3D], maxIteration: Int, haltDistance: Double)(implicit rng: Random): DataCollectionOfMeshVolume = {

      if (maxIteration == 0) return dc

      val referencePoints = dc.reference.pointSet.points.toIndexedSeq
      val numberOfPoints = referencePoints.size
      val referenceCenterOfMass = referencePoints.foldLeft(Point3D(0, 0, 0))((acc, pt) => acc + (pt.toVector / numberOfPoints))

      val meanShapePoints = meanShape.pointSet.points.toIndexedSeq

      // align all shape to it and create a transformation from the mean to the aligned shape
      val dataItemsWithAlignedTransform = dc.dataItems.par.map { dataItem =>
        val surface = dc.reference.transform(dataItem.transformation)
        val transform = LandmarkRegistration.rigid3DLandmarkRegistration(surface.pointSet.points.toIndexedSeq.zip(meanShapePoints), referenceCenterOfMass)

        DataItem("gpa -> " + dataItem.info, Transformation(transform.compose(dataItem.transformation)))
      }

      val newdc = DataCollectionOfMeshVolume(dc.reference, dataItemsWithAlignedTransform.toIndexedSeq)
      val newMean = newdc.meanSurface

      if (MeshVolumeMetrics.procrustesDistance(meanShape, newMean) < haltDistance) {
        newdc
      } else {
        gpaComputation(newdc, newMean, maxIteration - 1, haltDistance)
      }
    }

  }






