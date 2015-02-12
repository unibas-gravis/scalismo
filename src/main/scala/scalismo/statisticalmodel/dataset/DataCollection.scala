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

import scalismo.common.RealSpace
import scalismo.geometry.{Point, _3D, Dim}
import scalismo.io.MeshIO
import scalismo.mesh.{MeshMetrics, TriangleMesh}
import scalismo.registration.{LandmarkRegistration, Transformation}

import scala.annotation.tailrec
import scala.util.Random

private [dataset] case class CrossvalidationFold(val trainingData: DataCollection, val testingData: DataCollection)

/** A registered item in a dataset. 
 *  
 *  @param info A human-readable description of the processing the data item went through. Current implemented methods on data collections,
 *  such as [[DataCollection.gpa]] will increment this description
 *  
 *  @param transformation Transformation to apply to obtain the data item from the reference of the reference item of the dataset.
 *  This would typically be the transformation resulting from registering a reference mesh to the mesh represented by this data item. 
 * */
case class DataItem[D <: Dim](val info: String, val transformation: Transformation[D])


/**
 * Data-structure for handling a dataset of registered 3D meshes. All pre-implemeted operations such as building a 
 * PCA model or performing a Generalized Procrustes Analysis require a DataColection as input
 *    
 * @param reference The reference mesh of the dataset. This is the mesh that was registered to all other items of the dataset.
 * @param dataItems Sequence of data items containing the required transformations to apply to the reference mesh in order to obtain
 * other elements of the dataset.
 */
case class DataCollection(reference: TriangleMesh, dataItems: Seq[DataItem[_3D]]) {

  val size: Int = dataItems.size

  private [dataset] def createCrossValidationFolds(nFolds: Int): Seq[CrossvalidationFold] = {

    val shuffledDataItems = Random.shuffle(dataItems)
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

  private [dataset] def createLeaveOneOutFolds = createCrossValidationFolds(dataItems.size)

  /**
   * Returns a new DataCollection where the given function was applied to all data items
   * */
  def mapItems(f: DataItem[_3D] => DataItem[_3D]): DataCollection = {
    new DataCollection(reference, dataItems.map(f))
  }
}

/**
 * Implements utility functions on [[DataCollection]] instances
 * */
object DataCollection {

  
  /**
   * Builds a [[DataCollection]] instance from a reference mesh and a sequence of meshes in correspondence.
   * Returns a data collection containing the valid elements as well as the list of errors for invalid items.
   * */
  def fromMeshSequence(referenceMesh: TriangleMesh, registeredMeshes : Seq[TriangleMesh]) : (Option[DataCollection], Seq[Throwable]) = {   
    val (transformations, errors) = DataUtils.partitionSuccAndFailedTries(registeredMeshes.map (DataUtils.meshToTransformation(referenceMesh, _)))
    val dc = DataCollection(referenceMesh,transformations.map(DataItem("from mesh", _)))
    if(dc.size > 0) (Some(dc), errors) else (None, errors)    
  }
  
  /**
   * Builds a [[DataCollection]] instance from a reference mesh and a directory containing meshes in correspondence with the reference.
   * Only vtk and stl meshes are currently supported.
   * @return a data collection containing the valid elements as well as the list of errors for invalid items.
   * */
  def fromMeshDirectory(referenceMesh: TriangleMesh, meshDirectory : File) :  (Option[DataCollection], Seq[
    Throwable]) = { 
    val meshFileNames = meshDirectory.listFiles().toSeq.filter(fn => fn.getAbsolutePath().endsWith(".vtk") || fn.getAbsolutePath() .endsWith(".stl"))
    val (meshes, ioErrors) = DataUtils.partitionSuccAndFailedTries( for (meshFn <- meshFileNames) yield { MeshIO.readMesh(meshFn) })    
    val (dc, meshErrors) = fromMeshSequence(referenceMesh, meshes)
    (dc, ioErrors ++ meshErrors)
  }
  
  
  /**
   * Performs a Generalized Procrustes Analysis on the data collection. 
   * This is done by repeatedly computing a new reference mesh that is the mean of all meshes in the dataset and 
   * aligining all items rigidly to the mean.  
   * The final mean mesh will be the reference of the new data collection. 
   * 
   * */
  @tailrec
  def gpa(dc: DataCollection, maxIteration: Int = 3, haltDistance:Double = 1.0): DataCollection = {

    if (maxIteration == 0) return dc

    val allShapesPoints = dc.dataItems.map { dataitem => dc.reference.points.map(dataitem.transformation) }
    val nbShapes = dc.size

    // compute mean shape
    val meanShapePoints = allShapesPoints.par.reduce {
      (points1, points2) => points1.zip(points2).map(a => a._1 + a._2.toVector)
    }.map(Point(0, 0, 0) + _.toVector * (1.0 / nbShapes.toFloat))

    val newMeanMesh = TriangleMesh(meanShapePoints.seq.toIndexedSeq, dc.reference.cells)
    
    // if the new mean is close enough to the old one return
    if (MeshMetrics.procrustesDistance(newMeanMesh, dc.reference) < haltDistance){
      dc
    }
    else {
      // align all shape to it and create a transformation from the mean to the aligned shape 
      val alignedShapesTransformations = dc.dataItems.toSeq.zip(allShapesPoints).par.map { case (item, points) =>
        val transform = LandmarkRegistration.rigid3DLandmarkRegistration(points.zip(meanShapePoints).toIndexedSeq)
        val alignedPoints = points.map(transform)

        val t = meanShapePoints.zip(alignedPoints).toMap
        val returnedTrans = new Transformation[_3D] {
          override val domain = dc.dataItems.headOption.map(d => d.transformation.domain).getOrElse(RealSpace[_3D])
          override val f = (x: Point[_3D]) => t(x)
        }
        DataItem("gpa -> "+ item.info, returnedTrans)
      }

      val newdc = DataCollection(newMeanMesh, alignedShapesTransformations.toIndexedSeq)
      gpa(newdc , maxIteration-1)
    }
  }
}
