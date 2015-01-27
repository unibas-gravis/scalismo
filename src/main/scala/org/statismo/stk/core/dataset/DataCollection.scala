package org.statismo.stk.core.dataset

import java.io.File

import scala.annotation.tailrec
import scala.util.Random

import org.statismo.stk.core.geometry.Dim
import org.statismo.stk.core.geometry.Point
import org.statismo.stk.core.geometry._3D
import org.statismo.stk.core.io.MeshIO
import org.statismo.stk.core.mesh.MeshMetrics
import org.statismo.stk.core.mesh.TriangleMesh
import org.statismo.stk.core.registration.LandmarkRegistration
import org.statismo.stk.core.registration.Transformation

private [dataset] case class CrossvalidationFold(val trainingData: DataCollection, val testingData: DataCollection)
case class DataItem[D <: Dim](val info: String, val transformation: Transformation[D])

case class DataCollection(reference: TriangleMesh, dataItems: Seq[DataItem[_3D]]) {

  val size: Int = dataItems.size

  def createCrossValidationFolds(nFolds: Int): Seq[CrossvalidationFold] = {

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

  def createLeaveOneOutFolds = createCrossValidationFolds(dataItems.size)

  def mapItems(f: DataItem[_3D] => DataItem[_3D]): DataCollection = {
    new DataCollection(reference, dataItems.map(f))
  }
}

object DataCollection {

  def fromMeshSequence(referenceMesh: TriangleMesh, registeredMeshes : Seq[TriangleMesh]) : (Option[DataCollection], Seq[Throwable]) = {   
    val (transformations, errors) = DataUtils.partitionSuccAndFailedTries(registeredMeshes.map (DataUtils.meshToTransformation(referenceMesh, _)))
    val dc = DataCollection(referenceMesh,transformations.map(DataItem("from mesh", _)))
    if(dc.size > 0) (Some(dc), errors) else (None, errors)    
  }
  
  def fromMeshDirectory(referenceMesh: TriangleMesh, meshDirectory : File) :  (Option[DataCollection], Seq[Throwable]) = { 
    val meshFileNames = meshDirectory.listFiles().toSeq.filter(fn => fn.getAbsolutePath().endsWith(".vtk") || fn.getAbsolutePath() .endsWith(".stl"))
    val (meshes, ioErrors) = DataUtils.partitionSuccAndFailedTries( for (meshFn <- meshFileNames) yield { MeshIO.readMesh(meshFn) })    
    val (dc, meshErrors) = fromMeshSequence(referenceMesh, meshes)
    (dc, ioErrors ++ meshErrors)
  }
  
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
      val alignedShapesTransformations = allShapesPoints.par.map { points =>
        val transform = LandmarkRegistration.rigid3DLandmarkRegistration(points.zip(meanShapePoints).toIndexedSeq).transform
        val alignedPoints = points.map(transform)

        val t = meanShapePoints.zip(alignedPoints).toMap
        val returnedTrans = new Transformation[_3D] {
          def apply(x: Point[_3D]) = t(x)
          def takeDerivative(x: Point[_3D]) = ???
        }
        DataItem("gpa", returnedTrans)
      }

      val newdc = DataCollection(newMeanMesh, alignedShapesTransformations.toIndexedSeq)
      gpa(newdc , maxIteration-1)
    }
  }
}
