package scalismo.statisticalmodel.experimental.dataset

import java.io.File

import scalismo.geometry.{_3D, EuclideanVector3D, Point}
import scalismo.io.MeshIO
import scalismo.mesh.{TetrahedralMesh, TetrahedralMesh3D}
import scalismo.registration.Transformation
import scalismo.statisticalmodel.dataset.{DataItem, DataUtils}
import scalismo.utils.Random

private[dataset] case class CrossvalidationFoldVolumeMesh(trainingData: DataCollectionOfVolumeMesh,
                                                          testingData: DataCollectionOfVolumeMesh)

/**
 * Data-structure for handling a dataset of registered 3D meshes. All pre-implemented operations such as building a
 * PCA model or performing a Generalized Procrustes Analysis require a DataCollection as input
 *
 * @param reference The reference mesh of the dataset. This is the mesh that was registered to all other items of the dataset.
 * @param dataItems Sequence of data items containing the required transformations to apply to the reference mesh in order to obtain
 * other elements of the dataset.
 */
case class DataCollectionOfVolumeMesh(reference: TetrahedralMesh[_3D], dataItems: Seq[DataItem[_3D]])(
  implicit random: Random
) {

  val size: Int = dataItems.size

  private[dataset] def createCrossValidationFolds(nFolds: Int): Seq[CrossvalidationFoldVolumeMesh] = {

    val shuffledDataItems = random.scalaRandom.shuffle(dataItems)
    val foldSize = shuffledDataItems.size / nFolds
    val dataGroups = shuffledDataItems.grouped(foldSize).toSeq

    val folds = for (currFold <- 0 until nFolds) yield {
      val testingDataItems = dataGroups(currFold)
      val testingCollection = DataCollectionOfVolumeMesh(reference, testingDataItems)
      val trainingDataItems = (dataGroups.slice(0, currFold).flatten ++: dataGroups
        .slice(currFold + 1, dataGroups.size)
        .flatten)
      val trainingCollection = DataCollectionOfVolumeMesh(reference, trainingDataItems)

      CrossvalidationFoldVolumeMesh(trainingCollection, testingCollection)
    }
    folds
  }

  private[dataset] def createLeaveOneOutFolds = createCrossValidationFolds(dataItems.size)

  /**
   * Returns a new DataCollectionofMeshvolume where the given function was applied to all data items
   */
  def mapItems(f: DataItem[_3D] => DataItem[_3D]): DataCollectionOfVolumeMesh = {
    new DataCollectionOfVolumeMesh(reference, dataItems.map(f))
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

    Transformation { (pt: Point[_3D]) =>
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

object DataCollectionOfVolumeMesh {

  /**
   * Builds a [[DataCollectionOfVolumeMesh]] instance from a reference mesh volume and a sequence of meshes in correspondence.
   * Returns a data collection containing the valid elements as well as the list of errors for invalid items.
   */
  def fromMeshSequence(referenceMesh: TetrahedralMesh[_3D], registeredMeshes: Seq[TetrahedralMesh[_3D]])(
    implicit rng: Random
  ): (Option[DataCollectionOfVolumeMesh], Seq[Throwable]) = {
    val (transformations, errors) = DataUtils.partitionSuccAndFailedTries(
      registeredMeshes.map(DataUtils.volumeMeshToTransformation(referenceMesh, _))
    )
    val dc = DataCollectionOfVolumeMesh(referenceMesh, transformations.map(DataItem("from mesh", _)))
    if (dc.size > 0) (Some(dc), errors) else (None, errors)
  }

  /**
   * Builds a [[DataCollectionOfVolumeMesh]] instance from a reference mesh volume and a directory containing meshe volumes in correspondence with the reference.
   * Only vtk and stl meshes are currently supported.
   *
   * @return a data collection containing the valid elements as well as the list of errors for invalid items.
   */
  def fromMeshDirectory(referenceMesh: TetrahedralMesh[_3D], meshDirectory: File)(
    implicit rng: Random
  ): (Option[DataCollectionOfVolumeMesh], Seq[Throwable]) = {
    val meshFileNames = meshDirectory
      .listFiles()
      .toSeq
      .filter(fn => fn.getAbsolutePath.endsWith(".vtk") || fn.getAbsolutePath.endsWith(".vtu")||fn.getAbsolutePath.endsWith(".inp"))
    val (meshes, ioErrors) = DataUtils.partitionSuccAndFailedTries(for (meshFn <- meshFileNames) yield {
      MeshIO.readMesh(meshFn).map(m => TetrahedralMesh3D(m.pointSet, referenceMesh.tetrahedralization))
    })
    val (dc, meshErrors) = fromMeshSequence(referenceMesh, meshes)
    (dc, ioErrors ++ meshErrors)
  }

  //  /**
  //   * Performs a Generalized Procrustes Analysis on the data collection.
  //   * This is done by repeatedly computing the mean of all meshes in the dataset and
  //   * aligning all items rigidly to the mean.
  //   *
  //   * The reference mesh is unchanged, only the transformations in the collection are adapted
  //   */
  //  def gpa(dc: DataCollectionOfMeshVolume, maxIteration: Int = 3, haltDistance: Double = 1e-5)(implicit rng: Random): DataCollectionOfMeshVolume = {
  //    gpaComputation(dc, dc.meanSurface, maxIteration, haltDistance)
  //  }
  //
  //  @tailrec
  //  private def gpaComputation(dc: DataCollectionOfMeshVolume, meanShape: TetrahedralMesh[_3D], maxIteration: Int, haltDistance: Double)(implicit rng: Random): DataCollectionOfMeshVolume = {
  //
  //    if (maxIteration == 0) return dc
  //
  //    val referencePoints = dc.reference.pointSet.points.toIndexedSeq
  //    val numberOfPoints = referencePoints.size
  //    val referenceCenterOfMass = referencePoints.foldLeft(Point3D(0, 0, 0))((acc, pt) => acc + (pt.toVector / numberOfPoints))
  //
  //    val meanShapePoints = meanShape.pointSet.points.toIndexedSeq
  //
  //    // align all shape to it and create a transformation from the mean to the aligned shape
  //    val dataItemsWithAlignedTransform = dc.dataItems.par.map { dataItem =>
  //      val surface = dc.reference.transform(dataItem.transformation)
  //      val transform = LandmarkRegistration.rigid3DLandmarkRegistration(surface.pointSet.points.toIndexedSeq.zip(meanShapePoints), referenceCenterOfMass)
  //
  //      DataItem("gpa -> " + dataItem.info, Transformation(transform.compose(dataItem.transformation)))
  //    }
  //
  //    val newdc = DataCollectionOfMeshVolume(dc.reference, dataItemsWithAlignedTransform.toIndexedSeq)
  //    val newMean = newdc.meanSurface
  //
  //    if (MeshVolumeMetrics.procrustesDistance(meanShape, newMean) < haltDistance) {
  //      newdc
  //    } else {
  //      gpaComputation(newdc, newMean, maxIteration - 1, haltDistance)
  //    }
  //  }

}
