package scalismo.statisticalmodel.dataset

import scalismo.common.interpolation.FieldInterpolator
import scalismo.common.{DiscreteDomain, DiscreteField, Field, Scalar, UnstructuredPointsDomain}
import scalismo.geometry.{_3D, EuclideanVector, EuclideanVector3D, Point, Point3D}
import scalismo.common.interpolation.{FieldInterpolator, NearestNeighborInterpolator}
import scalismo.common.{DiscreteDomain, DiscreteField, Field, Scalar, UnstructuredPointsDomain}
import scalismo.geometry.{_3D, EuclideanVector, EuclideanVector3D, Point, Point3D}
import scalismo.mesh.{LineMesh, MeshMetrics, TetrahedralMesh, TriangleMesh}
import scalismo.registration.LandmarkRegistration
import scalismo.transformations.Transformation
import scalismo.statisticalmodel.dataset.DataCollection.TriangleMeshDataCollection
import scalismo.utils.Random

import scala.annotation.tailrec
import scala.language.higherKinds
import scala.collection.parallel.immutable.ParVector

case class CrossvalidationFold[D, DDomain[D] <: DiscreteDomain[D], Value](
  trainingData: DataCollection[D, DDomain, Value],
  testingData: DataCollection[D, DDomain, Value]
)

case class DataCollection[D, DDomain[D] <: DiscreteDomain[D], Value](dataItems: Seq[DiscreteField[D, DDomain, Value]]) {
  require(dataItems.size > 0 && dataItems.forall(di => di.domain == dataItems.head.domain))

  val size: Int = dataItems.size

  def reference: DDomain[D] = dataItems.head.domain

  def createCrossValidationFolds(
    nFolds: Int
  )(implicit rng: scalismo.utils.Random): Seq[CrossvalidationFold[D, DDomain, Value]] = {
    require(nFolds > 1)
    val shuffledDataItems = rng.scalaRandom.shuffle(dataItems)
    val foldSize = shuffledDataItems.size / nFolds
    val dataGroups = shuffledDataItems.grouped(foldSize).toSeq

    val folds = for (currFold <- 0 until nFolds) yield {
      val testingDataItems = dataGroups(currFold)
      val testingCollection = DataCollection(testingDataItems)
      val trainingDataItems = (dataGroups.slice(0, currFold).flatten ++: dataGroups
        .slice(currFold + 1, dataGroups.size)
        .flatten)
      val trainingCollection = DataCollection(trainingDataItems)

      CrossvalidationFold(trainingCollection, testingCollection)
    }
    folds
  }

  def fields(interpolator: FieldInterpolator[D, DDomain, Value]): Seq[Field[D, Value]] = {
    for (dataItem <- dataItems) yield {
      dataItem.interpolate(interpolator)
    }
  }

  /**
   * Perform a leave one out crossvalidation. See nFoldCrossvalidation for details
   */
  def createLeaveOneOutFolds(implicit
    rng: scalismo.utils.Random
  ): Seq[CrossvalidationFold[D, DDomain, Value]] = {
    createCrossValidationFolds(size)
  }

}

object DataCollection {

  type TriangleMeshDataCollection[D] = DataCollection[D, TriangleMesh, EuclideanVector[D]]
  type TetrahedralMeshDataCollection[D] = DataCollection[D, TetrahedralMesh, EuclideanVector[_3D]]
  type ScalarVolumeMeshField3DDataCollection[D, A] = DataCollection[D, TetrahedralMesh, A]
  type LineMeshDataCollection[D] = DataCollection[D, LineMesh, EuclideanVector[D]]
  type UnstructuredPointsDomainCollection[D] = DataCollection[D, UnstructuredPointsDomain, EuclideanVector[D]]

  /*
   * Performs a Generalized Procrustes Analysis on the data collection.
   * This is done by repeatedly computing the mean of all meshes in the dataset and
   * aligning all items rigidly to the mean.
   *
   * The reference mesh is unchanged, only the transformations in the collection are adapted
   */
  def gpa(dc: TriangleMeshDataCollection[_3D], maxIteration: Int = 3, haltDistance: Double = 1e-5)(implicit
    rng: Random
  ): TriangleMeshDataCollection[_3D] = {

    TriangleMeshDataCollection.gpa(dc, maxIteration, haltDistance)
  }

  private def differenceFieldToReference[D, DDomain[D] <: DiscreteDomain[D]](
    reference: DDomain[D],
    mesh: DDomain[D]
  ): DiscreteField[D, DDomain, EuclideanVector[D]] = {
    require(reference.pointSet.numberOfPoints == mesh.pointSet.numberOfPoints)

    val vecs = for ((refPt, meshPt) <- reference.pointSet.points.zip(mesh.pointSet.points)) yield {
      meshPt - refPt
    }
    DiscreteField(reference, vecs.toIndexedSeq)
  }

  def fromTriangleMesh3DSequence(reference: TriangleMesh[_3D],
                                 meshes: Seq[TriangleMesh[_3D]]
  ): TriangleMeshDataCollection[_3D] = {

    val dfs = for (mesh <- meshes) yield {
      differenceFieldToReference[_3D, TriangleMesh](reference, mesh)
    }
    new DataCollection(dfs)
  }

  def fromTetrahedralMesh3DSequence(reference: TetrahedralMesh[_3D],
                                    meshes: Seq[TetrahedralMesh[_3D]]
  ): TetrahedralMeshDataCollection[_3D] = {

    val dfs = for (mesh <- meshes) yield {
      differenceFieldToReference[_3D, TetrahedralMesh](reference, mesh)
    }
    new DataCollection(dfs)
  }

  def fromScalarVolumeMesh3DSequence[A: Scalar](
    scalarVolumeMeshFields: Seq[DiscreteField[_3D, TetrahedralMesh, A]]
  ): ScalarVolumeMeshField3DDataCollection[_3D, A] = {
    new DataCollection[_3D, TetrahedralMesh, A](scalarVolumeMeshFields)
  }

  def fromLineMeshSequence[D](reference: LineMesh[D], meshes: Seq[LineMesh[D]]): LineMeshDataCollection[D] = {
    val dfs = for (mesh <- meshes) yield {
      differenceFieldToReference[D, LineMesh](reference, mesh)
    }
    new DataCollection(dfs)
  }

  def fromUnstructuredPointsDomainSequence[D](
    reference: UnstructuredPointsDomain[D],
    meshes: Seq[UnstructuredPointsDomain[D]]
  ): UnstructuredPointsDomainCollection[D] = {
    val dfs = for (mesh <- meshes) yield {
      differenceFieldToReference[D, UnstructuredPointsDomain](reference, mesh)
    }
    new DataCollection(dfs)
  }
}

// TODO: Implement more general version of GPA
object TriangleMeshDataCollection {

  /**
   * Returns the mean transformation from all the transformations in the datacollection
   */
  private def meanTransformation(dc: TriangleMeshDataCollection[_3D]): Transformation[_3D] = {
    val fields = dc.fields(NearestNeighborInterpolator())

    Transformation { (pt: Point[_3D]) =>
      {
        var meanPoint = EuclideanVector3D(0, 0, 0)

        for (field <- fields) {
          meanPoint = meanPoint + (pt + field(pt)).toVector
        }
        (meanPoint / fields.size).toPoint

      }
    }
  }

  private def meanSurfaceFromDataCollection(dc: TriangleMeshDataCollection[_3D]): TriangleMesh[_3D] = {
    dc.reference.transform(meanTransformation(dc))
  }

  def gpa(dc: TriangleMeshDataCollection[_3D], maxIteration: Int = 5, haltDistance: Double = 1e-5)(implicit
    rng: Random
  ): TriangleMeshDataCollection[_3D] = {
    gpaComputation(dc, meanSurfaceFromDataCollection(dc), maxIteration, haltDistance)
  }

  @tailrec
  private def gpaComputation(dc: TriangleMeshDataCollection[_3D],
                             meanShape: TriangleMesh[_3D],
                             maxIteration: Int,
                             haltDistance: Double
  )(implicit rng: Random): TriangleMeshDataCollection[_3D] = {

    if (maxIteration == 0) return dc

    val referencePoints = dc.reference.pointSet.points.toIndexedSeq
    val numberOfPoints = referencePoints.size
    val referenceCenterOfMass =
      referencePoints.foldLeft(Point3D(0, 0, 0))((acc, pt) => acc + (pt.toVector / numberOfPoints))

    val meanShapePoints = meanShape.pointSet.points.toIndexedSeq

    val fields = dc.fields(NearestNeighborInterpolator())

    // align all shape to it and create a transformations from the mean to the aligned shape
    val newDiscreteFields = new ParVector(fields.toVector).map { field =>
      val surface = dc.reference.transform(p => p + field(p))
      val transform =
        LandmarkRegistration.rigid3DLandmarkRegistration(surface.pointSet.points.toIndexedSeq.zip(meanShapePoints),
                                                         referenceCenterOfMass
        )
      val newVecs = dc.reference.pointSet.points.toIndexedSeq.map(p => transform(p + field(p)) - p)
      new DiscreteField[_3D, TriangleMesh, EuclideanVector[_3D]](dc.reference, newVecs)
    }

    val newdc = DataCollection(newDiscreteFields.seq)
    val newMean = meanSurfaceFromDataCollection(newdc)

    if (MeshMetrics.procrustesDistance(meanShape, newMean) < haltDistance) {
      newdc
    } else {
      gpaComputation(newdc, newMean, maxIteration - 1, haltDistance)
    }
  }

}
