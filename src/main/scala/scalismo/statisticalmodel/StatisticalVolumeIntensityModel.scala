package scalismo.statisticalmodel

import breeze.linalg.DenseVector
import scalismo.common._
import scalismo.geometry._
import scalismo.mesh._
import scalismo.utils.Random

import scala.reflect.ClassTag
import scala.reflect.runtime.universe.TypeTag

trait StatisticalVolumeIntensityModel[S] {

  def referenceMesh: TetrahedralMesh3D

  def referenceMeshField: ScalarVolumeMeshField[S]

  def shape: StatisticalVolumeMeshModel

  def intensity: DiscreteLowRankGaussianProcess[_3D, UnstructuredPointsDomain[_3D], S]

  def landmarks: Map[String, Landmark[_3D]] = Map.empty[String, Landmark[_3D]]

  def mean: ScalarVolumeMeshField[S]

  def instance(coefficients: SVIMCoefficients): ScalarVolumeMeshField[S]

  def sample()(implicit rnd: Random): ScalarVolumeMeshField[S]

  def withLandmarks(landmarksMap: Map[String, Landmark[_3D]]): StatisticalVolumeIntensityModel[S]

  def hasLandmarks: Boolean = landmarks.nonEmpty

  def landmarkPointId(id: String): Option[PointId] = {
    for {
      lm <- landmarks.get(id)
      id <- referenceMesh.pointSet.pointId(lm.point)
    } yield id
  }

  def landmarksWithPointIds: Map[String, Option[PointId]] = landmarks.map { case (id, lm) => id -> referenceMesh.pointSet.pointId(lm.point) }

  def landmarksWithClosestPointIds: Map[String, PointId] = landmarks.map { case (id, lm) => id -> referenceMesh.pointSet.findClosestPoint(lm.point).id }

  def zeroCoefficients: SVIMCoefficients
}

object StatisticalVolumeIntensityModel {

  def apply[S: Scalar: TypeTag: ClassTag](referenceMeshField: ScalarVolumeMeshField[S],
    shape: StatisticalVolumeMeshModel, intensity: DiscreteLowRankGaussianProcess[_3D, UnstructuredPointsDomain[_3D], S],
    landmarks: Map[String, Landmark[_3D]]): SVIM[S] = {
    SVIM(referenceMeshField, shape, intensity, landmarks)
  }

  def apply[S: Scalar: TypeTag: ClassTag](referenceMeshField: ScalarVolumeMeshField[S],
    shape: StatisticalVolumeMeshModel, intensity: DiscreteLowRankGaussianProcess[_3D, UnstructuredPointsDomain[_3D], S]): SVIM[S] = {
    SVIM(referenceMeshField, shape, intensity, Map.empty)
  }

}

case class SVIM[S: Scalar: TypeTag: ClassTag](referenceMeshField: ScalarVolumeMeshField[S],
  shape: StatisticalVolumeMeshModel,
  intensity: DiscreteLowRankGaussianProcess[_3D, UnstructuredPointsDomain[_3D], S],
  override val landmarks: Map[String, Landmark[_3D]] = Map.empty[String, Landmark[_3D]])
    extends StatisticalVolumeIntensityModel[S] {

  override def mean: ScalarVolumeMeshField[S] = {
    ScalarVolumeMeshField(shape.mean, intensity.mean.data)
  }

  override def instance(coefficients: SVIMCoefficients): ScalarVolumeMeshField[S] = {
    ScalarVolumeMeshField(shape.instance(coefficients.shape), intensity.instance(coefficients.intensity).data)
  }

  override def sample()(implicit rnd: Random): ScalarVolumeMeshField[S] = {
    ScalarVolumeMeshField(shape.sample(), intensity.sample().data)
  }

  override def withLandmarks(landmarksMap: Map[String, Landmark[_3D]]): SVIM[S] = SVIM(referenceMeshField, shape, intensity, landmarksMap)

  override def zeroCoefficients: SVIMCoefficients = SVIMCoefficients(
    DenseVector.zeros[Double](shape.rank),
    DenseVector.zeros[Double](intensity.rank)
  )

  def truncate(shapeComps: Int, colorComps: Int): SVIM[S] = {
    require(shapeComps >= 0 && shapeComps <= shape.rank, "illegal number of reduced shape components")
    require(colorComps >= 0 && colorComps <= intensity.rank, "illegal number of reduced color components")

    SVIM(
      referenceMeshField,
      shape.truncate(shapeComps),
      intensity.truncate(colorComps),
      landmarks)
  }

  override def referenceMesh: TetrahedralMesh3D = referenceMeshField.mesh
}