package scalismo.statisticalmodel.experimental

import breeze.linalg.DenseVector
import scalismo.common._
import scalismo.geometry._
import scalismo.mesh._
import scalismo.statisticalmodel.DiscreteLowRankGaussianProcess
import scalismo.utils.Random

import scala.reflect.ClassTag
import scala.reflect.runtime.universe.TypeTag

trait StatisticalVolumeIntensityModel[S] {

  def referenceMeshField: ScalarVolumeMeshField[S]

  def shape: StatisticalVolumeMeshModel

  def intensity: DiscreteLowRankGaussianProcess[_3D, UnstructuredPointsDomain[_3D], S]

  def mean: ScalarVolumeMeshField[S]

  def instance(coefficients: SVIMCoefficients): ScalarVolumeMeshField[S]

  def sample()(implicit rnd: Random): ScalarVolumeMeshField[S]

  def zeroCoefficients: SVIMCoefficients
}

object StatisticalVolumeIntensityModel {

  def apply[S: Scalar: TypeTag: ClassTag](
    referenceMeshField: ScalarVolumeMeshField[S],
    shape: StatisticalVolumeMeshModel,
    intensity: DiscreteLowRankGaussianProcess[_3D, UnstructuredPointsDomain[_3D], S]
  ): SVIM[S] = {
    SVIM(referenceMeshField, shape, intensity)
  }

}

case class SVIM[S: Scalar: TypeTag: ClassTag](
  referenceMeshField: ScalarVolumeMeshField[S],
  shape: StatisticalVolumeMeshModel,
  intensity: DiscreteLowRankGaussianProcess[_3D, UnstructuredPointsDomain[_3D], S]
) extends StatisticalVolumeIntensityModel[S] {

  override def mean: ScalarVolumeMeshField[S] = {
    ScalarVolumeMeshField(shape.mean, warpReferenceIntensity(intensity.mean.data))
  }

  override def instance(coefficients: SVIMCoefficients): ScalarVolumeMeshField[S] = {
    ScalarVolumeMeshField(shape.instance(coefficients.shape),
                          warpReferenceIntensity(intensity.instance(coefficients.intensity).data))
  }

  override def sample()(implicit rnd: Random): ScalarVolumeMeshField[S] = {
    ScalarVolumeMeshField(shape.sample(), warpReferenceIntensity(intensity.sample().data))
  }

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
      intensity.truncate(colorComps)
    )
  }

  private def warpReferenceIntensity(scalarData: IndexedSeq[S]): ScalarArray[S] = {
    ScalarArray[S](
      referenceMeshField.data
        .zip(ScalarArray[S](scalarData.toArray))
        .map { case (r, s) => Scalar[S].plus(r, s) }
        .toArray
    )
  }
}
