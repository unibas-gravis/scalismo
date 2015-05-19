package scalismo.statisticalmodel.asm

import breeze.linalg.DenseVector
import ncsa.hdf.`object`.Group
import scalismo.common.Scalar
import scalismo.geometry.{Point, Vector, _3D}
import scalismo.image.DiscreteScalarImage
import scalismo.io.HDF5File
import scalismo.mesh.TriangleMesh
import scalismo.statisticalmodel.asm.FeatureExtractor.{FIG, FI}
import scalismo.utils.ImageConversion
import vtk.{vtkImageGaussianSmooth, vtkObjectBase}

import scala.collection.immutable.TreeMap
import scala.reflect.ClassTag
import scala.reflect.runtime.universe.TypeTag
import scala.util.{Success, Failure, Try}

trait FeatureExtractorSerializer {
  def identifier: String

  def loadHdf5(h5File: HDF5File, h5Group: Group): Try[FeatureExtractor]

  def saveHdf5(fe: FeatureExtractor, h5File: HDF5File, h5Group: Group): Try[Unit]
}

object FeatureExtractorSerializer {

  val IdentifierAttributeName = "type"

  private var instances = new TreeMap[String, FeatureExtractorSerializer]()

  def register(instance: FeatureExtractorSerializer): Unit = {
    instances = instances + ((instance.identifier, instance))
  }

  def get(identifier: String): Try[FeatureExtractorSerializer] = {
    instances.get(identifier) match {
      case Some(value) => Success(value)
      case None => Failure(new IllegalArgumentException(s"No Feature extractor serializer found for type=$identifier." +
        " You may need to call FeatureExtractorSerializer.register() once to make the implementation available."))
    }
  }

  private def registerBuiltins(): Unit = {
    register(NormalDirectionGradientGaussianFeatureExtractorSerializer)
  }

  // on object instantiation, register built-in implementations.
  registerBuiltins()
}

/*
 Logically, the Feature extractor is a single function (image, mesh, point) -> feature vector.
 For performance reasons, we must allow the arguments to be partly fixed (i.e., currying, but at the type level ;-)).
 That's what the following three traits are here for.
 */
trait FeatureExtractor extends Function1[DiscreteScalarImage[_3D, Float], FeatureImageGenerator] {
  def identifier: String
}
trait FeatureImageGenerator extends Function1[TriangleMesh, FeatureImage]
trait FeatureImage extends Function1[Point[_3D], DenseVector[Float]]

// TODO
// an alternative (to be discussed)
trait FE extends Function1[DiscreteScalarImage[_3D, Float], (TriangleMesh => (Point[_3D] => DenseVector[Float]))] {
  def identifier: String
}

// same thing using type aliases:
trait FE2 extends Function1[DiscreteScalarImage[_3D, Float], FIG] {
  def identifier: String
}

object FeatureExtractor {
  type FI = (Point[_3D] => DenseVector[Float])
  type FIG = (TriangleMesh => FI)

  def filterGaussian[T: Scalar : ClassTag : TypeTag](img: DiscreteScalarImage[_3D, T], sigma: Double): DiscreteScalarImage[_3D, T] = {

    val vtkImg = ImageConversion.imageToVtkStructuredPoints[_3D, T](img)
    val gaussianFilter = new vtkImageGaussianSmooth()
    gaussianFilter.SetInputData(vtkImg)
    gaussianFilter.SetStandardDeviation(sigma / img.domain.spacing(0), sigma / img.domain.spacing(1), sigma / img.domain.spacing(2))
    gaussianFilter.Update()
    val vtkRes = gaussianFilter.GetOutput()
    val imgRes = ImageConversion.vtkStructuredPointsToScalarImage[_3D, T](vtkRes).get
    vtkObjectBase.JAVA_OBJECT_MANAGER.gc(false)
    imgRes
  }

}

