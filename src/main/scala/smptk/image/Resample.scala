package smptk
package image
import Image._
import smptk.geometry._
import scala.reflect.ClassTag
import scala.reflect.ClassTag
import scala.reflect.runtime.universe._

object Resample {


  def sample[@specialized(Short, Float, Double) Pixel: ScalarPixel: ClassTag](img: ContinuousScalarImage3D, domain: DiscreteImageDomain3D, outsideValue: Double): DiscreteScalarImage3D[Pixel] = {    
    val scalarPixel = implicitly[ScalarPixel[Pixel]]
    val sampledValues = domain.points.par.map((pt: Point[ThreeD]) => {
      if (img.isDefinedAt(pt)) scalarPixel.fromDouble(img(pt))
      else scalarPixel.fromDouble(outsideValue)
    })

    DiscreteScalarImage3D(domain, sampledValues.toArray)
  }

  def sample[@specialized(Short, Float, Double) Pixel: ScalarPixel: ClassTag](img: ContinuousScalarImage2D, domain: DiscreteImageDomain2D, outsideValue: Double): DiscreteScalarImage2D[Pixel] = {
    val scalarPixel = implicitly[ScalarPixel[Pixel]]
    val sampledValues = domain.points.par.map((pt: Point[TwoD]) => {
      if (img.isDefinedAt(pt)) scalarPixel.fromDouble(img(pt))
      else scalarPixel.fromDouble(outsideValue)
    })

    DiscreteScalarImage2D(domain, sampledValues.toArray)
  }

  def sample[@specialized(Short, Float, Double) Pixel: ScalarPixel: ClassTag](img: ContinuousScalarImage1D, domain: DiscreteImageDomain1D, outsideValue: Double): DiscreteScalarImage1D[Pixel] = {
    val scalarPixel = implicitly[ScalarPixel[Pixel]]
    val sampledValues = domain.points.map((pt: Point[OneD]) => {
      if (img.isDefinedAt(pt)) scalarPixel.fromDouble(img(pt))
      else scalarPixel.fromDouble(outsideValue)
    })
    DiscreteScalarImage1D(domain, sampledValues.toArray)
  }
}

