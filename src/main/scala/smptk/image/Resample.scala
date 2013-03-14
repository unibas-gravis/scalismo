package smptk
package image
import Image._
import Geometry._
import scala.reflect.ClassTag

object Resample {
  def sample2D[@specialized(Short, Float, Double) Pixel: ScalarPixel : ClassTag](img: ContinuousScalarImage2D, domain: DiscreteImageDomain2D, outsideValue: Pixel): DiscreteScalarImage2D[Pixel] = {
    val scalarPixel = implicitly[ScalarPixel[Pixel]]
    val sampledValues = domain.points.map((pt : Point2D) => {
      if (img.isDefinedAt(pt)) scalarPixel.fromDouble(img(pt))
      else outsideValue
    })
    DiscreteScalarImage2D(domain, sampledValues)
  }

 def sample[@specialized(Short, Float, Double) Pixel: ScalarPixel : ClassTag](img: ContinuousScalarImage1D, domain: DiscreteImageDomain1D, outsideValue: Pixel): DiscreteScalarImage1D[Pixel] = {
    val scalarPixel = implicitly[ScalarPixel[Pixel]]
    val sampledValues = domain.points.map((pt : Point1D)=> {
      if (img.isDefinedAt(pt)) scalarPixel.fromDouble(img(pt))
      else outsideValue
    })
    DiscreteScalarImage1D(domain, sampledValues)
  }
}

