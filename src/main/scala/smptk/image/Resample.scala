package smptk
package image
import Image._
import Geometry._
import scala.reflect.ClassTag

object Resample {
  def sample3D[@specialized(Short, Float, Double) Pixel: ScalarPixel : ClassTag](img: ContinuousScalarImage3D, domain: DiscreteImageDomain3D, outsideValue: Pixel): DiscreteScalarImage3D[Pixel] = {
    val scalarPixel = implicitly[ScalarPixel[Pixel]]
    val sampledValues = domain.points.par.map((pt : Point3D) => {
      if (img.isDefinedAt(pt)) scalarPixel.fromDouble(img(pt))
      else outsideValue
    })
    
    DiscreteScalarImage3D(domain, sampledValues.toIndexedSeq)
  }
  
  def sample2D[@specialized(Short, Float, Double) Pixel: ScalarPixel : ClassTag](img: ContinuousScalarImage2D, domain: DiscreteImageDomain2D, outsideValue: Pixel): DiscreteScalarImage2D[Pixel] = {
    val scalarPixel = implicitly[ScalarPixel[Pixel]]
    val sampledValues = domain.points.par.map((pt : Point2D) => {
      if (img.isDefinedAt(pt)) scalarPixel.fromDouble(img(pt))
      else outsideValue
    })
    
    DiscreteScalarImage2D(domain, sampledValues.toIndexedSeq)
  }

 def sample[@specialized(Short, Float, Double) Pixel: ScalarPixel : ClassTag](img: ContinuousScalarImage1D, domain: DiscreteImageDomain1D, outsideValue: Pixel): DiscreteScalarImage1D[Pixel] = {
    val scalarPixel = implicitly[ScalarPixel[Pixel]]
    val sampledValues = domain.points.map((pt : Point1D)=> {
      if (img.isDefinedAt(pt)) scalarPixel.fromDouble(img(pt))
      else outsideValue
    })
    DiscreteScalarImage1D(domain, sampledValues.toIndexedSeq)
  }
}

