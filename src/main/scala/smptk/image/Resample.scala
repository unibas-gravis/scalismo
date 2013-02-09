package smptk.image
import Image._
import scala.reflect.ClassTag

object Resample {
  def sample2D[@specialized(Short, Float) Pixel: ScalarPixel : ClassTag](img: ContinuousScalarImage2D, domain: DiscreteImageDomain2D, outsideValue: Pixel): DiscreteScalarImage2D[Pixel] = {
    val scalarPixel = implicitly[ScalarPixel[Pixel]]
    val sampledValues = domain.points.map(pt => {
      if (img.isDefinedAt(pt)) scalarPixel.fromFloat(img(pt))
      else outsideValue
    })
    DiscreteScalarImage2D(domain, sampledValues)
  }
}