package smptk.image
import Image._

object Resample {
  def sample2D[Pixel: ScalarPixel](img: ContinuousScalarImage2D, domain: DiscreteImageDomain2D, outsideValue: Pixel): DiscreteScalarImage2D[Pixel] = {
    val scalarPixel = implicitly[ScalarPixel[Pixel]]
    val sampledValues = domain.points.par.map(pt => {
      if (img.isDefinedAt(pt)) scalarPixel.fromFloat(img(pt))
      else outsideValue
    })
    DiscreteScalarImage2D(domain, sampledValues.toIndexedSeq)
  }
}