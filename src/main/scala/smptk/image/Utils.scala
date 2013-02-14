package smptk.image

import breeze.plot._
import breeze.linalg._
import Image._
import smptk.image.Geometry.CoordVector1D

object Utils {

  def show[Pixel: ScalarPixel](img: DiscreteScalarImage1D[Pixel]) {
	val pixelConv = implicitly[ScalarPixel[Pixel]]
	
    val xs = img.domain.points.map(_(0).toDouble)

    val f = Figure()
    val p = f.subplot(0)

    p += plot(xs, img.pixelValues.map(pixelConv.toDouble(_)), '+')

  }

  def show(img: ContinuousScalarImage1D, domain : DiscreteImageDomain1D, outsideValue : Float = 0) {
	
    val xs = linspace(domain.origin(0).toDouble, domain.origin(0) + domain.extent(0), 1000)

    val f = Figure()
    val p = f.subplot(0)

    p += plot(xs, xs.map(x => img.liftPixelValue(x.toFloat).getOrElse(outsideValue).toDouble))

  }

    def show(img: ContinuousVectorImage[CoordVector1D], domain : DiscreteImageDomain1D) {
	
    val xs = linspace(domain.origin(0).toDouble, domain.origin(0) + domain.extent(0), 1000)

    val f = Figure()
    val p = f.subplot(0)

    p += plot(xs, xs.map(x => img(CoordVector1D(x.toFloat))(0).toDouble))

  }


}