package smptk
package image

import breeze.plot._
import breeze.linalg._
import Image._
import Geometry.CoordVector1D
import ij._
import ij.process.FloatProcessor
import ij.ImageStack
import ij.WindowManager
import smptk.registration.Transformation
import smptk.image.Geometry.{Point1D, Point2D}

object Utils {

  def show1D[Pixel: ScalarPixel](img: DiscreteScalarImage1D[Pixel]) {
    val pixelConv = implicitly[ScalarPixel[Pixel]]

    val xs = img.domain.points.map(_(0).toDouble)

    val f = Figure()
    val p = f.subplot(0)

    p += plot(xs, img.pixelValues.map(pixelConv.toDouble(_)), '+')

  }

  def show1D(img: ContinuousScalarImage1D, domain: DiscreteImageDomain1D, outsideValue: Double = 0) {

    val xs = linspace(domain.origin(0).toDouble, domain.extent(0), domain.numberOfPoints)
    val f = Figure()
    val p = f.subplot(0)

    p += plot(xs, xs.map(x => img.liftPixelValue(CoordVector1D(x)).getOrElse(outsideValue).toDouble))
    f.refresh
    f.visible = true
  }

  def show1D(img: ContinuousVectorImage[CoordVector1D], domain: DiscreteImageDomain1D) {

    val xs = linspace(domain.origin(0).toDouble, domain.extent(0), domain.numberOfPoints)

    val f = Figure()
    val p = f.subplot(0)

    p += plot(xs, xs.map(x => img(CoordVector1D(x))(0)))

  }

  def showGrid1D(domain: DiscreteImageDomain1D, transform: Transformation[CoordVector1D]) {
    val discreteImage = DiscreteScalarImage1D(domain, domain.points.map(x => if (math.round(x(0)) % 2 == 0) -1. else 1.))
   //val discreteImage = DiscreteScalarImage1D(domain, domain.points.map(x => x(0)))

    val continuousImg = Interpolation.interpolate(3)(discreteImage)
    show1D(continuousImg compose transform, domain)
  }

  def show2D[Pixel: ScalarPixel](img: DiscreteScalarImage2D[Pixel]) {
    val pixelConv = implicitly[ScalarPixel[Pixel]]
    val domain = img.domain

    val bp = new FloatProcessor(domain.size(0), domain.size(1), img.pixelValues.map(pixelConv.toFloat(_)).toArray)
    val imp = new ImagePlus("2D image", bp)
    imp.show()
  }

  def show2D(img: ContinuousScalarImage2D, domain: DiscreteImageDomain2D, outsideValue: Float = 0) {
    val discreteImg = Resample.sample2D(img, domain, outsideValue)
    show2D(discreteImg)
  }

  def show3D[Pixel: ScalarPixel](img: DiscreteScalarImage3D[Pixel]) {
    val pixelConv = implicitly[ScalarPixel[Pixel]]
    val domain = img.domain
    val (width, height, size) = (domain.size(0), domain.size(1), domain.size(2))

    // 	Create 3x3x3 3D stack and fill it with garbage  
    val stack = new ImageStack(width, height)

    val pixelValues = img.pixelValues.map(pixelConv.toFloat(_))
    for (slice <- 0 until size) {
      val startInd = slice * (width * height)
      val endInd = (slice + 1) * (width * height)
      val pixelForSlice = pixelValues.slice(startInd, endInd).toArray
      val bp = new FloatProcessor(width, height, pixelForSlice)
      stack.addSlice(bp)

    }
    val imp = new ImagePlus("3D image", stack)

    imp.show()
  }

  
  def gridImage2D(gridWidth : Double, tolerance : Double) : ContinuousScalarImage2D = {
    def grid(x : Point2D) = {
      if (math.abs(x(0) % gridWidth) < tolerance ||  math.abs(x(1) % gridWidth) < tolerance) 0f else 1f
    }
    def df(x : Point2D) = DenseVector(0.,0.)
    ContinuousScalarImage2D((x : Point2D) =>true, grid, df) 
        
  }

  def gridImage1D(gridWidth : Double, tolerance : Double) : ContinuousScalarImage1D = {
    def grid(x : Point1D) = {
      if (math.abs(x(0) % gridWidth) < tolerance) 0f else 1f
    }
    def df(x : Point1D) = DenseVector(0.)
    ContinuousScalarImage1D((x : Point1D) =>true, grid, df) 
  }

  
  //  def main(args: Array[String]) {
  //    import smptk.io.ImageIO
  //    import java.io.File
  //    val img2d = ImageIO.read2DScalarImage[Short](new File("/home/luethi/workspace/smptk/src/test/resources/lena.h5")).get
  //    val img2dcont = Interpolation.interpolate2D(3)(img2d)
  //    show2D(img2dcont, img2d.domain, 0)
  //    //	   val threeDImgFilename = "/home/luethi/workspace/smptk/../smptkDemo/zim_16.h5"
  //    //	   val img3d = ImageIO.read3DScalarImage[Short](new File(threeDImgFilename)).get
  //    //	   show3D(img3d)
  //  }

}