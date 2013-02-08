package smptk.registration

import org.scalatest.FunSpec
import java.nio.ByteBuffer
import java.io.File
import java.io.IOException
import smptk.image.Interpolation
import smptk.image.DiscreteImageDomain2D
import smptk.io.ImageIO
import smptk.image.Image._
import breeze.linalg.DenseVector
import smptk.image.Resample
import smptk.image.DiscreteImageDomain1D
import smptk.image.ContinuousScalarImage1D
import smptk.image.Geometry.CoordVector1D

class ImageTest extends FunSpec {
  describe("A translation in 2D") {
    it("translates an image") {
      val discreteImage = ImageIO.read2DScalarImage[Short](new File("/tmp/test.h5")).get
      val continuousImage = Interpolation.interpolate2D(3)(discreteImage)

      val translation = TranslationSpace2D()(DenseVector[Float](10,0))
      val translatedImg = continuousImage.warp(translation)
      val resampledImage = Resample.sample2D[Short](translatedImg, discreteImage.domain, 0)      
      ImageIO.writeImage(resampledImage, new File("/tmp/resampled.h5"))

    }

    it("translates a 1D image") {
      val domain = DiscreteImageDomain1D(-50, 1, 100)  
      val continuousImage = ContinuousScalarImage1D(domain.isInside, (x : CoordVector1D[Float]) => x * x, (x : CoordVector1D[Float]) => DenseVector(2 * x ))

      val translation = TranslationSpace1D()(DenseVector[Float](10))
      val translatedImg = continuousImage.warp(translation)
      

      assert(translatedImg(-10) === 0)
    }

  
  }
}
