package scalismo.image

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import java.io.File

import scalismo.io.ImageIO


class ResampleTests extends FunSpec with ShouldMatchers {
  scalismo.initialize()

  describe("Resampling a 2D image") {

    val testImgUrl = getClass.getResource("/lena.h5").getPath
    val discreteImage = ImageIO.read2DScalarImage[Short](new File(testImgUrl)).get

    // here we do 1st order interpolation. 3rd order would not work, as it does not necessarily preserve the
    // pixel values at the strong edges - and we thus could not formulate a reasonable test
    val continuousImage = discreteImage.interpolate(1)

    it("yields the original discrete image") {
      val resampledImage = continuousImage.sample[Short](discreteImage.domain, 0)
      discreteImage.values.size should equal(resampledImage.values.size)
      for (i <- 0 until discreteImage.values.size) {
        discreteImage(i) should be(resampledImage(i))
      }
    }
  }


  describe("Resampling a 3D image") {
    val path = getClass.getResource("/3dimage.h5").getPath
    val discreteImage = ImageIO.read3DScalarImage[Short](new File(path)).get
    val continuousImage = discreteImage.interpolate(0)

    it("yields the original discrete image") {
      val resampledImage = continuousImage.sample[Short](discreteImage.domain, 0)
      for (i <- 0 until discreteImage.values.size by 100) {
        discreteImage(i) should be(resampledImage(i))
      }
    }
  }
}