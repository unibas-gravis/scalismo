package smptk
package image

import Image._
import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import smptk.image.Interpolation._
import org.scalatest.Ignore
import smptk.io.ImageIO
import java.io.File


class ResampleTest extends FunSpec with ShouldMatchers {
  describe("Resampling a 2D image") {

    it("yields the original discrete image") {
    	//val testImgUrl = getClass().getResource("/3ddf.h5").getPath()
    	val discreteImage = ImageIO.read2DScalarImage[Short](new File("/tmp/test.h5")).get
        val continuousImage = Interpolation.interpolate2D(3)(discreteImage)
        val resampledImage = Resample.sample2D[Short](continuousImage, discreteImage.domain)
        ImageIO.writeImage(resampledImage, new File("/tmp/resampled.h5"))
    }
  }
}