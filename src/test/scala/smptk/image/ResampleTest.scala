package smptk
package image

import Image._
import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import Interpolation._
import org.scalatest.Ignore
import io.ImageIO
import java.io.File


class ResampleTest extends FunSpec with ShouldMatchers {
  describe("Resampling a 2D image") {

    ignore("yields the original discrete image") {
    	//val testImgUrl = getClass().getResource("/3ddf.h5").getPath()
    	val discreteImage = ImageIO.read2DScalarImage[Short](new File("/tmp/test.h5")).get
        val continuousImage = Interpolation.interpolate2D(3)(discreteImage)
        val resampledImage = Resample.sample2D[Short](continuousImage, discreteImage.domain, 0)
        ImageIO.writeImage(resampledImage, new File("/tmp/resampled.h5"))
    }

      it("yields the original discrete image for a translated domain") {
    	//val testImgUrl = getClass().getResource("/3ddf.h5").getPath()
    	val discreteImage = ImageIO.read2DScalarImage[Short](new File("/tmp/test.h5")).get
        val continuousImage = Interpolation.interpolate2D(3)(discreteImage)
        
        val origin = discreteImage.domain.origin
        val translatedDomain = DiscreteImageDomain2D((origin(0) + 10, origin(1) + 10), discreteImage.domain.spacing, discreteImage.domain.size)        
        val resampledImage = Resample.sample2D[Short](continuousImage, translatedDomain, 0)
        ImageIO.writeImage(resampledImage, new File("/tmp/resampled.h5"))
    }

  
  }
}