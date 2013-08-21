package org.statismo.stk.core.image

import Image._
import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import Interpolation._
import org.scalatest.Ignore
import org.statismo.stk.core.io.ImageIO
import java.io.File
import org.statismo.stk.core.registration.RotationSpace2D
import org.statismo.stk.core.geometry._
import org.statismo.stk.core.geometry.implicits._
import breeze.linalg.DenseVector

class ResampleTest extends FunSpec with ShouldMatchers {
  org.statismo.stk.core.initialize()

  describe("Resampling a 2D image") {

    val testImgUrl = getClass().getResource("/lena.h5").getPath()
    val discreteImage = ImageIO.read2DScalarImage[Short](new File(testImgUrl)).get
    
     // here we do 1st order interpolation. 3rd order would not work, as it does not necessarily preserve the 
    // pixel values at the strong edges - and we thus could not formulate a reasonable test
    val continuousImage = Interpolation.interpolate(discreteImage, 1)

    it("yields the original discrete image") {
      val resampledImage = Resample.sample[Short](continuousImage, discreteImage.domain, 0)
      discreteImage.pixelValues.size should equal(resampledImage.pixelValues.size)
      for (i <- 0 until discreteImage.pixelValues.size) {
        discreteImage.pixelValues(i) should be(resampledImage.pixelValues(i))
      }

    }
  }


  describe("Resampling a 3D image") {
    val path = getClass().getResource("/3dimage.h5").getPath()
    val discreteImage = ImageIO.read3DScalarImage[Short](new File(path)).get
    val continuousImage = Interpolation.interpolate(discreteImage, 0)

    it("yields the original discrete image") {
      val resampledImage = Resample.sample[Short](continuousImage, discreteImage.domain, 0)
      for (i <- 0 until discreteImage.pixelValues.size by 100) {
        discreteImage.pixelValues(i) should be(resampledImage.pixelValues(i))
      }
    }

  }

}