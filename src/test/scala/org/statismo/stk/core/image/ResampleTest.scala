package org.statismo.stk.core.image

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import Interpolation._
import org.scalatest.Ignore
import org.statismo.stk.core.io.ImageIO
import java.io.File
import org.statismo.stk.core.geometry._
import org.statismo.stk.core.geometry.Point._
import org.statismo.stk.core.geometry.Vector._
import org.statismo.stk.core.geometry.Index._

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
      discreteImage.values.size should equal(resampledImage.values.size)
      for (i <- 0 until discreteImage.values.size) {
        discreteImage.values(i) should be(resampledImage.values(i))
      }

    }
  }


  describe("Resampling a 3D image") {
    val path = getClass().getResource("/3dimage.h5").getPath()
    val discreteImage = ImageIO.read3DScalarImage[Short](new File(path)).get
    val continuousImage = Interpolation.interpolate(discreteImage, 0)

    it("yields the original discrete image") {
      val resampledImage = Resample.sample[Short](continuousImage, discreteImage.domain, 0)
      for (i <- 0 until discreteImage.values.size by 100) {
        discreteImage.values(i) should be(resampledImage.values(i))
      }
    }

  }

}