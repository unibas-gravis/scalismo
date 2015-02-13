/*
 * Copyright 2015 University of Basel, Graphics and Vision Research Group
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package scalismo.image

import org.scalatest.{ Matchers, FunSpec }
import org.scalatest.matchers.ShouldMatchers
import java.io.File

import scalismo.io.ImageIO

class ResampleTests extends FunSpec with Matchers {
  scalismo.initialize()

  describe("Resampling a 2D image") {

    val testImgUrl = getClass.getResource("/lena.vtk").getPath
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
    val path = getClass.getResource("/3dimage.nii").getPath
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