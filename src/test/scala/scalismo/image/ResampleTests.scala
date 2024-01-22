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

import scalismo.ScalismoTestSuite
import scalismo.common.PointId
import scalismo.common.interpolation.{BSplineImageInterpolator2D, BSplineImageInterpolator3D}
import scalismo.io.ImageIO

import java.io.File
import java.net.URLDecoder

class ResampleTests extends ScalismoTestSuite {

  describe("Resampling a 3D image") {
    val path = getClass.getResource("/3dimage.nii").getPath
    val discreteImage = ImageIO.readNifti[Short](new File(URLDecoder.decode(path, "UTF-8"))).get
    val continuousImage = discreteImage.interpolate(BSplineImageInterpolator3D[Short](0))

    it("yields the original discrete image") {
      val resampledImage = continuousImage.discretize(discreteImage.domain, 0)
      for (i <- 0 until discreteImage.values.size by 100) {
        discreteImage(PointId(i)) should be(resampledImage(PointId(i)))
      }
    }
  }
}
