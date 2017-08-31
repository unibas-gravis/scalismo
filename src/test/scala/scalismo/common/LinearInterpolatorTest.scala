/*
 * Copyright University of Basel, Graphics and Vision Research Group
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

package scalismo.common

import scalismo.ScalismoTestSuite
import scalismo.geometry._
import scalismo.image.{ DiscreteImageDomain, DiscreteScalarImage }
import scalismo.common.interpolation.LinearImageInterpolator

class LinearInterpolatorTest extends ScalismoTestSuite {

  describe("A Linear Interpolator") {

    it("returns correct value at grid points.") {

      val dom = DiscreteImageDomain(Point(0, 0, 0), Vector(1, 1, 1), IntVector(2, 2, 2))
      val img = DiscreteScalarImage(dom, (p: Point[_3D]) => p.x)
      val img_interpolated = img.interpolate(LinearImageInterpolator[_3D, Double])
      val id = img.domain.pointId(IntVector(1, 1, 1))
      val point = img.domain.point(id)

      img(id) shouldBe img_interpolated(point)

    }

    it("correctly interpolates in 3D") {

      val dom = DiscreteImageDomain(Point(0, 0, 0), Vector(1, 1, 1), IntVector(2, 2, 2))
      val img = DiscreteScalarImage(dom, (p: Point[_3D]) => p.x)
      val img_interpolated = img.interpolate(LinearImageInterpolator[_3D, Double])
      val point = Point(0.5, 0.5, 0.5)

      img_interpolated(point) shouldBe 0.5

    }

    it("correctly interpolates in 2D") {

      val dom = DiscreteImageDomain(Point(0, 0), Vector(1, 1), IntVector(2, 2))
      val img = DiscreteScalarImage(dom, (p: Point[_2D]) => p.x)
      val img_interpolated = img.interpolate(LinearImageInterpolator[_2D, Double])
      val point = Point(0.5, 0.5)

      img_interpolated(point) shouldBe 0.5

    }

    it("correctly interpolates in 1D") {

      val dom = DiscreteImageDomain(Point(0), Vector(1), IntVector(2))
      val img = DiscreteScalarImage(dom, (p: Point[_1D]) => p.x)
      val img_interpolated = img.interpolate(LinearImageInterpolator[_1D, Double])
      val point = Point(0.5)

      img_interpolated(point) shouldBe 0.5

    }

  }

}
