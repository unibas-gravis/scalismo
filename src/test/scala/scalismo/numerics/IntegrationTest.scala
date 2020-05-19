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
package scalismo.numerics

import scalismo.ScalismoTestSuite
import scalismo.common.{BoxDomain, BoxDomain1D, DifferentiableField, Field}
import scalismo.geometry.Point.implicits._
import scalismo.geometry._
import scalismo.image.{DiscreteImageDomain, DiscreteImageDomain1D, StructuredPoints}
import scalismo.utils.Random

import scala.language.implicitConversions

class IntegrationTest extends ScalismoTestSuite {

  implicit val rng = Random(42L)

  implicit def doubleToFloat(d: Double): Float = d.toFloat

  describe("An integration in 1D") {
    it("Correctly integrates x squared on interval [-1,1]") {

      val domain = BoxDomain1D(Point1D(0.0), Point1D(1.0))
      val img =
        DifferentiableField(domain, (x: Point[_1D]) => x * x, (x: Point[_1D]) => EuclideanVector(2f) * x(0))

      val grid = DiscreteImageDomain1D(domain.origin, domain.extent * (1.0 / 255.0), IntVector(255))
      val integrator = Integrator[_1D](GridSampler(grid))

      val res = integrator.integrateScalar(img)
      res should be((1.0 / 3.0) +- 0.01)
    }

    it("Correctly integrates sin(x) on interval [-Pi, Pi]") {

      val img = DifferentiableField(
        BoxDomain(Point1D(-math.Pi.toFloat), Point1D(math.Pi.toFloat)),
        (x: Point[_1D]) => math.sin(x.toDouble).toFloat,
        (x: Point[_1D]) => EuclideanVector(-math.cos(x.toDouble).toFloat)
      )

      val numPoints = 1000
      val grid = DiscreteImageDomain1D(Point(-math.Pi.toFloat),
                                       EuclideanVector(2 * math.Pi.toFloat / numPoints),
                                       IntVector(numPoints))
      val integrator = Integrator(GridSampler(grid))

      val res = integrator.integrateScalar(img)
      res should be(0.0f +- 0.01)

    }

    it("Correctly integrates a compact function") {

      val img = Field(BoxDomain1D(-1.0f, 1.0f), (x: Point[_1D]) => 1.0)

      val numPoints = 200
      val grid1 = DiscreteImageDomain1D(Point(-1.0), EuclideanVector(2.0 / numPoints), IntVector(numPoints))
      val grid2 = DiscreteImageDomain1D(Point(-8.0), EuclideanVector(16.0 / numPoints), IntVector(numPoints))
      val integrator1 = Integrator(GridSampler(grid1))
      val integrator2 = Integrator(GridSampler(grid2))
      val res1 = integrator1.integrateScalar(img)
      val res2 = integrator2.integrateScalar(img)

      res1 should be(res2 +- 0.01)

    }

  }

}
