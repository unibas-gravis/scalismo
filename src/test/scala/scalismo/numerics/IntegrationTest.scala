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

import scalismo.common.BoxDomain
import scalismo.geometry._
import scalismo.image.{DifferentiableScalarImage, ScalarImage, DiscreteImageDomain}
import scalismo.geometry.Point.implicits._
import scalismo.geometry.Index.implicits._
import scalismo.geometry.Vector.implicits._

import scala.language.implicitConversions
import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers


class IntegrationTest extends FunSpec with ShouldMatchers {

  implicit def doubleToFloat(d: Double) = d.toFloat

  describe("An integration in 1D") {
    it("Correctly integrates x squared on interval [-1,1]") {

      val domain = BoxDomain[_1D](0f, 1.0f)
      val img = DifferentiableScalarImage(domain, (x: Point[_1D]) => x * x, (x: Point[_1D]) => Vector(2f) * x(0))

      val grid = DiscreteImageDomain(domain.origin, domain.extent * (1.0 / 255.0), Index(255))
      val integrator = Integrator[_1D](GridSampler(grid))

      val res = integrator.integrateScalar(img)
      res should be((1.0 / 3.0).toFloat plusOrMinus 0.01)
    }

    it("Correctly integrates sin(x) on interval [-Pi, Pi]") {

      val img = DifferentiableScalarImage(
        BoxDomain[_1D](-math.Pi.toFloat, math.Pi.toFloat),
        (x: Point[_1D]) => math.sin(x.toDouble).toFloat,
        (x: Point[_1D]) => Vector(-math.cos(x.toDouble).toFloat)
      )

      val numPoints = 1000
      val grid = DiscreteImageDomain(Point(-math.Pi.toFloat), Vector(2 * math.Pi.toFloat / numPoints), Index(numPoints))
      val integrator = Integrator(GridSampler(grid))

      val res = integrator.integrateScalar(img)
      res should be(0.0f plusOrMinus 0.01)

    }

    it("Correctly integrates a compact function") {

      val img = ScalarImage(BoxDomain[_1D](-1.0f, 1.0f), (x: Point[_1D]) => 1.0)

      val region1 = BoxDomain[_1D](-1.0f, 1.0f)
      val region2 = BoxDomain[_1D](-8.0f, 8.0f)

      val numPoints = 200
      val grid1 = DiscreteImageDomain(Point(-1.0), Vector(2.0 / numPoints), Index(numPoints))
      val grid2 = DiscreteImageDomain(Point(-8.0), Vector(16.0 / numPoints), Index(numPoints))
      val integrator1 = Integrator(GridSampler(grid1))
      val integrator2 = Integrator(GridSampler(grid2))
      val res1 = integrator1.integrateScalar(img)
      val res2 = integrator2.integrateScalar(img)


      res1 should be(res2 plusOrMinus 0.01)

    }


  }

}