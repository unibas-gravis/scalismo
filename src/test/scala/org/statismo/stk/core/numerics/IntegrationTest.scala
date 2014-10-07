package org.statismo.stk.core.numerics

import scala.language.implicitConversions
import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import org.statismo.stk.core.image.ContinuousScalarImage1D
import org.statismo.stk.core.geometry._
import org.statismo.stk.core.geometry.Point.implicits._
import org.statismo.stk.core.common.BoxedDomain


class IntegrationTest extends FunSpec with ShouldMatchers {

  implicit def doubleToFloat(d: Double) = d.toFloat

  describe("An integration in 1D") {
    it("Correctly integrates x squared on interval [-1,1]") {

      val img = ContinuousScalarImage1D(BoxedDomain[_1D](0f, 1.0f), (x: Point[_1D]) => x * x, Some((x: Point[_1D]) => Vector(2f) * x(0)))

      val domain = BoxedDomain[_1D](-1.0f, 1.0f)
      val integrator = Integrator[_1D](IntegratorConfiguration(UniformSampler1D(domain, 1000)))

      val res = integrator.integrateScalar(img)
      res should be((1.0 / 3.0).toFloat plusOrMinus 0.001)
    }
    it("Correctly integrates sin(x) on interval [-Pi, Pi]") {

      val img = ContinuousScalarImage1D(
        BoxedDomain[_1D](-math.Pi.toFloat, math.Pi.toFloat),
        (x: Point[_1D]) => math.sin(x.toDouble).toFloat,
        Some((x: Point[_1D]) => Vector(-math.cos(x.toDouble).toFloat))
      )

      val domain = BoxedDomain[_1D](-math.Pi.toFloat, math.Pi.toFloat)
      val integrator = Integrator[_1D](IntegratorConfiguration(UniformSampler1D(domain, 1000)))

      val res = integrator.integrateScalar(img)
      res should be(0.0f plusOrMinus 0.001)

    }

    it("Correctly integrates a compact function") {

      val img = ContinuousScalarImage1D(BoxedDomain[_1D](-1.0f, 1.0f), (x: Point[_1D]) => 1.0)

      //Utils.show1D(img, DiscreteImageDomain[_1D](-2., 0.1, 40))

      val region1 = BoxedDomain[_1D](-1.0f, 1.0f)
      val region2 = BoxedDomain[_1D](-8.0f, 8.0f)

      val integrator1 = Integrator(IntegratorConfiguration(UniformSampler1D(region1, 1000)))
      val integrator2 = Integrator(IntegratorConfiguration(UniformSampler1D(region2, 1000)))
      val res1 = integrator1.integrateScalar(img)
      val res2 = integrator2.integrateScalar(img)


      res1 should be(res2 plusOrMinus 0.01)

    }


  }

}