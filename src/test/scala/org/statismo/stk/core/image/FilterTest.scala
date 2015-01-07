package org.statismo.stk.core.image

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import org.statismo.stk.core.numerics.{GridSampler, UniformSampler, Integrator, IntegratorConfiguration}
import org.statismo.stk.core.geometry._



class FilterTest extends FunSpec with ShouldMatchers {
  
  describe("A Gaussian 1D Filter") {
    it("integrates to 1") {
      val gf = GaussianFilter1D(10)

      val grid = DiscreteImageDomain(gf.support.origin, (gf.support.extent - gf.support.origin) * (1.0 / 1000), Index(1000))

      val integrator = Integrator[_1D](IntegratorConfiguration(GridSampler(grid)))
      val value = integrator.integrateScalar((x: Point[_1D]) => Some(gf(x)))

      value should be(1f plusOrMinus 0.01f)
    }
  }
  describe("A Gaussian 2D Filter") {
    it("integrates to 1") {
      val gf = GaussianFilter2D(10)

      val grid = DiscreteImageDomain(gf.support.origin, (gf.support.extent - gf.support.origin) * (1.0 / 200), Index(200, 200))
      val integrator = Integrator[_2D](IntegratorConfiguration(GridSampler(grid)))

      val value = integrator.integrateScalar((x: Point[_2D]) => Some(gf(x)))

      value should be(1.0f plusOrMinus 0.01f)
    }
  }
  
  describe("A Gaussian 3D Filter") {
    it("integrates to 1") {
      val gf = GaussianFilter3D(10)
      val grid = DiscreteImageDomain(gf.support.origin, (gf.support.extent - gf.support.origin) * (1.0 / 200), Index(200, 200, 200))

      val integrator =  Integrator[_3D](IntegratorConfiguration(GridSampler(grid)))
      val value = integrator.integrateScalar((x: Point[_3D]) => Some(gf(x)))

      value should be(1.0f plusOrMinus 0.01f)
    }
  }
  
  
}