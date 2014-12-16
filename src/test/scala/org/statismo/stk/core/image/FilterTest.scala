package org.statismo.stk.core.image

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import org.statismo.stk.core.numerics.Integrator
import org.statismo.stk.core.numerics.IntegratorConfiguration
import org.statismo.stk.core.numerics.{UniformSampler1D, UniformSampler2D, UniformSampler3D }
import org.statismo.stk.core.geometry._



class FilterTest extends FunSpec with ShouldMatchers {
  
  describe("A Gaussian 1D Filter") {
    it("integrates to 1") {
      val gf = GaussianFilter1D(10)
      val integrator = Integrator[_1D](IntegratorConfiguration(UniformSampler1D(gf.support, 1000)))
      val value = integrator.integrateScalar((x: Point[_1D]) => Some(gf(x)))

      value should be(1f plusOrMinus 0.01f)
    }
  }
  describe("A Gaussian 2D Filter") {
    it("integrates to 1") {
      val gf = GaussianFilter2D(10)
      val integrator =  Integrator[_2D](IntegratorConfiguration(UniformSampler2D(gf.support, 100 * 100)))
      val value = integrator.integrateScalar((x: Point[_2D]) => Some(gf(x)))

      value should be(1.0f plusOrMinus 0.01f)
    }
  }
  
  describe("A Gaussian 3D Filter") {
    it("integrates to 1") {
      val gf = GaussianFilter3D(10)
      val integrator =  Integrator[_3D](IntegratorConfiguration(UniformSampler3D(gf.support, 100 * 100 * 100)))
      val value = integrator.integrateScalar((x: Point[_3D]) => Some(gf(x)))

      value should be(1.0f plusOrMinus 0.01f)
    }
  }
  
  
}