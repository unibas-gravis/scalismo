package smptk.image

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import smptk.numerics.Integrator
import smptk.numerics.IntegratorConfiguration
import smptk.numerics.{UniformSampler1D, UniformSampler2D, UniformSampler3D }
import smptk.geometry._
import smptk.image.Image._

class FilterTest extends FunSpec with ShouldMatchers {
  describe("A Gaussian 1D Filter") {
    it("integrates to 1") {
      val gf = GaussianFilter1D(10)
      val integrator = Integrator[OneD](IntegratorConfiguration(UniformSampler1D(gf.support), 10000))
      val value = integrator.integrateScalar((x: Point[OneD]) => Some(gf(x)))

      value should be(1.0 plusOrMinus 0.0001)
    }
  }
  describe("A Gaussian 2D Filter") {
    it("integrates to 1") {
      val gf = GaussianFilter2D(10)
      val integrator =  Integrator[TwoD](IntegratorConfiguration(UniformSampler2D(gf.support), 10000))
      val value = integrator.integrateScalar((x: Point[TwoD]) => Some(gf(x)))

      value should be(1.0 plusOrMinus 0.0001)
    }
  }
  
  describe("A Gaussian 3D Filter") {
    it("integrates to 1") {
      val gf = GaussianFilter3D(10)
      val integrator =  Integrator[ThreeD](IntegratorConfiguration(UniformSampler3D(gf.support), 10000))
      val value = integrator.integrateScalar((x: Point[ThreeD]) => Some(gf(x)))

      value should be(1.0 plusOrMinus 0.0001)
    }
  }
  
  
}