package smptk.image

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import smptk.numerics.Integrator
import smptk.numerics.IntegratorConfiguration
import smptk.numerics.UniformSampler1D
import smptk.numerics.UniformSampler2D
import smptk.image.Geometry.Point1D
import smptk.image.Geometry.Point2D
import smptk.image.Geometry.CoordVector2D
import smptk.image.Geometry.CoordVector1D
import smptk.image.Image._

class FilterTest extends FunSpec with ShouldMatchers {
  describe("A Gaussian 1D Filter") {
    it("integrates to 1") {
      val gf = GaussianFilter1D(10)
      val integrator = Integrator[CoordVector1D](IntegratorConfiguration(UniformSampler1D(10000)))
      val value = integrator.integrateScalar((x: Point1D) => Some(gf(x)), gf.support)

      value should be(1. plusOrMinus 0.0001)
    }
  }
  describe("A Gaussian 2D Filter") {
    it("integrates to 1") {
      val gf = GaussianFilter2D(10)
      val integrator =  Integrator[CoordVector2D](IntegratorConfiguration(UniformSampler2D(10000)))
      val value = integrator.integrateScalar((x: Point2D) => Some(gf(x)), gf.support)

      value should be(1. plusOrMinus 0.0001)
    }
  }
}