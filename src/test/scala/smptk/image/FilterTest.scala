package smptk.image

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import smptk.numerics.Integrator
import smptk.numerics.IntegratorConfiguration
import smptk.numerics.{UniformSampler1D, UniformSampler2D, UniformSampler3D }
import smptk.image.Geometry.{Point1D, Point2D, Point3D}
import smptk.image.Geometry.{CoordVector2D,CoordVector1D,CoordVector3D}
import smptk.image.Image._

class FilterTest extends FunSpec with ShouldMatchers {
  describe("A Gaussian 1D Filter") {
    it("integrates to 1") {
      val gf = GaussianFilter1D(10)
      val integrator = Integrator[CoordVector1D](IntegratorConfiguration(UniformSampler1D(), 10000))
      val value = integrator.integrateScalar((x: Point1D) => Some(gf(x)), gf.support)

      value should be(1. plusOrMinus 0.0001)
    }
  }
  describe("A Gaussian 2D Filter") {
    it("integrates to 1") {
      val gf = GaussianFilter2D(10)
      val integrator =  Integrator[CoordVector2D](IntegratorConfiguration(UniformSampler2D(), 10000))
      val value = integrator.integrateScalar((x: Point2D) => Some(gf(x)), gf.support)

      value should be(1. plusOrMinus 0.0001)
    }
  }
  
  describe("A Gaussian 3D Filter") {
    it("integrates to 1") {
      val gf = GaussianFilter3D(10)
      val integrator =  Integrator[CoordVector3D](IntegratorConfiguration(UniformSampler3D(), 10000))
      val value = integrator.integrateScalar((x: Point3D) => Some(gf(x)), gf.support)

      value should be(1. plusOrMinus 0.0001)
    }
  }
  
  
}