package smptk
package registration

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import smptk.image.DiscreteImageDomain1D
import image.ContinuousScalarImage1D
import geometry._
import geometry.implicits._
import breeze.linalg.DenseVector
import registration.Metric._

import smptk.numerics.Integrator
import smptk.numerics.IntegratorConfiguration

import smptk.numerics.UniformSampler1D

class IntegrationTest extends FunSpec with ShouldMatchers {

  describe("A mean squares metric (1D)") {
    it("returns 0 if provided twice the same image") {

      val region = DiscreteImageDomain1D(0f, 0.001f, 1000)
      val img = ContinuousScalarImage1D((x: Point[OneD]) => x >= 0 && x <= 1,
        (x: Point[OneD]) => x * x,
        Some((x: Point[OneD]) => DenseVector(2.) * x(0)))
      val integrator = Integrator(IntegratorConfiguration(UniformSampler1D(), region.numberOfPoints))
      MeanSquaresMetric1D()(img, img)(integrator, region) should be(0. plusOrMinus 0.001)
    }
  }
}