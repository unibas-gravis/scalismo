package smptk.registration

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import smptk.image.DiscreteImageDomain1D
import smptk.image.ContinuousScalarImage1D
import smptk.geometry._
import smptk.geometry.implicits._
import breeze.linalg.DenseVector
import smptk.registration.Metric._
import smptk.numerics.Integrator
import smptk.numerics.IntegratorConfiguration
import smptk.numerics.UniformSampler1D
import smptk.common.BoxedDomain1D

class IntegrationTest extends FunSpec with ShouldMatchers {

  describe("A mean squares metric (1D)") {
    it("returns 0 if provided twice the same image") {

      val domain = BoxedDomain1D(0f, 1.0)
      val img = ContinuousScalarImage1D(BoxedDomain1D(0.0, 1.0),
        (x: Point[OneD]) => x * x,
        Some((x: Point[OneD]) => DenseVector(2.0) * x(0)))
      val integrator = Integrator(IntegratorConfiguration(UniformSampler1D(domain), 1000))
      MeanSquaresMetric1D()(img, img)(integrator) should be(0.0 plusOrMinus 0.001)
    }
  }
}