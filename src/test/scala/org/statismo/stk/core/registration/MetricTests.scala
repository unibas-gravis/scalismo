package org.statismo.stk.core.registration

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import org.statismo.stk.core.image.DiscreteImageDomain1D
import org.statismo.stk.core.image.ContinuousScalarImage1D
import org.statismo.stk.core.geometry._
import org.statismo.stk.core.geometry.implicits._
import breeze.linalg.DenseVector
import org.statismo.stk.core.registration.Metric._
import org.statismo.stk.core.numerics.Integrator
import org.statismo.stk.core.numerics.IntegratorConfiguration
import org.statismo.stk.core.numerics.UniformSampler1D
import org.statismo.stk.core.common.BoxedDomain1D

class IntegrationTest extends FunSpec with ShouldMatchers {

  describe("A mean squares metric (1D)") {
    it("returns 0 if provided twice the same image") {

      val domain = BoxedDomain1D(0f, 1.0)
      val img = ContinuousScalarImage1D(BoxedDomain1D(0.0, 1.0),
        (x: Point[OneD]) => x * x,
        Some((x: Point[OneD]) => Vector1D(2f) * x(0)))
      val integrator = Integrator(IntegratorConfiguration(UniformSampler1D(domain), 1000))
      MeanSquaresMetric1D()(img, img)(integrator) should be(0.0 plusOrMinus 0.001)
    }
  }
}