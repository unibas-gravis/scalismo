package org.statismo.stk.core.registration

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import org.statismo.stk.core.image.ContinuousScalarImage1D
import org.statismo.stk.core.geometry._
import org.statismo.stk.core.geometry.Point.implicits._
import org.statismo.stk.core.numerics.Integrator
import org.statismo.stk.core.numerics.IntegratorConfiguration
import org.statismo.stk.core.numerics.UniformSampler1D
import org.statismo.stk.core.common.BoxedDomain

class MetricTests extends FunSpec with ShouldMatchers {

  describe("A mean squares metric (1D)") {
    it("returns 0 if provided twice the same image") {

      val domain = BoxedDomain[_1D](0f, 1.0f)
      val img = ContinuousScalarImage1D(BoxedDomain[_1D](0.0f, 1.0f),
        (x: Point[_1D]) => x * x,
        Some((x: Point[_1D]) => Vector(2f) * x(0)))
      val identityTransform = TranslationSpace1D()(TranslationSpace1D().identityTransformParameters)
      val integrator = Integrator(IntegratorConfiguration(UniformSampler1D(domain, 1000)))
      MeanSquaresMetric1D(integrator)(img, img,identityTransform) should be(0.0 plusOrMinus 0.001)
    }
  }
}