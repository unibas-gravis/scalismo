package scalismo.registration

import org.scalatest.{Matchers, FunSpec}
import org.scalatest.matchers.ShouldMatchers
import scalismo.common.BoxDomain
import scalismo.image.DifferentiableScalarImage
import scalismo.geometry._
import scalismo.geometry.Point.implicits._
import scalismo.numerics.{UniformSampler, Integrator}

class MetricTests extends FunSpec with Matchers {

  describe("A mean squares metric (1D)") {
    it("returns 0 if provided twice the same image") {

      val domain = BoxDomain[_1D](0f, 1.0f)
      val img = DifferentiableScalarImage(BoxDomain[_1D](0.0f, 1.0f),
        (x: Point[_1D]) => x * x,
        (x: Point[_1D]) => Vector(2f) * x(0))
      val transSpace = TranslationSpace[_1D]
      val identityTransform = transSpace.transformForParameters(transSpace.identityTransformParameters)
      val sampler = UniformSampler(domain, 1000)
      MeanSquaresMetric(sampler).value(img, img, identityTransform) should be(0.0 +- 0.001)
    }
  }
}