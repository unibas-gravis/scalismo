package smptk
package registration

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import smptk.image.DiscreteImageDomain1D
import image.ContinuousScalarImage1D
import image.Geometry._
import smptk.image.Geometry.implicits._
import breeze.linalg.DenseVector
import registration.Metric._

class IntegrationTest extends FunSpec with ShouldMatchers {

  describe("A mean squares metric (1D)") {
    it("returns 0 if provided twice the same image") {

      val region = DiscreteImageDomain1D(0f, 0.001f, 1000)
      val img = ContinuousScalarImage1D((x: Point1D) => x >= 0 && x <= 1,
        (x: Point1D) => x * x,
        Some((x: Point1D) => DenseVector(2.) * x(0)))

      MeanSquaresMetric1D(MeanSquaresMetricConfiguration())(img, img)(region)._1 should be(0. plusOrMinus 0.001)
    }
  }
}