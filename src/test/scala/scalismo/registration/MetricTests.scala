/*
 * Copyright 2015 University of Basel, Graphics and Vision Research Group
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package scalismo.registration

import scalismo.ScalismoTestSuite
import scalismo.common.BoxDomain
import scalismo.geometry.Point.implicits._
import scalismo.geometry._
import scalismo.image.DifferentiableScalarImage
import scalismo.numerics.UniformSampler
import scalismo.utils.Random

class MetricTests extends ScalismoTestSuite {

  implicit val rng = Random(42L)

  describe("A mean squares metric (1D)") {
    it("returns 0 if provided twice the same image") {

      val domain = BoxDomain(0.0, 1.0)
      val img = DifferentiableScalarImage(BoxDomain(0.0, 1.0),
        (x: Point[_1D]) => (x * x).toFloat,
        (x: Point[_1D]) => Vector(2.0) * x(0))
      val transSpace = TranslationSpace[_1D]
      val identityTransform = transSpace.transformForParameters(transSpace.identityTransformParameters)
      val sampler = UniformSampler(domain, 1000)
      MeanSquaresMetric(sampler).value(img, img, identityTransform) should be(0.0 +- 0.001)
    }
  }
}