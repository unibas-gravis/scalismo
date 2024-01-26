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

import breeze.linalg.DenseVector
import scalismo.ScalismoTestSuite
import scalismo.common.interpolation.BSplineImageInterpolator2D
import scalismo.common.{BoxDomain, DifferentiableField}
import scalismo.geometry.*
import scalismo.geometry.Point.implicits.*
import scalismo.image.{DiscreteImageDomain, DiscreteImageDomain2D, StructuredPoints}
import scalismo.io.ImageIO
import scalismo.numerics.{GridSampler, LBFGSOptimizer, UniformSampler}
import scalismo.transformations.{TranslationSpace, TranslationSpace1D, TranslationSpace2D}
import scalismo.utils.Random

import _root_.java.io.File
import java.net.URLDecoder

class MetricTests extends ScalismoTestSuite {

  implicit val rng: Random = Random(42L)

  describe("A mean squares metric (1D)") {
    it("returns 0 if provided twice the same image") {

      val domain = BoxDomain(0.0, 1.0)
      val img = DifferentiableField(BoxDomain(0.0, 1.0),
                                    (x: Point[_1D]) => (x * x).toFloat,
                                    (x: Point[_1D]) => EuclideanVector(2.0) * x(0)
      )
      val transSpace = TranslationSpace1D
      val sampler = UniformSampler(domain, 1000)
      MeanSquaresMetric(img, img, transSpace, sampler).value(transSpace.identityTransformation.parameters) should be(
        0.0 +- 0.001
      )
    }
  }
}
