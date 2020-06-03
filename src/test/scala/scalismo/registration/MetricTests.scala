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

import _root_.java.io.File
import java.net.URLDecoder

import breeze.linalg.DenseVector
import scalismo.ScalismoTestSuite
import scalismo.common.{BoxDomain, DifferentiableField}
import scalismo.common.interpolation.BSplineImageInterpolator2D
import scalismo.geometry.Point.implicits._
import scalismo.geometry._
import scalismo.image.{DiscreteImageDomain, DiscreteImageDomain2D, StructuredPoints}
import scalismo.io.ImageIO
import scalismo.numerics.{GridSampler, LBFGSOptimizer, UniformSampler}
import scalismo.utils.Random

class MetricTests extends ScalismoTestSuite {

  implicit val rng = Random(42L)

  describe("A mean squares metric (1D)") {
    it("returns 0 if provided twice the same image") {

      val domain = BoxDomain(0.0, 1.0)
      val img = DifferentiableField(BoxDomain(0.0, 1.0),
                                    (x: Point[_1D]) => (x * x).toFloat,
                                    (x: Point[_1D]) => EuclideanVector(2.0) * x(0))
      val transSpace = TranslationSpace[_1D]
      val sampler = UniformSampler(domain, 1000)
      MeanSquaresMetric(img, img, transSpace, sampler).value(transSpace.identityTransformParameters) should be(
        0.0 +- 0.001
      )
    }
  }

  describe("The mutual information metric") {
    val testImgURL = getClass.getResource("/dm128.vtk").getPath

    val fixedImage = ImageIO.read2DScalarImage[Float](new File(URLDecoder.decode(testImgURL, "UTF-8"))).get
    val fixedImageCont = fixedImage.interpolateDifferentiable(BSplineImageInterpolator2D[Float](3))
    val translationSpace = TranslationSpace[_2D]
    val sampler = GridSampler(DiscreteImageDomain2D(fixedImage.domain.boundingBox, size = IntVector(50, 50)))

    it("has the global minimum where the images are similar") {

      val metric = MutualInformationMetric(fixedImageCont, fixedImage.domain, fixedImageCont, translationSpace, sampler)
      val zeroVec = DenseVector.zeros[Double](translationSpace.parametersDimensionality)

      for (_ <- 0 until 10) {
        val params = DenseVector.rand(translationSpace.parametersDimensionality, rng.breezeRandBasis.gaussian)
        metric.value(params) should be >= metric.value(zeroVec)
      }
    }

    it("goes to a lower value when following the (negative) gradient") {

      val metric = MutualInformationMetric(fixedImageCont, fixedImage.domain, fixedImageCont, translationSpace, sampler)
      for (_ <- 0 until 10) {
        val params = DenseVector.rand(translationSpace.parametersDimensionality, rng.breezeRandBasis.gaussian)

        val origValue = metric.value(params)
        val grad = metric.derivative(params)

        metric.value(params - grad * 1e-5) should be < origValue
      }
    }

    it("recovers the parameters in a registration") {

      val trueParams = DenseVector.ones[Double](translationSpace.parametersDimensionality)
      val movingImage = fixedImageCont.compose(translationSpace.transformForParameters(-trueParams))

      val metric = MutualInformationMetric(fixedImageCont, fixedImage.domain, movingImage, translationSpace, sampler)

      val initialParameters = DenseVector.zeros[Double](translationSpace.parametersDimensionality)
      val regIt =
        Registration(metric, L2Regularizer(translationSpace), 0.0, LBFGSOptimizer(20)).iterator(initialParameters)
      val finalParams = regIt.toIndexedSeq.last.parameters

      breeze.linalg.norm(finalParams - trueParams) should be < 1e-1
    }
  }

  describe("The huber loss metric") {
    val testImgURL = getClass.getResource("/dm128.vtk").getPath

    val fixedImage = ImageIO.read2DScalarImage[Float](new File(URLDecoder.decode(testImgURL, "UTF-8"))).get
    val fixedImageCont = fixedImage.interpolateDifferentiable(BSplineImageInterpolator2D[Float](3))
    val translationSpace = TranslationSpace[_2D]
    val sampler = GridSampler(DiscreteImageDomain2D(fixedImage.domain.boundingBox, size = IntVector(50, 50)))

    it("has the global minimum where the images are similar") {

      val metric = MeanHuberLossMetric(fixedImageCont, fixedImageCont, translationSpace, sampler)
      val zeroVec = DenseVector.zeros[Double](translationSpace.parametersDimensionality)

      for (_ <- 0 until 10) {
        val params = DenseVector.rand(translationSpace.parametersDimensionality, rng.breezeRandBasis.gaussian)
        metric.value(params) should be >= metric.value(zeroVec)
      }
    }

    it("goes to a lower value when following the (negative) gradient") {

      val metric = MeanHuberLossMetric(fixedImageCont, fixedImageCont, translationSpace, sampler)
      for (_ <- 0 until 10) {
        val params = DenseVector.rand(translationSpace.parametersDimensionality, rng.breezeRandBasis.gaussian)

        val origValue = metric.value(params)
        val grad = metric.derivative(params)

        metric.value(params - grad * 1e-1) should be < origValue
      }
    }

    it("recovers the parameters in a registration") {

      val trueParams = DenseVector.ones[Double](translationSpace.parametersDimensionality) * 5.0
      val movingImage = fixedImageCont.compose(translationSpace.transformForParameters(-trueParams))

      val metric = MeanHuberLossMetric(fixedImageCont, movingImage, translationSpace, sampler)

      val initialParameters = DenseVector.zeros[Double](translationSpace.parametersDimensionality)
      val regIt =
        Registration(metric, L2Regularizer(translationSpace), 0.0, LBFGSOptimizer(20)).iterator(initialParameters)
      val regSteps = regIt.toIndexedSeq
      val finalParams = regSteps.last.parameters

      breeze.linalg.norm(finalParams - trueParams) should be < 1e-1
    }
  }

}
