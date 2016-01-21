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
package scalismo.image

import breeze.linalg.DenseVector
import scalismo.ScalismoTestSuite
import scalismo.common.{ BoxDomain, PointId, Scalar, ScalarArray }
import scalismo.geometry.IntVector.implicits._
import scalismo.geometry.Point.implicits._
import scalismo.geometry.Vector.implicits._
import scalismo.geometry._
import scalismo.registration.TranslationSpace

import scala.language.implicitConversions
import scala.reflect.ClassTag

class ImageTests extends ScalismoTestSuite {
  implicit def doubleToFloat(d: Double): Float = d.toFloat

  implicit def arrayToScalarArray[A: Scalar: ClassTag](a: Array[A]): ScalarArray[A] = ScalarArray(a)

  describe("A discrete 1D image") {
    it("returns the same points for a 1d index and a coordinate index") {
      val domain = DiscreteImageDomain[_1D](0.0f, 1f, 5)
      val discreteImage = DiscreteScalarImage(domain, Array(3.0, 2.0, 1.5, 1, 0))

      for (i <- 0 until domain.size(0)) {
        assert(discreteImage(i) == discreteImage(i))
      }
    }
  }

  describe("A discrete 2D image") {
    it("returns the same points for a 1d index and a (2d) coordinate index") {
      val domain = DiscreteImageDomain[_2D]((0.0f, 0.0f), (1.0f, 2.0f), (3, 2))
      val discreteImage = DiscreteScalarImage(domain, Array(3.0, 2.0, 1.5, 1.0, 0.0, 4.0))

      for (
        y <- 0 until domain.size(1);
        x <- 0 until domain.size(0)
      ) {
        assert(discreteImage(PointId(y * domain.size(0) + x)) === discreteImage((x, y)))
      }
    }
  }

  describe("A continuous 1D image") {
    it("yields the right values after composing with a translation") {

      val image = DifferentiableScalarImage(BoxDomain(-4.0f, 6.0f),
        (x: Point[_1D]) => Math.sin(x(0).toDouble).toFloat,
        (x: Point[_1D]) => Vector(Math.cos(x(0).toDouble).toFloat))
      val translationTransform = TranslationSpace[_1D].transformForParameters(DenseVector(1f))
      val composedImage = image.compose(translationTransform)
      assert(composedImage.isDefinedAt(-4f) === true)
      assert(composedImage.isDefinedAt(5f) === true)
      assert(composedImage.isDefinedAt(-4.5f) === true)
      assert(composedImage.isDefinedAt(5.5f) === false)
      composedImage(0) should be(image(1) +- 1e-5f)
    }

    it("yields the right values after warping with a translation") {

      val image = DifferentiableScalarImage(BoxDomain(-4.0f, 6.0f),
        (x: Point[_1D]) => Math.sin(x(0).toDouble).toFloat,
        (x: Point[_1D]) => Vector(Math.cos(x(0).toDouble).toFloat))

      val translationTransform = TranslationSpace[_1D].transformForParameters(DenseVector(-1f))

      val warpedImage = image.compose(translationTransform)

      warpedImage.isDefinedAt(-4f) should equal(false)
      warpedImage.isDefinedAt(-3f) should equal(true)
      warpedImage.isDefinedAt(5f) should equal(true)
      warpedImage.isDefinedAt(-3.5f) should equal(false)
      warpedImage.isDefinedAt(5.5f) should equal(true)
      warpedImage.isDefinedAt(6.5f) should equal(true)
      warpedImage.isDefinedAt(7f) should equal(true)

      warpedImage(0) should be(image(-1) +- 1e-5f)
    }
  }

  describe("A continuous 2D image") {
    it("can be translated to a new place") {

      val cImg = ScalarImage(BoxDomain((0.0f, 0.0f), (1.0f, 1.0f)), (_: Point[_2D]) => 1.0)

      def t = TranslationSpace[_2D].transformForParameters(DenseVector(2.0, 2.0))
      val warpedImg = cImg.compose(t)

      warpedImg.isDefinedAt((-0.5f, -0.5f)) should equal(false)
      warpedImg.isDefinedAt((-2.5f, -2.5f)) should equal(false)
      warpedImg.isDefinedAt((-1.5f, -1.5f)) should equal(true)
      warpedImg((-1.5f, -1.5f)) should be(1.0)
    }
  }
}

