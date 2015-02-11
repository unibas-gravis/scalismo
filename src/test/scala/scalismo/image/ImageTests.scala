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

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import breeze.linalg.DenseVector
import scalismo.common.BoxDomain
import scalismo.io.ImageIO
import scalismo.registration.TranslationSpace
import scala.language.implicitConversions
import java.io.File
import scalismo.geometry._
import scalismo.geometry.Point.implicits._
import scalismo.geometry.Vector.implicits._
import scalismo.geometry.Index.implicits._

import spire.math.Numeric._

class ImageTests extends FunSpec with ShouldMatchers {
  implicit def doubleToFloat(d: Double) = d.toFloat

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
        assert(discreteImage(y * domain.size(0) + x) === discreteImage((x, y)))
      }
    }
  }

  describe("A continuous 1D image") {
    it("yields the right values after composing with a translation") {

      val image = DifferentiableScalarImage(BoxDomain[_1D](-4.0f, 6.0f),
        (x: Point[_1D]) => Math.sin(x(0).toDouble).toFloat,
        (x: Point[_1D]) => Vector(Math.cos(x(0).toDouble).toFloat))
      val translationTransform = TranslationSpace[_1D].transformForParameters(DenseVector(1f))
      val composedImage = image.compose(translationTransform)
      assert(composedImage.isDefinedAt(-4f) === true)
      assert(composedImage.isDefinedAt(5f) === true)
      assert(composedImage.isDefinedAt(-4.5f) === true)
      assert(composedImage.isDefinedAt(5.5f) === false)
      composedImage(0) should be(image(1) plusOrMinus 1e-5f)
    }

    it("yields the right values after warping with a translation") {

      val image = DifferentiableScalarImage(BoxDomain[_1D](-4.0f, 6.0f),
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

      warpedImage(0) should be(image(-1) plusOrMinus 1e-5f)
    }
  }

  describe("A continuous 2Dimage") {
    it("can be translated to a new place") {

      val cImg = ScalarImage(BoxDomain[_2D]((0.0f, 0.0f), (1.0f, 1.0f)), (_ : Point[_2D]) => 1.0)

      def t = TranslationSpace[_2D].transformForParameters(DenseVector(2.0, 2.0))
      val warpedImg = cImg.compose(t)

      warpedImg.isDefinedAt((-0.5f, -0.5f)) should equal(false)
      warpedImg.isDefinedAt((-2.5f, -2.5f)) should equal(false)
      warpedImg.isDefinedAt((-1.5f, -1.5f)) should equal(true)
      warpedImg((-1.5f, -1.5f)) should be(1.0)
    }
  }
}

class DomainTest extends FunSpec with ShouldMatchers {
  describe("a domain") {
    it("correctly reports the number of points") {
      val domain = DiscreteImageDomain[_2D]((0.0f, 0.0f), (1.0f, 2.0f), (42, 49))
      assert(domain.numberOfPoints === domain.points.size)
    }
  }

  describe("a discrete domain in 2d") {
    it("correctly maps a coordinate index to a linearIndex") {
      val domain = DiscreteImageDomain[_2D]((0.0f, 0.0f), (1.0f, 2.0f), (42, 49))
      assert(domain.pointId((40, 34)) === 40 + 34 * domain.size(0))
    }

    it("can correclty map a linear index to an index and back") {
      val domain = DiscreteImageDomain[_2D]((1.0f, 2.0f), (2.0f, 1.0f), (42, 49))
      val idx = Index(5, 7)
      val recIdx = domain.index(domain.pointId(idx))
      assert(recIdx === idx)
    }

    it("domains with same parameters yield to the same anisotropic simlarity transform ") {
      val domain1 = DiscreteImageDomain[_2D]((1.0f, 2.0f), (2.0f, 1.0f), (42, 49))
      val domain2 = DiscreteImageDomain[_2D]((1.0f, 2.0f), (2.0f, 1.0f), (42, 49))
      assert(domain1.indexToPhysicalCoordinateTransform == domain2.indexToPhysicalCoordinateTransform)
    }
    it("equality works for image domains ") {
      val domain1 = DiscreteImageDomain[_2D]((1.0f, 2.0f), (2.0f, 1.0f), (42, 49))
      val domain2 = DiscreteImageDomain[_2D]((1.0f, 2.0f), (2.0f, 1.0f), (42, 49))
      assert(domain1 == domain2)
    }

  }

  describe("a discrete domain in 3d") {
    it("correctly maps a coordinate index to a linearIndex") {
      val domain = DiscreteImageDomain[_3D]((0.0f, 0.0f, 0.0f), (1.0f, 2.0f, 3.0f), (42, 49, 65))
      assert(domain.pointId((40, 34, 15)) === 40 + 34 * domain.size(0) + 15 * domain.size(0) * domain.size(1))
    }

    it("can correclty map a linear index to an index and back") {
      val domain = DiscreteImageDomain[_3D]((0.0f, 0.0f, 0.0f), (1.0f, 2.0f, 3.0f), (42, 49, 65))

      val idx = Index(5, 3, 7)
      val recIdx = domain.index(domain.pointId(idx))
      assert(recIdx === idx)
    }

    it("domains with same parameters yield to the same anisotropic simlarity transform ") {
      val domain1 = DiscreteImageDomain[_3D]((1.0f, 2.0f, 3f), (2.0f, 1.0f, 0f), (42, 49, 74))
      val domain2 = DiscreteImageDomain[_3D]((1.0f, 2.0f, 3f), (2.0f, 1.0f, 0f), (42, 49, 74))
      assert(domain1.indexToPhysicalCoordinateTransform == domain2.indexToPhysicalCoordinateTransform)
    }
    it("equality works for image domains ") {
      val domain1 = DiscreteImageDomain[_3D]((1.0f, 2.0f, 3f), (2.0f, 1.0f, 1f), (42, 49, 74))
      val domain2 = DiscreteImageDomain[_3D]((1.0f, 2.0f, 3f), (2.0f, 1.0f, 1f), (42, 49, 74))
      assert(domain1 == domain2)
    }

    it("the anisotropic similarity transform defining the donmain is correct and invertible") {
      val pathH5 = getClass.getResource("/3dimage.nii").getPath
      val origImg = ImageIO.read3DScalarImage[Short](new File(pathH5)).get

      val trans = origImg.domain.indexToPhysicalCoordinateTransform
      val inverseTrans = trans.inverse

      assert((trans(Point(0, 0, 0)) - origImg.domain.origin).norm < 0.1f)
      assert(inverseTrans(origImg.domain.origin).toVector.norm < 0.1f)

      (trans(Point(origImg.domain.size(0) , origImg.domain.size(1) , origImg.domain.size(2) )) - origImg.domain.imageBox.oppositeCorner).norm should be <(0.1)
      (inverseTrans(origImg.domain.imageBox.oppositeCorner) - Point(origImg.domain.size(0), origImg.domain.size(1) , origImg.domain.size(2) )).norm should be <(0.1)
    }

  }
}