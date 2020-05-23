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
package scalismo

import java.io.File
import java.net.URLDecoder

import breeze.linalg.DenseVector
import scalismo.common.interpolation.BSplineImageInterpolator3D
import scalismo.common.{DifferentiableField, PointId}
import scalismo.geometry.EuclideanVector.implicits._
import scalismo.geometry.IntVector.implicits._
import scalismo.geometry.Point.implicits._
import scalismo.geometry._
import scalismo.image.{StructuredPoints, StructuredPoints1D}
import scalismo.io.{ImageIO, MeshIO}
import scalismo.transformations._

import scala.language.implicitConversions

class TransformationTests extends ScalismoTestSuite {

  implicit def doubleToFloat(d: Double): Float = d.toFloat

  describe("A Transformation") {
    it("can be memoized and yields the same results") {
      val transform = RotationSpace2D(Point(0f, 0f)).transformationForParameters(DenseVector(0.1f))
      val transformMemoized = Transformation.memoize(transform, 100)
      for (x <- 0 until 10; y <- -5 until 5) {
        val p = Point(x, y)
        transform(p) should equal(transformMemoized(p))
      }
    }
  }

  describe("A scaling in 2D") {
    val ss = ScalingSpace2D()
    val params = DenseVector[Double](3.0)
    val scale = ss.transformationForParameters(params)
    val pt = Point(2.0, 1.0)
    val scaledPt = scale(pt)
    it("scales a point correctly") {
      scaledPt(0) should be(6.0 +- 0.0001)
      scaledPt(1) should be(3.0 +- 0.0001)
    }

    it("can be inverted") {
      val identitiyTransform = ss.transformationForParameters(params).inverse compose scale
      identitiyTransform(pt)(0) should be(pt(0) +- 0.00001)
      identitiyTransform(pt)(1) should be(pt(1) +- 0.00001)
    }
  }

  describe("A Rotation in 2D") {
    val center = Point(2.0, 3.5)
    val rs = RotationSpace2D(center)
    val phi = scala.math.Pi / 2
    val rotate = rs.transformationForParameters(DenseVector(phi))
    val pt = Point(2.0, 2.0)
    val rotatedPt = rotate(pt)
    it("rotates a point correctly") {
      rotatedPt(0) should be(3.5 +- 0.0001)
      rotatedPt(1) should be(3.5 +- 0.0001)
    }

    it("can be inverted") {

      val identitiyTransform = rs.transformationForParameters(DenseVector(phi)).inverse compose rotate
      identitiyTransform(pt)(0) should be(pt(0) +- 0.00001)
      identitiyTransform(pt)(1) should be(pt(1) +- 0.00001)
    }
  }

  describe("A translation in 2D") {

    describe("composed with a rotation") {

      val ts = TranslationSpace2D
      val center = Point(2.0, 3.5)
      val rs = RotationSpace2D(center)

      val translation = Translation2D(EuclideanVector2D(1.0, 1.5))

      val phi = scala.math.Pi / 2
      val rotation = Rotation2D(phi, center = Point2D(0, 0))

      val pt = Point(2.0f, 2.0f)
      val rotatedPt = rotation(pt)

      val translatedRotatedPt = translation(rotatedPt)

      val compositeTransformation = CompositeDifferentiableTransformation(translation, rotation)
      it("correctly transforms a point") {

        compositeTransformation(pt) should equal(translatedRotatedPt)
      }

      val productDerivative =
        (x: Point[_2D]) =>
          breeze.linalg.DenseMatrix.horzcat(translation.derivativeWRTParameters(x), rotation.derivativeWRTParameters(x))

      it("differentiates correctly with regard to parameters") {
        compositeTransformation.derivativeWRTParameters(pt) should equal(productDerivative(pt))
      }
      it("correctly differentiates the parametrized transforms") {
        compositeTransformation.derivativeWRTPosition(pt) should equal(
          translation.derivativeWRTPosition(rotation(pt)) * rotation.derivativeWRTPosition(pt)
        )
      }

    }

//    it("translates a 1D image") {
//      val domain = StructuredPoints(-50.0, 1.0, 100)
//      val continuousImage =
//        DifferentiableField(domain.boundingBox, (x: Point[_1D]) => (x * x), (x: Point[_1D]) => EuclideanVector(2f * x))
//
//      val translation = TranslationSpace[_1D].transformationForParameters(DenseVector[Double](10))
//      val translatedImg = continuousImage.compose(translation)
//
//      translatedImg(Point(-10)) should equal(0)
//    }
  }

  describe("In 3D") {

    val path = getClass.getResource("/3dimage.nii").getPath
    val discreteImage = ImageIO.read3DScalarImage[Short](new File(URLDecoder.decode(path, "UTF-8"))).get
    val continuousImage = discreteImage.interpolate(BSplineImageInterpolator3D[Short](0))

    it("translation forth and back of a real dataset yields the same image") {

      val parameterVector = DenseVector[Double](75.0, 50.0, 25.0)
      val translation = TranslationSpace3D.transformationForParameters(parameterVector)
      val inverseTransform = TranslationSpace3D.transformationForParameters(parameterVector).inverse
      val translatedForthBackImg = continuousImage.compose(translation).compose(inverseTransform)

      for (p <- discreteImage.domain.pointSet.points.filter(translatedForthBackImg.isDefinedAt))
        assert(translatedForthBackImg(p) === continuousImage(p))
    }

    it("rotation forth and back of a real dataset yields the same image") {

      val parameterVector = DenseVector[Double](2.0 * Math.PI, 2.0 * Math.PI, 2.0 * Math.PI)
      val origin = discreteImage.domain.origin
      val corner = discreteImage.domain.boundingBox.oppositeCorner
      val center = ((corner - origin) * 0.5).toPoint

      val rotation = RotationSpace3D(center).transformationForParameters(parameterVector)

      val rotatedImage = continuousImage.compose(rotation)

      for (p <- discreteImage.domain.pointSet.points.filter(rotatedImage.isDefinedAt))
        rotatedImage(p) should equal(continuousImage(p))
    }

    val meshPath = URLDecoder.decode(getClass.getResource("/facemesh.stl").getPath, "UTF-8")
    val mesh = MeshIO.readMesh(new File(meshPath)).get

    it("rotation is invertible on meshes") {

      val center = Point(0.0, 0.0, 0.0)
      val parameterVector = DenseVector[Double](Math.PI, -Math.PI / 2.0, -Math.PI)
      val rotation = RotationSpace3D(center).transformationForParameters(parameterVector)
      val inverseRotation = rotation.inverse

      val rotRotMesh = mesh.transform(rotation).transform(inverseRotation)
      rotRotMesh.pointSet.points.zipWithIndex.foreach {
        case (p, i) =>
          val id = PointId(i)
          p(0) should be(mesh.pointSet.point(id)(0) +- 0.000001)
          p(1) should be(mesh.pointSet.point(id)(1) +- 0.000001)
          p(2) should be(mesh.pointSet.point(id)(2) +- 0.000001)
      }
    }
  }

  describe("A 2D rigid transform") {

    val rigidTransformation =
      RotationThenTranslation2D(Rotation(Math.PI / 2.0, Point2D(0, 0)), Translation(EuclideanVector2D(2.0, 5.0)))

    it("correctly transforms a 2D point") {
      val point = Point(1.0, 1.0)
      val correctlyTransformedPoint = Point(1.0, 6.0)
      (rigidTransformation(point) - correctlyTransformedPoint).norm should be < 1e-5
    }

  }
  describe("A 2D similarity transform") {

    val similarityTransformation =
      RotationThenScalingThenTranslation2D(Rotation(Math.PI / 2.0, Point2D(0, 0)),
                                           Scaling(2.0),
                                           Translation(EuclideanVector2D(2.0, 5.0)))

    it("correctly transforms a 2D point") {
      val point = Point(1.0, 1.0)
      val correctlyTransformedPoint = Point(0, 7.0)
      (similarityTransformation(point) - correctlyTransformedPoint).norm should be < 1e-5
    }

  }
}
