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

      val ts = TranslationSpace2D()
      val center = Point(2.0, 3.5)
      val rs = RotationSpace2D(center)

      val productSpace = ProductTransformationSpace(ts, rs)

      it("can be composed with a rotation 2D") {
        productSpace.numberOfParameters should equal(ts.numberOfParameters + rs.numberOfParameters)
      }

      val transParams = DenseVector[Double](1.0, 1.5)
      val translate = ts.transformationForParameters(transParams)

      val phi = scala.math.Pi / 2
      val rotationParams = DenseVector(phi)
      val rotate = rs.transformationForParameters(rotationParams)

      val pt = Point(2.0f, 2.0f)
      val rotatedPt = rotate(pt)

      val translatedRotatedPt = translate(rotatedPt)

      val productParams = DenseVector.vertcat(transParams, rotationParams)
      val productTransform = productSpace.transformationForParameters(productParams)

      it("correctly transforms a point") {
        productTransform(pt) should equal(translatedRotatedPt)
      }

      val productDerivative =
        (x: Point[_2D]) => breeze.linalg.DenseMatrix.horzcat(translate.jacobian(x), rotate.jacobian(x))

      it("differentiates correctly with regard to parameters") {
        productTransform.jacobian(pt) should equal(productDerivative(pt))
      }
      it("correctly differentiates the parametrized transforms") {
        productTransform.derivative(pt) should equal(
          translate.derivative(rotate(pt)) * rotate.derivative(pt)
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
      val translation = TranslationSpace[_3D].transformationForParameters(parameterVector)
      val inverseTransform = TranslationSpace[_3D].transformationForParameters(parameterVector).inverse
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

    it("a rigid transformations yields the same result as the rigid transform composed of rotation and translation") {

      val parameterVector = DenseVector[Double](1.5, 1.0, 3.5, Math.PI, -Math.PI / 2.0, -Math.PI)
      val translationParams = DenseVector(1.5, 1.0, 3.5)
      val rotation =
        RotationSpace3D(Point(0f, 0f, 0f))
          .transformationForParameters(DenseVector(Math.PI, -Math.PI / 2.0, -Math.PI))
      val translation = TranslationSpace[_3D].transformationForParameters(translationParams)

      val rigid = RigidTransformationSpace3D(Point(0, 0, 0)).transformationForParameters(parameterVector)

      val transformedRigid = mesh.transform(rigid)
      val transformedComposed = mesh.transform(translation compose rotation)

      val diffNormMax = transformedRigid.pointSet.points
        .zip(transformedComposed.pointSet.points)
        .map { case (p1, p2) => (p1 - p2).norm }
        .max
      assert(diffNormMax < 0.00001)

    }
  }

  describe("An anisotropic similarity transform") {

    val translationParams = DenseVector(1.0, 2.0, 3.0)
    val rotationParams = DenseVector(0.0, Math.PI / 2.0, Math.PI / 4.0)
    val anisotropScalingParams = DenseVector(2.0, 3.0, 1.0)

    val translation = TranslationSpace3D().transformationForParameters(translationParams)
    val rotation = RotationSpace3D(Point(0, 0, 0)).transformationForParameters(rotationParams)
    val anisotropicScaling = AnisotropicScalingSpace[_3D]().transformationForParameters(anisotropScalingParams)

    val p = Point(1, 1, 1)
    it("Anisotropic scaling is correctly invertible") {
      val inverseScaling = anisotropicScaling.inverse
      assert((inverseScaling(anisotropicScaling(p)) - p).norm < 0.1f)
    }

    val composedTrans = translation compose rotation compose anisotropicScaling
    val combinedParams = DenseVector(translationParams.data ++ rotationParams.data ++ anisotropScalingParams.data)
    val anisotropicSimTrans =
      AnisotropicSimilarityTransformationSpace3D(Point(0, 0, 0)).transformationForParameters(combinedParams)

    val rigidTransformation =
      RigidTransformationSpace3D(Point(0, 0, 0))
        .transformationForParameters(DenseVector(translationParams.data ++ rotationParams.data))

    it("yields the right result as a composition of unit transform") {
      assert((anisotropicSimTrans(p) - composedTrans(p)).norm < 0.1f)
    }

    it("yields the right result as a composition of anisotropic scaling and rigid transform") {
      val composedTrans2 = rigidTransformation compose anisotropicScaling
      assert((anisotropicSimTrans(p) - composedTrans2(p)).norm < 0.1f)
    }

    it("a rigid transformations is correctly invertible") {
      val inverseRigid = rigidTransformation.inverse
      assert((inverseRigid(rigidTransformation(p)) - p).norm < 0.1f)
    }

    it("Anisotropic similarity is correctly invertible") {
      val inverseTrans = anisotropicSimTrans.inverse
      val shouldBeP = inverseTrans(anisotropicSimTrans(p))
      assert((shouldBeP - p).norm < 0.1f)
    }

  }

  describe("A 2D similarity transform") {

    val rigidTransformation =
      RigidTransformation(Translation(EuclideanVector2D(2.0, 5.0)), Rotation(Math.PI / 2.0, Point2D(0, 0)))
    val similarityTransform = SimilarityTransformation(Scaling2D(2.0), rigidTransformation)

    it("correctly transforms a 2D point") {
      val point = Point(1.0, 1.0)
      val correctlyTransformedPoint = Point(2.0, 12.0)
      (similarityTransform(point) - correctlyTransformedPoint).norm should be < 1e-5
    }

  }

  describe("A 3D similarity transform") {

    val rigidTransformation = RigidTransformation(Translation(EuclideanVector3D(2.0, 5.0, 1.0)),
                                                  Rotation(Math.PI / 2.0, 0.0, 0.0, Point3D(0, 0, 0)))
    val similarityTransform = SimilarityTransformation(Scaling3D(2.0), rigidTransformation)

    it("correctly transforms a 3D point") {
      val point = Point(1.0, 1.0, 1.0)
      val correctlyTransformedPoint = Point(2.0, 12.0, 4.0)
      (similarityTransform(point) - correctlyTransformedPoint).norm should be < 1e-5
    }

  }

}
