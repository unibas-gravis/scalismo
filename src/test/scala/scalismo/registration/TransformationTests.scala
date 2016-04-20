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

import java.io.File

import breeze.linalg.DenseVector
import scalismo.ScalismoTestSuite
import scalismo.common.PointId
import scalismo.geometry.IntVector.implicits._
import scalismo.geometry.Point.implicits._
import scalismo.geometry.Vector.implicits._
import scalismo.geometry._
import scalismo.image.{ DifferentiableScalarImage, DiscreteImageDomain }
import scalismo.io.{ ImageIO, MeshIO }

import scala.language.implicitConversions

class TransformationTests extends ScalismoTestSuite {

  implicit def doubleToFloat(d: Double): Float = d.toFloat

  describe("A Transformation") {
    it("can be memoized and yields the same results") {
      val transform = RotationSpace[_2D](Point(0f, 0f)).transformForParameters(DenseVector(0.1f))
      val transformMemoized = Transformation.memoize(transform, 100)
      for (x <- 0 until 10; y <- -5 until 5) {
        val p = Point(x, y)
        transform(p) should equal(transformMemoized(p))
      }
    }
  }

  describe("A scaling in 2D") {
    val ss = ScalingSpace[_2D]
    val params = DenseVector[Float](3.0)
    val scale = ss.transformForParameters(params)
    val pt = Point(2.0, 1.0)
    val scaledPt = scale(pt)
    it("scales a point correctly") {
      scaledPt(0) should be(6f +- 0.0001)
      scaledPt(1) should be(3f +- 0.0001)
    }

    it("can be inverted") {
      val identitiyTransform = ss.transformForParameters(params).inverse compose scale
      identitiyTransform(pt)(0) should be(pt(0) +- 0.00001)
      identitiyTransform(pt)(1) should be(pt(1) +- 0.00001)
    }
  }

  describe("A Rotation in 2D") {
    val center = Point(2.0, 3.5)
    val rs = RotationSpace[_2D](center)
    val phi = scala.math.Pi / 2
    val rotate = rs.transformForParameters(DenseVector(phi.toFloat))
    val pt = Point(2.0, 2.0)
    val rotatedPt = rotate(pt)
    it("rotates a point correctly") {
      rotatedPt(0) should be(3.5f +- 0.0001)
      rotatedPt(1) should be(3.5f +- 0.0001)
    }

    it("can be inverted") {

      val identitiyTransform = rs.transformForParameters(DenseVector(phi.toFloat)).inverse compose rotate
      identitiyTransform(pt)(0) should be(pt(0) +- 0.00001)
      identitiyTransform(pt)(1) should be(pt(1) +- 0.00001)
    }
  }

  describe("A translation in 2D") {

    describe("composed with a rotation") {

      val ts = TranslationSpace[_2D]
      val center = Point(2.0, 3.5)
      val rs = RotationSpace[_2D](center)

      val productSpace = ts.product(rs)

      it("can be composed with a rotation 2D") {
        productSpace.parametersDimensionality should equal(ts.parametersDimensionality + rs.parametersDimensionality)
      }

      val transParams = DenseVector[Float](1.0, 1.5)
      val translate = ts.transformForParameters(transParams)

      val phi = scala.math.Pi / 2
      val rotationParams = DenseVector(phi.toFloat)
      val rotate = rs.transformForParameters(rotationParams)

      val pt = Point(2.0f, 2.0f)
      val rotatedPt = rotate(pt)

      val translatedRotatedPt = translate(rotatedPt)

      val productParams = DenseVector.vertcat(transParams, rotationParams)
      val productTransform = productSpace.transformForParameters(productParams)

      it("correctly transforms a point") {
        productTransform(pt) should equal(translatedRotatedPt)
      }
      val productDerivative = (x: Point[_2D]) =>
        breeze.linalg.DenseMatrix.horzcat(
          ts.takeDerivativeWRTParameters(transParams)(x),
          rs.takeDerivativeWRTParameters(rotationParams)(x))
      it("differentiates correctly with regard to parameters") {
        productSpace.takeDerivativeWRTParameters(productParams)(pt) should equal(productDerivative(pt))
      }
      it("correctly differentiates the parametrized transforms") {
        productTransform.takeDerivative(pt) should equal(translate.takeDerivative(rotate(pt)) * rotate.takeDerivative(pt))
      }

    }

    it("translates a 1D image") {
      val domain = DiscreteImageDomain[_1D](-50.0f, 1.0f, 100)
      val continuousImage = DifferentiableScalarImage(domain.boundingBox, (x: Point[_1D]) => x * x, (x: Point[_1D]) => Vector(2f * x))

      val translation = TranslationSpace[_1D].transformForParameters(DenseVector[Float](10))
      val translatedImg = continuousImage.compose(translation)

      translatedImg(-10) should equal(0)
    }
  }

  describe("In 3D") {

    val path = getClass.getResource("/3dimage.nii").getPath
    val discreteImage = ImageIO.read3DScalarImage[Short](new File(path)).get
    val continuousImage = discreteImage.interpolate(0)

    it("translation forth and back of a real dataset yields the same image") {

      val parameterVector = DenseVector[Float](75.0, 50.0, 25.0)
      val translation = TranslationSpace[_3D].transformForParameters(parameterVector)
      val inverseTransform = TranslationSpace[_3D].transformForParameters(parameterVector).inverse
      val translatedForthBackImg = continuousImage.compose(translation).compose(inverseTransform)

      for (p <- discreteImage.domain.points.filter(translatedForthBackImg.isDefinedAt)) assert(translatedForthBackImg(p) === continuousImage(p))
    }

    it("rotation forth and back of a real dataset yields the same image") {

      val parameterVector = DenseVector[Float](2.0 * Math.PI, 2.0 * Math.PI, 2.0 * Math.PI)
      val origin = discreteImage.domain.origin
      val corner = discreteImage.domain.boundingBox.oppositeCorner
      val center = ((corner - origin) * 0.5).toPoint

      val rotation = RotationSpace[_3D](center).transformForParameters(parameterVector)

      val rotatedImage = continuousImage.compose(rotation)

      for (p <- discreteImage.domain.points.filter(rotatedImage.isDefinedAt)) rotatedImage(p) should equal(continuousImage(p))
    }

    val mesh = MeshIO.readMesh(new File(getClass.getResource("/facemesh.stl").getPath)).get

    it("rotation is invertible on meshes") {

      val center = Point(0f, 0f, 0f)
      val parameterVector = DenseVector[Float](Math.PI, -Math.PI / 2.0, -Math.PI)
      val rotation = RotationSpace[_3D](center).transformForParameters(parameterVector)
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

    it("a rigid transformation yields the same result as the composition of rotation and translation") {

      val parameterVector = DenseVector[Float](1.5, 1.0, 3.5, Math.PI, -Math.PI / 2.0, -Math.PI)
      val translationParams = DenseVector(1.5f, 1.0f, 3.5f)
      val rotation = RotationSpace[_3D](Point(0f, 0f, 0f)).transformForParameters(DenseVector(Math.PI, -Math.PI / 2.0, -Math.PI))
      val translation = TranslationSpace[_3D].transformForParameters(translationParams)

      val composed = translation compose rotation
      val rigid = RigidTransformationSpace[_3D]().transformForParameters(parameterVector)

      val transformedRigid = mesh.transform(rigid)
      val transformedComposed = mesh.transform(rigid)

      val diffNormMax = transformedRigid.pointSet.points.zip(transformedComposed.pointSet.points).map { case (p1, p2) => (p1 - p2).norm }.max
      assert(diffNormMax < 0.00001)

    }
  }

  describe("An anisotropic similarity transform") {

    val translationParams = DenseVector(1f, 2f, 3f)
    val rotationParams = DenseVector(0f, Math.PI.toFloat / 2f, Math.PI.toFloat / 4f)
    val anisotropScalingParams = DenseVector(2f, 3f, 1f)

    val translation = TranslationSpace[_3D].transformForParameters(translationParams)
    val rotation = RotationSpace[_3D](Point(0, 0, 0)).transformForParameters(rotationParams)
    val anisotropicScaling = AnisotropicScalingSpace[_3D]().transformForParameters(anisotropScalingParams)

    val p = Point(1, 1, 1)
    it("Anisotropic scaling is correctly invertible") {
      val inverseScaling = anisotropicScaling.inverse
      assert((inverseScaling(anisotropicScaling(p)) - p).norm < 0.1f)
    }

    val composedTrans = translation compose rotation compose anisotropicScaling
    val combinedParams = DenseVector(translationParams.data ++ rotationParams.data ++ anisotropScalingParams.data)
    val anisotropicSimTrans = AnisotropicSimilarityTransformationSpace[_3D](Point(0, 0, 0)).transformForParameters(combinedParams)

    val rigidTransformation = RigidTransformationSpace[_3D]().transformForParameters(DenseVector(translationParams.data ++ rotationParams.data))

    it("yields the right result as a composition of unit transform") {
      assert((anisotropicSimTrans(p) - composedTrans(p)).norm < 0.1f)
    }

    it("yields the right result as a composition of anisotropic scaling and rigid transform") {
      val composedTrans2 = rigidTransformation compose anisotropicScaling
      assert((anisotropicSimTrans(p) - composedTrans2(p)).norm < 0.1f)
    }

    it("a rigid transformation is correctly invertible") {
      val inverseRigid = rigidTransformation.inverse
      assert((inverseRigid(rigidTransformation(p)) - p).norm < 0.1f)
    }

    it("Anisotropic similarity is correctly invertible") {
      val inverseTrans = anisotropicSimTrans.inverse
      val shouldBeP = inverseTrans(anisotropicSimTrans(p))
      assert((shouldBeP - p).norm < 0.1f)
    }

  }
}
