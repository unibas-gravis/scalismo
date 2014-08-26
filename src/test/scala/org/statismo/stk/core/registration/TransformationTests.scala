package org.statismo.stk.core.registration

import scala.language.implicitConversions
import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import java.io.File
import org.statismo.stk.core.image._
import org.statismo.stk.core.io.ImageIO
import breeze.linalg.DenseVector

import org.statismo.stk.core.geometry._
import org.statismo.stk.core.geometry.Point.implicits._
import org.statismo.stk.core.geometry.Vector.implicits._
import org.statismo.stk.core.geometry.Index.implicits._


import org.statismo.stk.core.io.MeshIO

class TransformationTests extends FunSpec with ShouldMatchers {

  //FIXME: this test needs to be seriously overhauled.

  implicit def doubleToFloat(d: Double) = d.toFloat

  org.statismo.stk.core.initialize()

  describe("A scaling in 2D") {
    val ss = ScalingSpace[_2D]
    val params = DenseVector[Float](3.0)
    val scale = ss.transformForParameters(params)
    val pt = Point(2.0, 1.0)
    val scaledPt = scale(pt)
    it("Scales a point correctly") {
      scaledPt(0) should be(6f plusOrMinus 0.0001)
      scaledPt(1) should be(3f plusOrMinus 0.0001)
    }

    it("Can be inverted") {
      val identitiyTransform = ss.transformForParameters(params).inverse compose scale
      identitiyTransform(pt)(0) should be(pt(0) plusOrMinus 0.00001)
      identitiyTransform(pt)(1) should be(pt(1) plusOrMinus 0.00001)
    }
  }

  describe("A Rotation in 2D") {
    val center = Point(2.0, 3.5)
    val rs = RotationSpace[_2D](center)
    val phi = scala.math.Pi / 2
    val rotate = rs.transformForParameters(DenseVector(phi.toFloat))
    val pt = Point(2.0, 2.0)
    val rotatedPt = rotate(pt)
    it("Rotates a point correctly") {
      rotatedPt(0) should be(3.5f plusOrMinus 0.0001)
      rotatedPt(1) should be(3.5f plusOrMinus 0.0001)
    }

    it("can be inverted") {

      val identitiyTransform = (rs.transformForParameters(DenseVector(phi.toFloat)).inverse) compose rotate
      (identitiyTransform(pt)(0) should be(pt(0) plusOrMinus 0.00001))
      (identitiyTransform(pt)(1) should be(pt(1) plusOrMinus 0.00001))

    }
  }

  describe("A translation in 2D") {
    it("translates an image") {
      val testImgUrl = getClass.getResource("/lena.h5").getPath
      val discreteImage = ImageIO.read2DScalarImage[Short](new File(testImgUrl)).get
      val continuousImage = Interpolation.interpolate(discreteImage, 3)

      val translation = TranslationSpace[_2D].transformForParameters(DenseVector[Float](10, 0))
      val translatedImg = continuousImage.compose(translation)
      val resampledImage = Resample.sample[Short](translatedImg, discreteImage.domain, 0)
    }

    describe("composed with a rotation") {

      val ts = TranslationSpace[_2D]
      val center = Point(2.0, 3.5)
      val rs = RotationSpace[_2D](center)

      val productSpace = ts.product(rs)

      it("can be composed with a rotation 2D") {
        assert(productSpace.parametersDimensionality === ts.parametersDimensionality + rs.parametersDimensionality)
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
        assert(productTransform(pt) === translatedRotatedPt)
      }
      val productDerivative = (x: Point[_2D]) =>
        breeze.linalg.DenseMatrix.horzcat(
          ts.takeDerivativeWRTParameters(transParams)(x),
          rs.takeDerivativeWRTParameters(rotationParams)(x))
      it("differentiates correctly with regard to parameters") {
        assert(productSpace.takeDerivativeWRTParameters(productParams)(pt) === productDerivative(pt))
      }
      it("differenetiates correctly the parametrized transforms") {
        assert(productTransform.takeDerivative(pt) === translate.takeDerivative(rotate(pt)) * rotate.takeDerivative(pt))
      }

      //      it("can be inverted") {
      //        val identitiyTransform = (productSpace.transformForParameters(productParams).inverse) compose productTransform
      //        (identitiyTransform(pt)(0) should be(pt(0) plusOrMinus 0.00001f))
      //        (identitiyTransform(pt)(1) should be(pt(1) plusOrMinus 0.00001f))
      //      }
    }

    it("translates a 1D image") {
      val domain = DiscreteImageDomain1D(-50.0f, 1.0f, 100)
      val continuousImage = ContinuousScalarImage1D(domain, (x: Point[_1D]) => x * x, Some((x: Point[_1D]) => Vector(2f * x)))

      val translation = TranslationSpace[_1D].transformForParameters(DenseVector[Float](10))
      val translatedImg = continuousImage.compose(translation)

      assert(translatedImg(-10) === 0)
    }
  }

  describe("In 3D") {

    val path = getClass.getResource("/3dimage.h5").getPath
    val discreteImage = ImageIO.read3DScalarImage[Short](new File(path)).get
    val continuousImage = Interpolation.interpolate(discreteImage, 0)

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
      val extent = discreteImage.domain.extent
      val center = ((extent - origin) * 0.5).toPoint

      val rotation = RotationSpace[_3D](center).transformForParameters(parameterVector)

      // val inverseRotation =  RotationSpace3D(center).inverseTransform(parameterVector).get

      val rotatedImage = continuousImage.compose(rotation)


      for (p <- discreteImage.domain.points.filter(rotatedImage.isDefinedAt)) assert(rotatedImage(p) === continuousImage(p))
    }

    it("rotation works on meshes") {

      val path = getClass.getResource("/facemesh.h5").getPath
      val mesh = MeshIO.readHDF5(new File(path)).get

      val region = mesh.boundingBox
      val origin = region.origin
      val extent = region.extent
      val center = ((extent - origin) * 0.5).toPoint

      val parameterVector = DenseVector[Float](Math.PI, Math.PI, Math.PI)
      
      val rotation = RotationSpace[_3D](center).transformForParameters(parameterVector)

    }
  }

  describe("A Transformation space") {
  }
}
