package smptk.registration

import org.scalatest.FunSpec
import java.nio.ByteBuffer
import java.io.File
import java.io.IOException
import smptk.image.Interpolation
import smptk.image.DiscreteImageDomain2D
import smptk.io.ImageIO
import smptk.image.Image._
import breeze.linalg.DenseVector
import smptk.image.Resample
import smptk.image.DiscreteImageDomain1D
import smptk.image.ContinuousScalarImage1D
import smptk.image.Geometry.CoordVector1D
import org.scalatest.matchers.ShouldMatchers
import smptk.image.Geometry.CoordVector2D

class ImageTest extends FunSpec with ShouldMatchers {

  describe("A Rotation in 2D") {
    val center = CoordVector2D(2f, 3.5f)
    val rs = RotationSpace2D(center)
    val phi = scala.math.Pi / 2
    val rotationParams = rs.rotationParametersToParameterVector(phi.toFloat)
    val rotate = rs(rotationParams)
    val pt = CoordVector2D(2f, 2f)
    val rotatedPt = rotate(pt)
    it("Rotates a point correctly") {

      (rotatedPt(0)) should be(3.5f plusOrMinus 0.0001f)
      (rotatedPt(1)) should be(3.5f plusOrMinus 0.0001f)
    }

    it("can be inverted") {
        val identitiyTransform = (rs.inverseTransform(rotationParams).get) compose rotate
    	(identitiyTransform(pt)(0) should be (pt(0) plusOrMinus 0.00001f))
        (identitiyTransform(pt)(1) should be (pt(1) plusOrMinus 0.00001f))
    }

  }

  describe("A translation in 2D") {
    ignore("translates an image") {
      val discreteImage = ImageIO.read2DScalarImage[Short](new File("/tmp/test.h5")).get
      val continuousImage = Interpolation.interpolate2D(3)(discreteImage)

      val translation = TranslationSpace2D()(DenseVector[Float](10, 0))
      val translatedImg = continuousImage.compose(translation)
      val resampledImage = Resample.sample2D[Short](translatedImg, discreteImage.domain, 0)
      ImageIO.writeImage(resampledImage, new File("/tmp/resampled.h5"))

    }

    describe("composed with a rotation") {

      val ts = TranslationSpace2D()
      val center = CoordVector2D(2f, 3.5f)
      val rs = RotationSpace2D(center)

      val productSpace = ts.product(rs)

      it("can be composed with a rotation 2D") {
        assert(productSpace.parametersDimensionality === ts.parametersDimensionality + rs.parametersDimensionality)
      }

      val transParams = DenseVector(1.f, 1.5f)
      val translate = ts(transParams.map(_.toFloat))

      val phi = scala.math.Pi / 2
      val rotationParams = rs.rotationParametersToParameterVector(phi.toFloat)
      val rotate = rs(rotationParams)

      val pt = CoordVector2D(2f, 2f)
      val rotatedPt = rotate(pt)
      println("rotated point " + rotatedPt)
      val translatedRotatedPt = translate(rotatedPt)

      val productParams = DenseVector.vertcat(transParams, rotationParams)
      val productTransform = productSpace(productParams)

      it("correctly transforms a point") {
        assert(productTransform(pt) === translatedRotatedPt)
      }
      val productDerivative = (x: CoordVector2D[Float]) =>
        breeze.linalg.DenseMatrix.horzcat(
          ts.takeDerivativeWRTParameters(transParams)(x),
          (rs.takeDerivativeWRTParameters(rotationParams)(x)))
      it("differentiates correctly with regard to parameters") {
        assert(productSpace.takeDerivativeWRTParameters(productParams)(pt) === productDerivative(pt))
      }
      it("differenetiates correctly the parametrized transforms") {
        assert(productTransform.takeDerivative(pt) === translate.takeDerivative(rotate(pt)) * rotate.takeDerivative(pt))
      }
      
      it("can be inverted") {
        val identitiyTransform = (productSpace.inverseTransform(productParams).get) compose productTransform
    	(identitiyTransform(pt)(0) should be (pt(0) plusOrMinus 0.00001f))
        (identitiyTransform(pt)(1) should be (pt(1) plusOrMinus 0.00001f))
      }
    }

    it("translates a 1D image") {
      val domain = DiscreteImageDomain1D(-50, 1, 100)
      val continuousImage = ContinuousScalarImage1D(domain.isInside, (x: CoordVector1D[Float]) => x * x, (x: CoordVector1D[Float]) => DenseVector(2 * x))

      val translation = TranslationSpace1D()(DenseVector[Float](10))
      val translatedImg = continuousImage.compose(translation)

      assert(translatedImg(-10) === 0)
    }

  }

  describe("A Transformation space") {

  }

}
