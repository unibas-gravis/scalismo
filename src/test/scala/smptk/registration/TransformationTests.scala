package smptk
package registration

import org.scalatest.FunSpec
import java.nio.ByteBuffer
import java.io.File
import java.io.IOException
import image.Interpolation
import image.DiscreteImageDomain2D
import io.ImageIO
import image.Image._
import breeze.linalg.DenseVector
import image.Resample
import image.DiscreteImageDomain1D
import image.ContinuousScalarImage1D
import org.scalatest.matchers.ShouldMatchers
import image.Geometry.{CoordVector1D, CoordVector2D}
import smptk.image.Geometry.implicits._

class TransformationTests extends FunSpec with ShouldMatchers {

  describe("A Rotation in 2D") {
    val center = CoordVector2D(2., 3.5)
    val rs = RotationSpace2D(center)
    val phi = scala.math.Pi / 2
    val rotationParams = rs.rotationParametersToParameterVector(phi.toFloat)
    val rotate = rs(rotationParams)
    val pt = CoordVector2D(2., 2.)
    val rotatedPt = rotate(pt)
    it("Rotates a point correctly") {

      (rotatedPt(0)) should be(3.5 plusOrMinus 0.0001)
      (rotatedPt(1)) should be(3.5 plusOrMinus 0.0001)
    }

    it("can be inverted") {
        val identitiyTransform = (rs.inverseTransform(rotationParams).get) compose rotate
    	(identitiyTransform(pt)(0) should be (pt(0) plusOrMinus 0.00001))
        (identitiyTransform(pt)(1) should be (pt(1) plusOrMinus 0.00001))
    }

  }

  describe("A translation in 2D") {
    ignore("translates an image") {
      val discreteImage = ImageIO.read2DScalarImage[Short](new File("/tmp/test.h5")).get
      val continuousImage = Interpolation.interpolate2D(3)(discreteImage)

      val translation = TranslationSpace2D()(DenseVector[Double](10, 0))
      val translatedImg = continuousImage.compose(translation)
      val resampledImage = Resample.sample2D[Short](translatedImg, discreteImage.domain, 0)
      ImageIO.writeImage(resampledImage, new File("/tmp/resampled.h5"))

    }

    describe("composed with a rotation") {

      val ts = TranslationSpace2D()
      val center = CoordVector2D(2., 3.5)
      val rs = RotationSpace2D(center)

      val productSpace = ts.product(rs)

      it("can be composed with a rotation 2D") {
        assert(productSpace.parametersDimensionality === ts.parametersDimensionality + rs.parametersDimensionality)
      }

      val transParams = DenseVector(1., 1.5)
      val translate = ts(transParams)

      val phi = scala.math.Pi / 2
      val rotationParams = rs.rotationParametersToParameterVector(phi.toFloat)
      val rotate = rs(rotationParams)

      val pt = CoordVector2D(2., 2.)
      val rotatedPt = rotate(pt)

      val translatedRotatedPt = translate(rotatedPt)

      val productParams = DenseVector.vertcat(transParams, rotationParams)
      val productTransform = productSpace(productParams)

      it("correctly transforms a point") {
        assert(productTransform(pt) === translatedRotatedPt)
      }
      val productDerivative = (x: CoordVector2D[Double]) =>
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
      val domain = DiscreteImageDomain1D(-50., 1., 100)
      val continuousImage = ContinuousScalarImage1D(domain.isInside, (x: CoordVector1D[Double]) => x * x, (x: CoordVector1D[Double]) => DenseVector(2. * x))

      val translation = TranslationSpace1D()(DenseVector[Double](10))
      val translatedImg = continuousImage.compose(translation)

      assert(translatedImg(-10) === 0)
    }

  }

  describe("A Transformation space") {

  }

}
