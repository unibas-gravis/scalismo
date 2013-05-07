package smptk
package registration

import org.scalatest.FunSpec
import java.nio.ByteBuffer
import java.io.File
import java.io.IOException
import image.Interpolation
import image.{ DiscreteImageDomain1D, DiscreteImageDomain2D }
import io.ImageIO
import image.Image._
import breeze.linalg.DenseVector
import image.Resample
import image.ContinuousScalarImage1D
import org.scalatest.matchers.ShouldMatchers
import image.Geometry.{ CoordVector1D, CoordVector2D, CoordVector3D }
import smptk.image.Geometry.implicits._
import smptk.image.Utils
import smptk.io.MeshIO

class TransformationTests extends FunSpec with ShouldMatchers {

  describe("A scaling in 2D") {
    val ss = ScalingSpace2D()
    val params = DenseVector(3.)
    val scale = ss(params)
    val pt = CoordVector2D(2., 1.)
    val scaledPt = scale(pt)
    it("Scales a point correctly") {
      (scaledPt(0)) should be(6. plusOrMinus 0.0001)
      (scaledPt(1)) should be(3. plusOrMinus 0.0001)
    }

    it("Can be inverted") {
      val identitiyTransform = (ss.inverseTransform(params).get) compose scale
      (identitiyTransform(pt)(0) should be(pt(0) plusOrMinus 0.00001))
      (identitiyTransform(pt)(1) should be(pt(1) plusOrMinus 0.00001))
    }
  }

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
      (identitiyTransform(pt)(0) should be(pt(0) plusOrMinus 0.00001))
      (identitiyTransform(pt)(1) should be(pt(1) plusOrMinus 0.00001))
    }

  }

  describe("A translation in 2D") {
    it("translates an image") {
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
        (identitiyTransform(pt)(0) should be(pt(0) plusOrMinus 0.00001f))
        (identitiyTransform(pt)(1) should be(pt(1) plusOrMinus 0.00001f))
      }
    }

    it("translates a 1D image") {
      val domain = DiscreteImageDomain1D(-50., 1., 100)
      val continuousImage = ContinuousScalarImage1D(domain.isInside, (x: CoordVector1D[Double]) => x * x, Some((x: CoordVector1D[Double]) => DenseVector(2. * x)))

      val translation = TranslationSpace1D()(DenseVector[Double](10))
      val translatedImg = continuousImage.compose(translation)

      assert(translatedImg(-10) === 0)
    }

  }

  describe("In 3D") {

    val path = getClass().getResource("/chimp3D-11.h5").getPath()
    val discreteImage = ImageIO.read3DScalarImage[Short](new File(path)).get
    val continuousImage = Interpolation.interpolate3D(0)(discreteImage)

    it("translation forth and back of a real dataset yields the same image") {

      val parameterVector = DenseVector[Double](75., 50., 25.)
      val translation = TranslationSpace3D()(parameterVector)
      val inverseTransform = TranslationSpace3D().inverseTransform(parameterVector).get
      val translatedForthBackImg = continuousImage.backwardWarp(translation, discreteImage.domain.isInside).backwardWarp(inverseTransform, discreteImage.domain.isInside)

      Utils.show3D(translatedForthBackImg, discreteImage.domain)

      for (p <- discreteImage.domain.points.filter(translatedForthBackImg.isDefinedAt)) assert(translatedForthBackImg(p) === continuousImage(p))
    }

    it("rotation forth and back of a real dataset yields the same image") {

      val parameterVector = DenseVector[Double](2. * Math.PI, 2. * Math.PI, 2. * Math.PI)
      val origin = discreteImage.domain.origin
      val extent = discreteImage.domain.extent
      val center = CoordVector3D[Double]((extent(0) - origin(0)) / 2., (extent(1) - origin(1)) / 2., (extent(2) - origin(2)) / 2.)

      val rotation = RotationSpace3D(center)(parameterVector)

      // val inverseRotation =  RotationSpace3D(center).inverseTransform(parameterVector).get

      val rotatedImage = continuousImage.backwardWarp(rotation, discreteImage.domain.isInside)

      Utils.show3D(rotatedImage, discreteImage.domain)

      for (p <- discreteImage.domain.points.filter(rotatedImage.isDefinedAt)) assert(rotatedImage(p) === continuousImage(p))
    }

    it("rotation works on meshes") {

      val path = getClass().getResource("/facemesh.h5").getPath
      val mesh = MeshIO.readHDF5(new File(path)).get
       
      val region = mesh.boundingBox     
      val origin = region.origin
      val extent = region.extent
      val center = CoordVector3D[Double]((extent(0) - origin(0)) / 2., (extent(1) - origin(1)) / 2., (extent(2) - origin(2)) / 2.)

      val parameterVector = DenseVector[Double](Math.PI, Math.PI, Math.PI)
      
      val rotation = RotationSpace3D(center)(parameterVector)
      val vtkpd = Utils.meshToVTKMesh(mesh compose rotation)
      Utils.showVTK(vtkpd)
    }

  }

  describe("A Transformation space") {

  }

}
