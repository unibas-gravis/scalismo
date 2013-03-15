package smptk
package registration

import org.scalatest.FunSpec
import java.nio.ByteBuffer
import java.io.File
import java.io.IOException
import smptk.image.DiscreteImageDomain2D
import smptk.image.Geometry.CoordVector2D
import breeze.linalg.DenseVector
import org.scalatest.matchers.ShouldMatchers
import smptk.image.Geometry._
import smptk.image.Geometry.implicits._
import breeze.plot.Figure
import breeze.plot._
import smptk.io.ImageIO
import smptk.image.Interpolation
import smptk.image.Image._

class ImageTest extends FunSpec with ShouldMatchers {
  describe("A 2D rigid landmark based registration") {
    it("can retrieve correct parameters") {
      val points = IndexedSeq(CoordVector2D(0f, 0f), CoordVector2D(1f, 4f), CoordVector2D(2f, 0f))

      val c = CoordVector2D(1f, 4/3f) 
      for (angle <- (1 until 16).map(i => (math.Pi).toFloat / i)) { 
	      val rotationParams = DenseVector( -angle )
	      val transParams = DenseVector[Float](1f, 1.5f)
	      val productParams = DenseVector.vertcat(transParams, rotationParams)
	
	
	      val productSpace = RigidTransformationSpace2D(c)
	
	      val transformedPoints = points.map((pt: CoordVector2D[Float]) => productSpace(productParams)(pt))
	
	      val (optiTransfrom, p) = LandmarkRegistration.rigid2DLandmarkRegistration(points.zip(transformedPoints), c)
	       
	      val alignedPoints = points.map((pt: CoordVector2D[Float]) => optiTransfrom(pt))
	      
	      
	      (transformedPoints(0)(0) should be(alignedPoints(0)(0) plusOrMinus 0.0001f))
	      (transformedPoints(0)(1) should be(alignedPoints(0)(1) plusOrMinus 0.0001f))
	      (transformedPoints(1)(0) should be(alignedPoints(1)(0) plusOrMinus 0.0001f))
	      (transformedPoints(1)(1) should be(alignedPoints(1)(1) plusOrMinus 0.0001f))
	      (transformedPoints(2)(0) should be(alignedPoints(2)(0) plusOrMinus 0.0001f))
	      (transformedPoints(2)(1) should be(alignedPoints(2)(1) plusOrMinus 0.0001f))
      }
    }
  }
  
  
  describe("A 2D image registration") {
    it("Recovers the correct parameters for a translation transfrom") {
      val testImgUrl = getClass().getResource("/lena.h5").getPath()
      val discreteFixedImage = ImageIO.read2DScalarImage[Short](new File(testImgUrl)).get
      val fixedImage = Interpolation.interpolate2D(3)(discreteFixedImage)
      
      val domain = discreteFixedImage.domain
      val center = (domain.origin(0) + domain.extent(0) / 2, domain.origin(1) + domain.extent(1) / 2)
      
     // val rigidTransform = RigidTransformationSpace2D(center)(DenseVector(-0f,-0f, 3.14f  / 20))
     // val translationTransform = TranslationSpace2D()(DenseVector(-1f, 5f))
      val rotationTransform = RotationSpace2D(center)(DenseVector(3.14f/20))
      val transformedLena =fixedImage compose rotationTransform
      
      val registration = Registration.registration2D(fixedImage, transformedLena, RotationSpace2D(center), MeanSquaresMetric2D, 
          0f, DenseVector(1f))
      
      val regResult = registration(domain)    
      
     // (regResult.parameters(0) should be (1f plusOrMinus 0.0001f))
     // (regResult.parameters(1) should be (5f plusOrMinus 0.0001f))
      (regResult.parameters(0) should be (-3.14f/20 plusOrMinus 0.0001f))
      
    }
  }
}
