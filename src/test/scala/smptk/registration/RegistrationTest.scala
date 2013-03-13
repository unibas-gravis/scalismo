package registration

import org.scalatest.FunSpec
import java.nio.ByteBuffer
import java.io.File
import java.io.IOException
import smptk.image.DiscreteImageDomain2D
import smptk.registration.RotationSpace2D
import smptk.image.Geometry.CoordVector2D
import smptk.registration.TranslationSpace2D
import breeze.linalg.DenseVector
import smptk.registration.LandmarkRegistration
import org.scalatest.matchers.ShouldMatchers
import smptk.image.Geometry._
import smptk.registration.RigidTransformationSpace2D
import breeze.plot.Figure
import breeze.plot._

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
}
