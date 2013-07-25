package smptk
package geometry

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import implicits._

class GeometryTests extends FunSpec with ShouldMatchers {
  val p = Point3D(0.1, 3., 1.1)
  val pGeneric = new Point[ThreeD] {
    val data = Array(0.1, 3., 1.1)
  }
  val v = Vector3D(0.1, 3., 1.1)
  val vGeneric = new Vector[ThreeD] {
    val data = Array(0.1, 3., 1.1)
  }  
  
  describe("A 3D Point") {
    it("equals a Point[ThreeD]") {
      p should equal(pGeneric)
    }

    it("does not equal a Vector[ThreeD]") {
      p should not equal(v)
      p should not equal(vGeneric)
    }
  }

  describe("A 3D Vector") {
    it("equals a Vector[ThreeD]") {
      v should equal(vGeneric)
    }

    it("does not equal a Point[ThreeD]") {
      v should not equal (p)
      v should not equal (pGeneric)      
    }

    it("equals a point when converted to point") {
      v.toPoint should equal(p)
    }
    
    it("gives the correct norm and normsquared for various test cases") {
    	Vector1D(1).norm2 should equal(1)
    	Vector2D(1,1).norm2 should equal(2)
    	Vector3D(1, 1,1).norm2 should equal(3)
    	Vector2D(math.sqrt(2), math.sqrt(2)).norm2 should be(4.0 plusOrMinus(1e-5))
    	v.norm should be(math.sqrt(v.norm2) plusOrMinus(1e-5))
    }
    
  }
}
