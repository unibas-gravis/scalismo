package smptk
package geometry

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import implicits._

class GeometryTests extends FunSpec with ShouldMatchers {
  val p = Point3D(0.1, 3., 1.1)
  val pGeneric = new Point[ThreeD] {
    val data = Array(0.1, 3., 1.1)
    val dimensionality = 3
  }
  val v = Vector3D(0.1, 3., 1.1)
  val vGeneric = new Vector[ThreeD] {
    val data = Array(0.1, 3., 1.1)
    val dimensionality = 3
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
  }
}
