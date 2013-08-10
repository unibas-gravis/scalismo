package smptk.mesh

import scala.language.implicitConversions
import smptk.io.MeshIO
import smptk.geometry._
import smptk.geometry.implicits._
import java.io.File
import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import smptk.registration.RotationSpace3D
import breeze.linalg.DenseVector
import smptk.registration.ProductTransformationSpace
import smptk.registration.ScalingSpace3D
import smptk.registration.ScalingSpace3D
import smptk.registration.RotationSpace3D

class MeshTests extends FunSpec with ShouldMatchers {
  
    implicit def doubleToFloat(d : Double) = d.toFloat
  
  smptk.initialize()
  describe("a mesh") {
    it("finds the right closest points for all the points that define the mesh") {
      val path = getClass().getResource("/facemesh.h5").getPath
      val facemesh = MeshIO.readHDF5(new File(path)).get

      for ((pt, id) <- facemesh.points.zipWithIndex) {
        val (closestPt, closestId) = facemesh.findClosestPoint(pt)
        assert(closestPt === pt)
        assert(closestId === id)
      }
    }
    it("finds the right closest point for a point that is not defined on the mesh") {
      val pts = IndexedSeq(Point3D(0.0, 0.0, 0.0), Point3D(1.0, 1.0, 1.0), Point3D(1.0, 1.0, 5.0))
      val cells = IndexedSeq(TriangleCell(0, 1, 2))
      val mesh = TriangleMesh(pts, cells)

      val newPt = Point3D(1.1, 1.1, 4)
      val (closestPt, closestPtId) = mesh.findClosestPoint(newPt)
      assert(closestPtId === 2)
      assert(closestPt === pts(2))

    }
    it("computes its area correctly for a triangle") {
      val pts : IndexedSeq[Point3D]= IndexedSeq((0.0, 0.0, 0.0), (0.0, 1.0, 0.0), (1.0, 0.0, 0.0))
      val cells = IndexedSeq(TriangleCell(0, 1, 2))
      val mesh = TriangleMesh(pts, cells)
      
      val R = RotationSpace3D((0.0, 0.0, 0.0))(DenseVector(0.3, 0.4, 0.1))
      val s = ScalingSpace3D()(DenseVector(2.0))
      val transformedMesh = mesh.compose(R).compose(s)
      mesh.area should be(0.5 plusOrMinus 1e-8)
      transformedMesh.area should be(4.0f * mesh.area plusOrMinus 1e-5) // scaling by two gives 4 times the area 
    }
  }
}