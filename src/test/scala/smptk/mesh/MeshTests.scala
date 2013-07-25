package smptk
package mesh

import smptk.io.MeshIO
import smptk.geometry._
import java.io.File
import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers

class MeshTests extends FunSpec with ShouldMatchers {

  describe("a mesh domain") {
    it("finds the right closest points for all the points that define the mesh") {
      val path = getClass().getResource("/facemesh.h5").getPath
      val facemesh = MeshIO.readHDF5(new File(path)).get

      for ((pt, id) <- facemesh.domain.points.zipWithIndex) {
        val (closestPt, closestId) = facemesh.domain.findClosestPoint(pt)
        assert(closestPt === pt)
        assert(closestId === id)
      }
    }
    it("finds the right closest point for a point that is not defined on the mesh") {
      val pts = IndexedSeq(Point3D(0., 0., 0.), Point3D(1., 1., 1.), Point3D(1., 1., 5.))
      val cells = IndexedSeq(TriangleCell(0, 1, 2))
      val meshDomain = TriangleMeshDomain(pts, cells)

      val newPt = Point3D(1.1, 1.1, 4)
      val (closestPt, closestPtId) = meshDomain.findClosestPoint(newPt)
      assert(closestPtId === 2)
      assert(closestPt === pts(2))

    }
  }
}