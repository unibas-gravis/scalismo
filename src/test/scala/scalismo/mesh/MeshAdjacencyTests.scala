package scalismo.mesh

import scalismo.ScalismoTestSuite
import scalismo.common.PointId
import scalismo.geometry.Point3D

class MeshAdjacencyTests extends ScalismoTestSuite {

  describe("a mesh adjacency operation") {

    object Fixture {
      val points = IndexedSeq(
        Point3D(0, 0, 0),
        Point3D(1, 0, 0),
        Point3D(0, 1, 0),
        Point3D(0, 0, 1)
      )
      val cells = TriangleList(
        IndexedSeq(
          TriangleCell(PointId(0), PointId(1), PointId(2)),
          TriangleCell(PointId(0), PointId(2), PointId(3))
        )
      )
      val mesh = TriangleMesh3D(
        points,
        cells
      )
      val meshWithFreePoint = TriangleMesh3D(
        points,
        TriangleList(cells.triangles.drop(1))
      )
      val meshWithFreePointAtEnd = TriangleMesh3D(
        points,
        TriangleList(cells.triangles.dropRight(1))
      )
    }

    it("should return the correct triangles for all points") {
      val mesh = Fixture.mesh

      val tests = Seq(
        (PointId(0), IndexedSeq(TriangleId(0), TriangleId(1))),
        (PointId(1), IndexedSeq(TriangleId(0))),
        (PointId(2), IndexedSeq(TriangleId(0), TriangleId(1))),
        (PointId(3), IndexedSeq(TriangleId(1)))
      )
      for ((pid, adjList) <- tests) {
        val adj = mesh.triangulation.adjacentTrianglesForPoint(pid)
        adj.sortBy(_.id) shouldBe adjList
      }
    }

    it("should return the correct triangles for points also not contained in a triangle") {
      {
        val mesh = Fixture.meshWithFreePoint

        val tests = Seq((PointId(0), IndexedSeq(TriangleId(0))),
                        (PointId(1), IndexedSeq()),
                        (PointId(2), IndexedSeq(TriangleId(0))),
                        (PointId(3), IndexedSeq(TriangleId(0)))
        )
        for ((pid, adjList) <- tests) {
          val adj = mesh.triangulation.adjacentTrianglesForPoint(pid)
          adj.sortBy(_.id) shouldBe adjList
        }
      }
      {
        val mesh = Fixture.meshWithFreePointAtEnd
        val tests = Seq(
          (PointId(0), IndexedSeq(TriangleId(0))),
          (PointId(1), IndexedSeq(TriangleId(0))),
          (PointId(2), IndexedSeq(TriangleId(0))),
          (PointId(3), IndexedSeq())
        )
        for ((pid, adjList) <- tests) {
          val adj = mesh.triangulation.adjacentTrianglesForPoint(pid)
          adj.sortBy(_.id) shouldBe adjList
        }
      }
    }
  }

}
