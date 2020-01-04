package scalismo.mesh

import scalismo.ScalismoTestSuite
import scalismo.common.PointId
import scalismo.geometry.Point

class MeshSurfacePropertyTests extends ScalismoTestSuite {

  val points = IndexedSeq(
    Point(0, 1, 0),
    Point(0, 0, 0),
    Point(1, 1, 0),
    Point(1, 0, 0))

  val values = IndexedSeq(0.0, 1.0, 2.0, 3.0)

  val triangulation = TriangleList(
    IndexedSeq(
      TriangleCell(PointId(0), PointId(1), PointId(2)),
      TriangleCell(PointId(2), PointId(1), PointId(3))))

  val pointProperty = SurfacePointProperty(triangulation, values)

  describe("A SurfacePointProperty") {
    it("should have the point values at vertex points") {
      pointProperty(TriangleId(1), BarycentricCoordinates.v1) shouldBe values(1)
      pointProperty(TriangleId(1), BarycentricCoordinates.v0) shouldBe values(2)
      pointProperty(TriangleId(0), BarycentricCoordinates.v1) shouldBe values(1)
    }

    it("should interpolate point values inside the triangles") {
      pointProperty(TriangleId(0), BarycentricCoordinates.center) shouldBe (values(0) + values(1) + values(2)) / 3.0
      pointProperty(TriangleId(1), BarycentricCoordinates.center) shouldBe (values(2) + values(1) + values(3)) / 3.0

    }
  }

  describe("A MappedMeshSurfaceProperty") {
    // negative values
    val f = (d: Double) => -d

    val mappedProperty: MeshSurfaceProperty[Double] = pointProperty.map { f }

    it("applies function f as expected") {
      mappedProperty(TriangleId(1), BarycentricCoordinates.v1) shouldBe -values(1)
      mappedProperty(TriangleId(0), BarycentricCoordinates(0.1, 0.2, 0.7)) shouldBe -pointProperty(TriangleId(0), BarycentricCoordinates(0.1, 0.2, 0.7))
    }

    it("grants access to its triangulation without an AbstractMethodError (regression check, issue #138)") {
      mappedProperty.triangulation
    }

    it("uses the same triangulation as the underlying property") {
      mappedProperty.triangulation shouldBe pointProperty.triangulation
    }
  }
}
