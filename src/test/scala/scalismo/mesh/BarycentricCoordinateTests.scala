package scalismo.mesh

import scalismo.ScalismoTestSuite
import scalismo.common.PointId
import scalismo.geometry.{_3D, EuclideanVector, Point, Point3D}
import scalismo.utils.Random
import vtk.vtkTetra

class BarycentricCoordinateTests extends ScalismoTestSuite {

  implicit val rng: Random = Random(1024L)

  def genPoint()(implicit rng: Random) = Point3D(
    rng.scalaRandom.nextDouble() * 20 - 10,
    rng.scalaRandom.nextDouble() * 20 - 10,
    rng.scalaRandom.nextDouble() * 20 - 10
  )

  def determinantVectorsInRows(t: EuclideanVector[_3D], u: EuclideanVector[_3D], v: EuclideanVector[_3D]): Double =
    t.x * u.y * v.z + u.x * v.y * t.z + v.x * t.y * u.z - t.x * v.y * u.z - v.x * u.y * t.z - u.x * t.y * v.z

  def calculateSignedVolume(
    a: Point3D,
    b: Point3D,
    c: Point3D,
    d: Point3D
  ): Double = {
    val t = b - a
    val u = c - a
    val v = d - a
    determinantVectorsInRows(t, u, v)
  }

  def generatePositiveOrientedSingleTetrahedronMesh(): TetrahedralMesh3D = {
    val points = {
      val points = IndexedSeq.fill(4)(genPoint)
      if (calculateSignedVolume(points(0), points(1), points(2), points(3)) < 0) {
        IndexedSeq(points(1), points(2), points(3), points(0))
      } else
        points
    }
    TetrahedralMesh3D(points,
                      TetrahedralList(IndexedSeq(TetrahedralCell(PointId(0), PointId(1), PointId(2), PointId(3)))))
  }

  def getBarycentricCoordinatesFromVTK(a: Point[_3D],
                                       b: Point[_3D],
                                       c: Point[_3D],
                                       d: Point[_3D],
                                       point: Point[_3D]): IndexedSeq[Double] = {
    val barycentricCoordinates = new Array[Double](4)
    val vtkTetra = new vtkTetra()
    vtkTetra.BarycentricCoords(point.toArray, a.toArray, b.toArray, c.toArray, d.toArray, barycentricCoordinates)
    vtkTetra.Delete()
    barycentricCoordinates.toIndexedSeq
  }

  describe("Barycentric coordinates for a tetrahedron") {

    it("should return the same coordinates for a point as VTK") {
      for (j <- 0 until 10) {
        val tmesh = generatePositiveOrientedSingleTetrahedronMesh()
        val cell = tmesh.cells.head
        val a = tmesh.pointSet.point(cell.ptId1)
        val b = tmesh.pointSet.point(cell.ptId2)
        val c = tmesh.pointSet.point(cell.ptId3)
        val d = tmesh.pointSet.point(cell.ptId4)
        for (i <- 0 until 20) {
          val randomPoint = genPoint()
          val bc = BarycentricCoordinates4.pointInTetrahedron(randomPoint, a, b, c, d)
          val bcVTK = getBarycentricCoordinatesFromVTK(a, b, c, d, randomPoint)

          bc.a should be(bcVTK(0) +- 1e-13)
        }
      }
    }

    it("should calculate coordinates faster than using VTK") {

      val tmesh = generatePositiveOrientedSingleTetrahedronMesh()
      val cell = tmesh.cells.head
      val a = tmesh.pointSet.point(cell.ptId1)
      val b = tmesh.pointSet.point(cell.ptId2)
      val c = tmesh.pointSet.point(cell.ptId3)
      val d = tmesh.pointSet.point(cell.ptId4)

      val N = 1000

      val startVTK = System.currentTimeMillis()
      for (i <- 0 until N) {
        val randomPoint = genPoint()
        val bcVTK = getBarycentricCoordinatesFromVTK(a, b, c, d, randomPoint)
      }
      val vtkTime = System.currentTimeMillis() - startVTK

      val startScala = System.currentTimeMillis()
      for (i <- 0 until N) {
        val randomPoint = genPoint()
        val bc = BarycentricCoordinates4.pointInTetrahedron(randomPoint, a, b, c, d)
      }
      val scalaTime = System.currentTimeMillis() - startScala

      scalaTime should be < vtkTime
    }

    it("should reconstruct the point from the bc coordinates") {
      val tmesh = generatePositiveOrientedSingleTetrahedronMesh()
      val cell = tmesh.cells.head
      val a = tmesh.pointSet.point(cell.ptId1)
      val b = tmesh.pointSet.point(cell.ptId2)
      val c = tmesh.pointSet.point(cell.ptId3)
      val d = tmesh.pointSet.point(cell.ptId4)

      for (i <- 0 until 1000) {
        val randomPoint = genPoint()
        val bc = BarycentricCoordinates4.pointInTetrahedron(randomPoint, a, b, c, d)
        val pt = bc.a *: a.toVector + bc.b *: b.toVector + bc.c *: c.toVector + bc.d *: d.toVector

        (randomPoint.toVector - pt).norm should be < 1e-8
      }

    }
  }
}
