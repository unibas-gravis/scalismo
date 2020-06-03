package scalismo.mesh

import scalismo.ScalismoTestSuite
import scalismo.common.PointId
import scalismo.geometry.{_3D, Point, Point3D}
import scalismo.mesh.boundingSpheres.BoundingSphereHelpers
import scalismo.utils.Random
import vtk.{vtkTetra, vtkTriangle}

class BarycentricCoordinateTests extends ScalismoTestSuite {

  implicit val rng: Random = Random(1024L)

  val epsilon = 1.0e-8

  def genPoint()(implicit rng: Random) = Point3D(
    rng.scalaRandom.nextDouble() * 20 - 10,
    rng.scalaRandom.nextDouble() * 20 - 10,
    rng.scalaRandom.nextDouble() * 20 - 10
  )

  def generatePositiveOrientedSingleTetrahedronMesh(): TetrahedralMesh3D = {
    val points = {
      val points = IndexedSeq.fill(4)(genPoint)
      if (BoundingSphereHelpers.calculateSignedVolume(points(0).toVector,
                                                      points(1).toVector,
                                                      points(2).toVector,
                                                      points(3).toVector) < 0) {
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
                                       point: Point[_3D]): IndexedSeq[Double] = {
    val barycentricCoordinates = new Array[Double](3)
    val vtkTriangle = new vtkTriangle()
    vtkTriangle.BarycentricCoords(point.toArray, a.toArray, b.toArray, c.toArray, barycentricCoordinates)
    vtkTriangle.Delete()
    barycentricCoordinates.toIndexedSeq
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

  describe("Barycentric coordinates for a triangle") {

    it("should calculate the same values as defined for the vertices") {
      for (j <- 0 until 10) {
        val a = genPoint()
        val b = genPoint()
        val c = genPoint()

        {
          val bc = BarycentricCoordinates.pointInTriangle3D(a, a, b, c)
          val bcDef = BarycentricCoordinates.v0

          bc.a should be(bcDef.a +- epsilon)
          bc.b should be(bcDef.b +- epsilon)
          bc.c should be(bcDef.c +- epsilon)
        }

        {
          val bc = BarycentricCoordinates.pointInTriangle3D(a, a, b, c)
          val bcDef = BarycentricCoordinates.v0

          bc.a should be(bcDef.a +- epsilon)
          bc.b should be(bcDef.b +- epsilon)
          bc.c should be(bcDef.c +- epsilon)
        }

        {
          val bc = BarycentricCoordinates.pointInTriangle3D(a, a, b, c)
          val bcDef = BarycentricCoordinates.v0

          bc.a should be(bcDef.a +- epsilon)
          bc.b should be(bcDef.b +- epsilon)
          bc.c should be(bcDef.c +- epsilon)
        }
      }
    }

    it("should return the same coordinates for a point as VTK") {
      for (j <- 0 until 10) {
        val a = genPoint()
        val b = genPoint()
        val c = genPoint()
        for (i <- 0 until 20) {
          val randomBC = BarycentricCoordinates.randomUniform
          val randomPointInTriangle =
            (a.toVector * randomBC.a + b.toVector * randomBC.b + c.toVector * randomBC.c).toPoint
          val bc = BarycentricCoordinates.pointInTriangle3D(randomPointInTriangle, a, b, c)
          val bcVTK = getBarycentricCoordinatesFromVTK(a, b, c, randomPointInTriangle)

          bc.a should be(bcVTK(0) +- epsilon)
          bc.b should be(bcVTK(1) +- epsilon)
          bc.c should be(bcVTK(2) +- epsilon)
        }
      }
    }

    it("should return the same coordinates in 2d as in 3d for flat triangles") {
      for (j <- 0 until 10) {
        val a = genPoint.copy(z = 0.0)
        val b = genPoint.copy(z = 0.0)
        val c = genPoint.copy(z = 0.0)

        def to2d(pt: Point[_3D]) = Point(pt.x, pt.y)

        for (i <- 0 until 20) {
          val randomPoint = genPoint.copy(z = 0.0)
          val bc3D = BarycentricCoordinates.pointInTriangle3D(randomPoint, a, b, c)
          val bc2D = BarycentricCoordinates.pointInTriangle(to2d(randomPoint), to2d(a), to2d(b), to2d(c))

          bc3D.a should be(bc2D.a +- epsilon)
          bc3D.b should be(bc2D.b +- epsilon)
          bc3D.c should be(bc2D.c +- epsilon)
        }
      }
    }

    it("should calculate the barycentric coordinates form the generated point") {
      for (j <- 0 until 10) {
        val a = genPoint()
        val b = genPoint()
        val c = genPoint()
        for (i <- 0 until 20) {
          val randomBC = BarycentricCoordinates.randomUniform
          val pointFromBC =
            (a.toVector * randomBC.a + b.toVector * randomBC.b + c.toVector * randomBC.c).toPoint
          val bc = BarycentricCoordinates.pointInTriangle3D(pointFromBC, a, b, c)

          bc.a should be(randomBC.a +- epsilon)
          bc.b should be(randomBC.b +- epsilon)
          bc.c should be(randomBC.c +- epsilon)
        }
      }
    }
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

          bc.a should be(bcVTK(0) +- epsilon)
          bc.b should be(bcVTK(1) +- epsilon)
          bc.c should be(bcVTK(2) +- epsilon)
          bc.d should be(bcVTK(3) +- epsilon)
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

      val N = 10000

      val startVTK = System.currentTimeMillis()
      for (i <- 0 until N) {
        val randomPoint = genPoint()
        getBarycentricCoordinatesFromVTK(a, b, c, d, randomPoint)
      }
      val vtkTime = System.currentTimeMillis() - startVTK

      val startScala = System.currentTimeMillis()
      for (i <- 0 until N) {
        val randomPoint = genPoint()
        BarycentricCoordinates4.pointInTetrahedron(randomPoint, a, b, c, d)
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
