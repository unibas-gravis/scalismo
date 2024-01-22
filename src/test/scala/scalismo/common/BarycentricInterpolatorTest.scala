package scalismo.common

import scalismo.ScalismoTestSuite
import scalismo.common.interpolation.BarycentricInterpolator
import scalismo.geometry.{_3D, EuclideanVector, Point, Point3D}
import scalismo.mesh.{TetrahedralCell, TetrahedralList, TetrahedralMesh, TetrahedralMesh3D}
import scalismo.utils.Random

class BarycentricInterpolatorTest extends ScalismoTestSuite {

  implicit val rng: Random = Random(42L)

  def createTetrahedronsInUnitCube(): TetrahedralMesh[_3D] = {
    // points around unit cube
    val points = IndexedSeq(Point(0, 0, 0),
                            Point(1, 0, 0),
                            Point(1, 1, 0),
                            Point(0, 1, 0),
                            Point(0, 0, 1),
                            Point(1, 0, 1),
                            Point(1, 1, 1),
                            Point(0, 1, 1)
    )
    val domain = UnstructuredPoints(points)

    // cells covering the complete cube
    implicit def intToPointId(i: Int): PointId = PointId(i)
    val cells = IndexedSeq(TetrahedralCell(0, 2, 7, 3),
                           TetrahedralCell(0, 2, 5, 1),
                           TetrahedralCell(2, 5, 7, 6),
                           TetrahedralCell(0, 5, 7, 4),
                           TetrahedralCell(0, 2, 5, 7)
    )
    val list = TetrahedralList(cells)

    TetrahedralMesh3D(domain, list)
  }

  describe("A Barycentric Interpolator") {

    it("returns correct value at vertex points.") {

      val tetrahedralMesh = createTetrahedronsInUnitCube()
      val scalars = IndexedSeq[Double](0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0)
      val scalarVolumeMeshField =
        DiscreteField.apply(tetrahedralMesh, scalars)
      val interpolatedVolumeMeshField = scalarVolumeMeshField.interpolate(BarycentricInterpolator[_3D, Double]())

      val vertexValues = tetrahedralMesh.pointSet.points.map(interpolatedVolumeMeshField(_))
      vertexValues.zipWithIndex.toIndexedSeq
        .foreach(p => {
          p._1 shouldBe p._2
        })

    }

    it("returns correct values inside cells.") {

      def getTetrahedralMeshCell(m: TetrahedralMesh3D, p: Point[_3D]): TetrahedralCell = {
        val closestPoint = m.pointSet.findClosestPoint(p)
        val adjacentTetrahedra = m.tetrahedralization.adjacentTetrahedronsForPoint(closestPoint.id)
        val tetraId =
          adjacentTetrahedra.filter(tId => m.isInsideTetrahedralCell(p, m.tetrahedralization.tetrahedrons(tId.id))).head
        m.tetrahedralization.tetrahedrons(tetraId.id)
      }

      val tetrahedralMesh = createTetrahedronsInUnitCube()
      val scalars = IndexedSeq[Double](0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0)
      val scalarVolumeMeshField = DiscreteField(tetrahedralMesh, scalars)
      val interpolatedVolumeMeshField = scalarVolumeMeshField.interpolate(BarycentricInterpolator())

      val point = Point3D(0.0080570729074948, 0.4107871517927135, 0.6832234717598454)
      val cell = getTetrahedralMeshCell(tetrahedralMesh, point)
      val vertexValues = cell.pointIds.map(pId => { scalars(pId.id) })
      val barycentricCoordinates = tetrahedralMesh.getBarycentricCoordinates(point, cell)

      val valueAtPoint = vertexValues.zip(barycentricCoordinates).map(t => t._1 * t._2).sum
      val interpolatedValueAtPoint = interpolatedVolumeMeshField(point)

      valueAtPoint shouldBe interpolatedValueAtPoint
    }

  }

}
