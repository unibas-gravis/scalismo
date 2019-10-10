package scalismo.tetramesh

import java.io.File

import breeze.linalg.DenseVector
import scalismo.ScalismoTestSuite
import scalismo.common.{ PointId, UnstructuredPointsDomain }
import scalismo.geometry.Point.implicits._
import scalismo.geometry.{ Point, _3D }
import scalismo.io.{ MeshIO, TetraMeshIO }
import scalismo.registration.{ RotationSpace, ScalingSpace }

import scala.language.implicitConversions

class TetraMeshtest extends ScalismoTestSuite {

  implicit def doubleToFloat(d: Double): Float = d.toFloat
  implicit def intToPointId(i: Int): PointId = PointId(i)

  describe("a tetramesh") {
    val path = getClass.getResource("/virtualhumerustetramesh.vtl").getPath
    val virtualhumerustetramesh = TetraMeshIO.readTetraMesh(new File(path)).get

    it("finds the right closest points for all the points that define the tetramesh") {

      for ((pt, id) <- virtualhumerustetramesh.pointSet.points.zipWithIndex) {
        val ptWithID = virtualhumerustetramesh.pointSet.findClosestPoint(pt)
        val closestPt = ptWithID.point
        val closestId = ptWithID.id
        assert(closestPt === pt)
        assert(closestId.id === id)
      }
    }
    it("finds the right closest point for a point that is not defined on the tetramesh") {
      val pts = IndexedSeq(Point(0.0, 0.0, 0.0), Point(1.0, 1.0, 1.0), Point(1.0, 1.0, 5.0), Point(1.0, 1.0, -5.0))
      val cells = IndexedSeq(TetrahedralCell(0, 1, 2, 3))
      val tetramesh = TetrahedralMesh3D(UnstructuredPointsDomain(pts), TetrahedralList(cells))

      val newPt = Point(1.1, 1.1, 4)
      val ptWithID = tetramesh.pointSet.findClosestPoint(newPt)
      val closestPt = ptWithID.point
      val closestPtId = ptWithID.id
      assert(closestPtId.id === 2)
      assert(closestPt === pts(2))
    }
    it("computes its volume correctly for a tetrahedron") {
      val pts: IndexedSeq[Point[_3D]] = IndexedSeq((0.0, 0.0, 0.0), (0.0, 1.0, 0.0), (1.0, 0.0, 0.0), (0.0, 0.0, 1.0))
      val cells = IndexedSeq(TetrahedralCell(0, 1, 2, 3))
      val tetramesh = TetrahedralMesh3D(UnstructuredPointsDomain(pts), TetrahedralList(cells))

      val R = RotationSpace[_3D]((0.0, 0.0, 0.0)).transformForParameters(DenseVector(0.3, 0.4, 0.1))
      val s = ScalingSpace[_3D].transformForParameters(DenseVector(2.0))
      val transformedTetraMesh = tetramesh.transform(R).transform(s)
      tetramesh.volume should be(0.166 +- 1e-8)
      transformedTetraMesh.volume should be(8.0f * tetramesh.volume +- 1e-5) // scaling by two gives 8 times the volume
    }

    /* it("computes the right binary image for the unit sphere") {
      val path = getClass.getResource("/unit-sphere.stl").getPath
      val spheremesh = MeshIO.readMesh(new File(path)).get
      val binaryImg = spheremesh.operations.toBinaryImage
      binaryImg(Point(0, 0, 0)) should be(1)
      binaryImg(Point(2, 0, 0)) should be(0)
    }*/

    it("can have an empty cell list") {
      val pts = IndexedSeq(Point(0.0, 0.0, 0.0), Point(1.0, 1.0, 1.0), Point(1.0, 1.0, 5.0), Point(1.0, -1.0, 5.0))
      val cells = IndexedSeq[TetrahedralCell]()
      try {
        TetrahedralMesh3D(UnstructuredPointsDomain(pts), TetrahedralList(cells)) // would throw exception on fail
      } catch {
        case e: Exception => fail("It should be possible to create tetrahedralMesh with an empty cell list")
      }
    }
  }

}
