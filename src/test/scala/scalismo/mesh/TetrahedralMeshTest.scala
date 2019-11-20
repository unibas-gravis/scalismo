/*
 * Copyright University of Basel, Graphics and Vision Research Group
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package scalismo.mesh

import scalismo.ScalismoTestSuite
import scalismo.common.{ PointId, UnstructuredPointsDomain }
import scalismo.geometry.{ EuclideanVector3D, Point, Point3D, _3D }
import scalismo.registration.{ RotationTransform, TranslationTransform }
import scalismo.utils.Random

class TetrahedralMeshTest extends ScalismoTestSuite {

  implicit val rng = Random(42L)

  def createTetrahedronsInUnitCube(): TetrahedralMesh3D = {
    // points around unit cube
    val points = IndexedSeq(
      Point(0, 0, 0),
      Point(1, 0, 0),
      Point(1, 1, 0),
      Point(0, 1, 0),
      Point(0, 0, 1),
      Point(1, 0, 1),
      Point(1, 1, 1),
      Point(0, 1, 1)
    )
    val domain = UnstructuredPointsDomain(points)

    // cells covering the complete cube
    implicit def intToPointId(i: Int): PointId = PointId(i)
    val cells = IndexedSeq(
      TetrahedralCell(0, 2, 7, 3),
      TetrahedralCell(0, 2, 5, 1),
      TetrahedralCell(2, 5, 7, 6),
      TetrahedralCell(0, 5, 7, 4),
      TetrahedralCell(0, 2, 5, 7)
    )
    val list = TetrahedralList(cells)

    TetrahedralMesh3D(domain, list)
  }

  def createTetrahedronsTwoUnitCubes(): TetrahedralMesh3D = {
    // points around unit cube
    val points = IndexedSeq(
      Point(0, 0, 0),
      Point(1, 0, 0),
      Point(1, 1, 0),
      Point(0, 1, 0),
      Point(0, 0, 1),
      Point(1, 0, 1),
      Point(1, 1, 1),
      Point(0, 1, 1),
      Point(0, 0, 2),
      Point(1, 0, 2),
      Point(1, 1, 2),
      Point(0, 1, 2)
    )
    val domain = UnstructuredPointsDomain(points)

    // cells covering the complete cube
    implicit def intToPointId(i: Int): PointId = PointId(i)
    val cells = IndexedSeq(
      TetrahedralCell(0, 2, 7, 3),
      TetrahedralCell(0, 2, 5, 1),
      TetrahedralCell(2, 5, 7, 6),
      TetrahedralCell(0, 5, 7, 4),
      TetrahedralCell(0, 2, 5, 7),
      TetrahedralCell(4, 6, 11, 7),
      TetrahedralCell(4, 6, 9, 5),
      TetrahedralCell(6, 9, 11, 10),
      TetrahedralCell(4, 9, 11, 8),
      TetrahedralCell(4, 6, 9, 11)
    )
    val list = TetrahedralList(cells)

    TetrahedralMesh3D(domain, list)
  }

  def createRandomTetrahedralMesh(): TetrahedralMesh3D = {
    val rng = Random(42l)
    val N = 200
    val points = IndexedSeq.fill(N)(Point(
      rng.scalaRandom.nextGaussian() * 2,
      rng.scalaRandom.nextGaussian() * 1000,
      rng.scalaRandom.nextGaussian() * 1000000
    ))
    val domain = UnstructuredPointsDomain(points)

    implicit def intToPointId(i: Int): PointId = PointId(i)
    val T = 200
    val indices = rng.scalaRandom.shuffle((0 until N).toIndexedSeq).take(4)
    val cells = IndexedSeq.fill(T)(TetrahedralCell(
      indices(0),
      indices(1),
      indices(2),
      indices(3)
    ))
    val list = TetrahedralList(cells)

    TetrahedralMesh3D(domain, list)
  }

  describe("a tetrahedral mesh") {
    it("should calculate the correct volume of a tetrahedral mesh and its tetrahedrals") {

      val epsilonVolume = 1.0e-8

      for (i <- 0 until 20) {
        val t = TranslationTransform(EuclideanVector3D(
          rng.scalaRandom.nextGaussian() * 50,
          rng.scalaRandom.nextGaussian() * 50,
          rng.scalaRandom.nextGaussian() * 50
        ))

        val R = RotationTransform(
          rng.scalaRandom.nextGaussian() * Math.PI,
          rng.scalaRandom.nextGaussian() * Math.PI,
          rng.scalaRandom.nextGaussian() * Math.PI,
          Point3D.origin
        )
        def mapping(pt: Point[_3D]) = R(t(pt))

        val tetrahedron = createTetrahedronsInUnitCube().transform(mapping)

        val ref = tetrahedron.computeTetrahedronVolume(tetrahedron.cells(0))
        tetrahedron.computeTetrahedronVolume(tetrahedron.cells(1)) should be(ref +- epsilonVolume)
        tetrahedron.computeTetrahedronVolume(tetrahedron.cells(2)) should be(ref +- epsilonVolume)
        tetrahedron.computeTetrahedronVolume(tetrahedron.cells(3)) should be(ref +- epsilonVolume)
        tetrahedron.computeTetrahedronVolume(tetrahedron.cells(4)) should be((1.0 - 4.0 * ref) +- 1.0e-8)
        tetrahedron.volume should be(1.0 +- epsilonVolume)
      }
    }

    it("can have an empty cell list") {
      val pts = IndexedSeq(Point(0.0, 0.0, 0.0), Point(1.0, 1.0, 1.0), Point(1.0, 1.0, 5.0), Point(1.0, -1.0, 5.0))
      val cells = IndexedSeq[TetrahedralCell]()
      try {
        TetrahedralMesh3D(UnstructuredPointsDomain(pts), TetrahedralList(cells)) // would throw exception on fail
      } catch {
        case e: Exception => fail("It should be possible to create tetrahedralMesh with an empty cell list")
      }
    }

    it("should return the correct adjacent points for a point") {
      val tetrahedron = createTetrahedronsInUnitCube()

      {
        val pid = PointId(0)
        val shouldReturn = Seq(1, 2, 3, 4, 5, 7).sorted
        val neighbours = tetrahedron.tetrahedralization.adjacentPointsForPoint(pid).map(_.id).sorted
        assert(shouldReturn.size == neighbours.size)
        assert(shouldReturn.zip(neighbours).forall { case (a, b) => a == b })
      }

      {
        val pid = PointId(1)
        val shouldReturn = Seq(0, 2, 5).sorted
        val neighbours = tetrahedron.tetrahedralization.adjacentPointsForPoint(pid).map(_.id).sorted
        assert(shouldReturn.size == neighbours.size)
        assert(shouldReturn.zip(neighbours).forall { case (a, b) => a == b })
      }

      {
        val pid = PointId(2)
        val shouldReturn = Seq(0, 1, 3, 5, 6, 7).sorted
        val neighbours = tetrahedron.tetrahedralization.adjacentPointsForPoint(pid).map(_.id).sorted
        assert(shouldReturn.size == neighbours.size)
        assert(shouldReturn.zip(neighbours).forall { case (a, b) => a == b })
      }
    }

    it("should return the correct adjacent tetrahedral cells for a point") {
      val tetrahedron = createTetrahedronsInUnitCube()

      {
        val pid = PointId(0)
        val shouldReturn = Seq(0, 1, 3, 4).sorted
        val neighbours = tetrahedron.tetrahedralization.adjacentTetrahedronsForPoint(pid).map(_.id).sorted
        assert(shouldReturn.size == neighbours.size)
        assert(shouldReturn.zip(neighbours).forall { case (a, b) => a == b })
      }

      {
        val pid = PointId(1)
        val shouldReturn = Seq(1).sorted
        val neighbours = tetrahedron.tetrahedralization.adjacentTetrahedronsForPoint(pid).map(_.id).sorted
        assert(shouldReturn.size == neighbours.size)
        assert(shouldReturn.zip(neighbours).forall { case (a, b) => a == b })
      }

      {
        val pid = PointId(2)
        val shouldReturn = Seq(0, 1, 2, 4).sorted
        val neighbours = tetrahedron.tetrahedralization.adjacentTetrahedronsForPoint(pid).map(_.id).sorted
        assert(shouldReturn.size == neighbours.size)
        assert(shouldReturn.zip(neighbours).forall { case (a, b) => a == b })
      }
    }

    it("should return the correct adjacent tetrahedrons for a tetrahedron") {
      val tetrahedron = createTetrahedronsTwoUnitCubes()

      {
        val tid = TetrahedronId(0)
        val shouldReturn = Seq(1, 2, 3, 4, 5).sorted
        val neighbours = tetrahedron.tetrahedralization.adjacentTetrahedronsForTetrahedron(tid).map(_.id).sorted
        assert(shouldReturn.size == neighbours.size)
        assert(shouldReturn.zip(neighbours).forall { case (a, b) => a == b })
      }

      {
        val tid = TetrahedronId(1)
        val shouldReturn = Seq(0, 2, 3, 4, 6).sorted
        val neighbours = tetrahedron.tetrahedralization.adjacentTetrahedronsForTetrahedron(tid).map(_.id).sorted
        assert(shouldReturn.size == neighbours.size)
        assert(shouldReturn.zip(neighbours).forall { case (a, b) => a == b })
      }

      {
        val tid = TetrahedronId(4)
        val shouldReturn = Seq(0, 1, 2, 3, 5, 6).sorted
        val neighbours = tetrahedron.tetrahedralization.adjacentTetrahedronsForTetrahedron(tid).map(_.id).sorted
        assert(shouldReturn.size == neighbours.size)
        assert(shouldReturn.zip(neighbours).forall { case (a, b) => a == b })
      }
    }

    it("should return only sampled points within a tetrahedron when trying to sample uniformly in it") {
      val mesh = createRandomTetrahedralMesh()

      for (tetrahedron <- mesh.tetrahedrons) {
        for (_ <- 0 until 20) {
          val point = mesh.samplePointInTetrahedralCell(tetrahedron)
          require(mesh.isInsideTetrahedralCell(point, tetrahedron))
        }
      }
    }

    it("should not throw an error when sampling points uniformly in a tetrahedron") {
      val tetrahedron = createTetrahedronsInUnitCube()
      try {
        for (i <- 0 until 1000) {
          tetrahedron.samplePointInTetrahedralCell(tetrahedron.tetrahedralization.tetrahedron(TetrahedronId(0)))
        }
      } catch {
        case e: Exception => fail("It should be possible to sample points in a tetrahedron without throwing an exception")
      }
    }
  }

}
