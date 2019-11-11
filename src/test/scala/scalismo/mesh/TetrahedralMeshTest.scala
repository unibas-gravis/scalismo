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
  }

}
