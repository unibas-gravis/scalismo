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

import breeze.linalg.DenseVector
import scalismo.common._
import scalismo.geometry._
import scalismo.mesh.boundingSpheres.Determinant
import scalismo.transformations.Transformation
import scalismo.utils.Random
import vtk.vtkTetra

import scala.language.implicitConversions

/** Represents a tetrahedron as a ordered list of four point ids. */
case class TetrahedralCell(ptId1: PointId, ptId2: PointId, ptId3: PointId, ptId4: PointId) extends Cell {

  /** Ordered list of the four point ids. */
  val pointIds = IndexedSeq(ptId1, ptId2, ptId3, ptId4)

  /** Ordered list of the triangles of the tetrahedron. */
  val triangles = Seq(TriangleCell(ptId1, ptId2, ptId3),
                      TriangleCell(ptId1, ptId4, ptId2),
                      TriangleCell(ptId1, ptId3, ptId4),
                      TriangleCell(ptId2, ptId4, ptId3)
  )

  /** Returns true if @ptId is one of the point ids of the tetrahedral. */
  def containsPoint(ptId: PointId): Boolean = {
    (ptId1 == ptId) || (ptId2 == ptId) || (ptId3 == ptId) || (ptId4 == ptId)
  }

  /** Returns a [[ DenseVector[Int] ]] with the ids of the point ids in order. */
  def toDenseVector: DenseVector[Int] = DenseVector[Int](ptId1.id, ptId2.id, ptId3.id, ptId4.id)
}

/** Represents a tetrahedral mesh. */
trait TetrahedralMesh[D] extends DiscreteDomain[D] {

  /** Ordered list of tetrahedrals forming the tetrahedral mesh. */
  def tetrahedralization: TetrahedralList

  /**
   * Superset of points used to define the tetrahedral mesh. In general it contains exactly the points used in all
   * tetrahedrals, but it not restricted to only contain points used in a tetrahedral. Points not used in any
   * tetrahedral are allowed.
   */
  def pointSet: UnstructuredPoints[D]

}

object TetrahedralMesh {

  def apply[D: NDSpace](pointList: IndexedSeq[Point[D]], topology: TetrahedralList)(implicit
    creator: Create[D]
  ): TetrahedralMesh[D] = {
    creator.createTetrahedraleMesh(UnstructuredPoints(pointList.toIndexedSeq), topology)
  }

  def apply[D: NDSpace](pointSet: UnstructuredPoints[D], topology: TetrahedralList)(implicit
    creator: Create[D]
  ): TetrahedralMesh[D] = {
    creator.createTetrahedraleMesh(pointSet, topology)
  }

  /** Typeclass for creating domains of arbitrary dimensionality */
  trait Create[D] extends UnstructuredPoints.Create[D] {
    def createTetrahedraleMesh(pointSet: UnstructuredPoints[D], topology: TetrahedralList): TetrahedralMesh[D]
  }

  trait Create3D extends Create[_3D] {
    override def createTetrahedraleMesh(pointSet: UnstructuredPoints[_3D],
                                        topology: TetrahedralList
    ): TetrahedralMesh3D = {
      TetrahedralMesh3D(pointSet, topology)
    }
  }

  implicit def parametricToConcreteType3D(tetrahedralMesh: TetrahedralMesh[_3D]): TetrahedralMesh3D = {
    tetrahedralMesh.asInstanceOf[TetrahedralMesh3D]
  }

  implicit object domainWarp extends DomainWarp[_3D, TetrahedralMesh] {

    /**
     * Warp the points of the domain of the discrete field and turn it into the warped domain
     */
    override def transformWithField(
      domain: TetrahedralMesh[_3D],
      warpField: DiscreteField[_3D, TetrahedralMesh, EuclideanVector[_3D]]
    ): TetrahedralMesh[_3D] = {

      // we should require(domain.pointSet == warpField.pointSet)
      // but we
      require(domain.pointSet.numberOfPoints == warpField.domain.pointSet.numberOfPoints)

      val newPoints = warpField.pointsWithValues.map { case (pt, v) => pt + v }
      TetrahedralMesh3D(UnstructuredPoints(newPoints.toIndexedSeq), domain.tetrahedralization)
    }

    override def transform(pointSet: TetrahedralMesh[_3D],
                           transformation: Transformation[_3D]
    ): TetrahedralMesh[_3D] = {
      TetrahedralMesh3D(pointSet.pointSet.transform(transformation), pointSet.tetrahedralization)
    }
  }
}

case class TetrahedralMesh3D(pointSet: UnstructuredPoints[_3D], tetrahedralization: TetrahedralList)
    extends TetrahedralMesh[_3D] {

  // val position = SurfacePointProperty(triangulation, pointSet.points.toIndexedSeq)
  val tetrahedrons: IndexedSeq[TetrahedralCell] = tetrahedralization.tetrahedrons
  val cells: IndexedSeq[TetrahedralCell] = tetrahedrons

  // lazy val operations: TetrahedralMesh3DOperations = TetrahedralMeshOperations(this)

  lazy val operations: TetrahedralMesh3DOperations = MeshOperations(this)

  lazy val boundingBox: BoxDomain[_3D] = pointSet.boundingBox

  /**
   * Applies a point transformation to the point set and returns a new transformed mesh. The method keeps the
   * tetrahedralization as it is and only changes the location of the points.
   *
   * @param transform
   *   A function that maps a given point to a new position. All instances of [[scalismo.registration.Transformation]]
   *   being descendants of <code>Function1[Point[_3D], Point[_3D] ]</code> are valid arguments.
   */
  def transform(transform: Point[_3D] => Point[_3D]): TetrahedralMesh3D = {
    TetrahedralMesh3D(pointSet.points.map(transform).toIndexedSeq, tetrahedralization)
  }

  /**
   * Returns the volume of the TetrahedralMesh as sum of all tetrahedrals. For meshes with overlapping tetrahedrals the
   * value will not be correct.
   */
  lazy val volume: Double = {
    var sum = 0.0
    tetrahedrons.foreach(t => sum += computeTetrahedronVolume(t))
    sum
  }

  /** Returns the volume of the indicated tetrahedral cell. */
  def computeTetrahedronVolume(tetrahedron: TetrahedralCell): Double = {
    val a = pointSet.point(tetrahedron.ptId1)
    val b = pointSet.point(tetrahedron.ptId2)
    val c = pointSet.point(tetrahedron.ptId3)
    val d = pointSet.point(tetrahedron.ptId4)

    val signedVolume = Determinant.det3x3(
      b.x - a.x,
      c.x - a.x,
      d.x - a.x,
      b.y - a.y,
      c.y - a.y,
      d.y - a.y,
      b.z - a.z,
      c.z - a.z,
      d.z - a.z
    ) / 6

    math.abs(signedVolume)
  }

  /** Returns the Barycentric coordinates of a point inside the indicated cell. */
  def getBarycentricCoordinates(point: Point[_3D], tetrathedron: TetrahedralCell): Array[Double] = {

    val a = pointSet.point(tetrathedron.ptId1)
    val b = pointSet.point(tetrathedron.ptId2)
    val c = pointSet.point(tetrathedron.ptId3)
    val d = pointSet.point(tetrathedron.ptId4)
    BarycentricCoordinates4.pointInTetrahedron(point, a, b, c, d).toArray
  }

  /** Returns true for points within a tetrahedron defined by the indicated cell. */
  def isInsideTetrahedralCell(point: Point[_3D], tetrahedron: TetrahedralCell): Boolean = {

    def hasOnlyStrictPositiveElements(array: Array[Double]): Boolean = {
      array.forall(_ > 0.0)
    }

    def countZeroEntries(array: Array[Double]): Int = {
      array.map(element => if (element == 0.0) 1 else 0).sum
    }

    // note: replace call to vtk with own implementation
    val barycentricCoordinates = getBarycentricCoordinates(point, tetrahedron)

    val normalized = barycentricCoordinates.map { e =>
      if (Math.abs(e) <= 1e-8) 0.0 else e
    }
    val numberOfZeroEntries = countZeroEntries(normalized)
    hasOnlyStrictPositiveElements(normalized) || (numberOfZeroEntries == 2) || (numberOfZeroEntries == 3)
  }

  /**
   * Returns a random point lying within the tetrahedron defined by the indicated cell.
   *
   * The sampled points follow a uniform distribution within the tetrahedron. The method if based on the paper
   * Generating Random Points in a Tetrahedron" from Rocchini et. al.:
   * https://www.tandfonline.com/doi/abs/10.1080/10867651.2000.10487528
   *
   * @param tc
   *   Tetrahedral cell of the mesh, in which to draw a random point
   * @param rnd
   *   implicit [[Random]] object
   */
  def samplePointInTetrahedralCell(tc: TetrahedralCell)(implicit rnd: Random): Point[_3D] = {
    val bc = BarycentricCoordinates4.randomUniform
    (bc.a *: pointSet.point(tc.ptId1).toVector +
      bc.b *: pointSet.point(tc.ptId2).toVector +
      bc.c *: pointSet.point(tc.ptId3).toVector +
      bc.d *: pointSet.point(tc.ptId4).toVector).toPoint
  }
}

object TetrahedralMesh3D {
  def apply(points: IndexedSeq[Point[_3D]], topology: TetrahedralList): TetrahedralMesh3D = {
    TetrahedralMesh3D(UnstructuredPoints(points.toIndexedSeq), topology)
  }

}
