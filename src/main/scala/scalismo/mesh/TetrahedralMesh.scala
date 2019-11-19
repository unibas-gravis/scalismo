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
import scalismo.utils.Random
import vtk.vtkTetra

import scala.language.implicitConversions

/** Represents a tetrahedron as a ordered list of four point ids. */
case class TetrahedralCell(ptId1: PointId, ptId2: PointId, ptId3: PointId, ptId4: PointId) extends Cell {

  /** Ordered list of the four point ids. */
  val pointIds = IndexedSeq(ptId1, ptId2, ptId3, ptId4)

  /** Ordered list of the triangles of the tetrahedron. */
  val triangles = Seq(TriangleCell(ptId1, ptId2, ptId3), TriangleCell(ptId1, ptId2, ptId4),
    TriangleCell(ptId1, ptId3, ptId4), TriangleCell(ptId2, ptId3, ptId4))

  /** Returns true if @ptId is one of the point ids of the tetrahedral. */
  def containsPoint(ptId: PointId): Boolean = {
    (ptId1 == ptId) || (ptId2 == ptId) || (ptId3 == ptId) || (ptId4 == ptId)
  }

  /** Returns a [[ DenseVector[Int] ]] with the ids of the point ids in order. */
  def toDenseVector: DenseVector[Int] = DenseVector[Int](ptId1.id, ptId2.id, ptId3.id, ptId4.id)
}

/** Represents a tetrahedral mesh. */
trait TetrahedralMesh[D] {

  /** Ordered list of tetrahedrals forming the tetrahedral mesh. */
  def tetrahedralization: TetrahedralList

  /**
   * Superset of points used to define the tetrahedral mesh.
   * In general it contains exactly the points used in all
   * tetrahedrals, but it not restricted to only contain
   * points used in a tetrahedral. Points not used in any
   * tetrahedral are allowed.
   */
  def pointSet: UnstructuredPointsDomain[D]

  /** Applies the point transformation to the point set only and returns the transformed mesh. */
  def transform(transform: Point[D] => Point[D]): TetrahedralMesh[D]

}

object TetrahedralMesh {

  def apply[D: NDSpace](pointList: IndexedSeq[Point[D]], topology: TetrahedralList)(implicit creator: Create[D]): TetrahedralMesh[D] = {
    creator.createTetrahedraleMesh(UnstructuredPointsDomain(pointList.toIndexedSeq), topology)
  }

  def apply[D: NDSpace](pointSet: UnstructuredPointsDomain[D], topology: TetrahedralList)(implicit creator: Create[D]): TetrahedralMesh[D] = {
    creator.createTetrahedraleMesh(pointSet, topology)
  }

  /** Typeclass for creating domains of arbitrary dimensionality */
  trait Create[D] extends UnstructuredPointsDomain.Create[D] {
    def createTetrahedraleMesh(pointSet: UnstructuredPointsDomain[D], topology: TetrahedralList): TetrahedralMesh[D]
  }

  trait Create3D extends Create[_3D] {
    override def createTetrahedraleMesh(pointSet: UnstructuredPointsDomain[_3D], topology: TetrahedralList): TetrahedralMesh3D = {
      TetrahedralMesh3D(pointSet, topology)
    }
  }

  implicit def parametricToConcreteType3D(tetrahedralMesh: TetrahedralMesh[_3D]): TetrahedralMesh3D = {
    tetrahedralMesh.asInstanceOf[TetrahedralMesh3D]
  }

}

case class TetrahedralMesh3D(pointSet: UnstructuredPointsDomain[_3D], tetrahedralization: TetrahedralList) extends TetrahedralMesh[_3D] {

  // val position = SurfacePointProperty(triangulation, pointSet.points.toIndexedSeq)
  val tetrahedrons: IndexedSeq[TetrahedralCell] = tetrahedralization.tetrahedrons
  val cells: IndexedSeq[TetrahedralCell] = tetrahedrons

  // lazy val operations: TetrahedralMesh3DOperations = TetrahedralMeshOperations(this)

  lazy val operations: TetrahedralMesh3DOperations = MeshOperations(this)

  lazy val boundingBox: BoxDomain[_3D] = pointSet.boundingBox

  /**
   * Applies a point transformation to the point set and returns a new transformed mesh.
   * The method keeps the tetrahedralization as it is and only changes the location of the points.
   *
   * @param transform A function that maps a given point to a new position. All instances of [[scalismo.registration.Transformation]] being descendants of <code>Function1[Point[_3D], Point[_3D] ]</code> are valid arguments.
   */
  override def transform(transform: Point[_3D] => Point[_3D]): TetrahedralMesh3D = {
    TetrahedralMesh3D(pointSet.points.map(transform).toIndexedSeq, tetrahedralization)
  }

  /**
   * Returns the volume of the TetrahedralMesh as sum of all tetrahedrals.
   * For meshes with overlapping tetrahedrals the value will not be correct.
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

    // note: replace call to vtk with own implementation
    val signedVolume = new vtkTetra().ComputeVolume(a.toArray, b.toArray, c.toArray, d.toArray)
    math.abs(signedVolume)
  }

  /** Returns the Barycentric coordinates of a point inside the indicated cell. */
  def getBarycentricCoordinates(point: Point[_3D], tetrathedron: TetrahedralCell): Array[Double] = {

    val a = pointSet.point(tetrathedron.ptId1).toVector
    val b = pointSet.point(tetrathedron.ptId2).toVector
    val c = pointSet.point(tetrathedron.ptId3).toVector
    val d = pointSet.point(tetrathedron.ptId4).toVector

    val barycentricCoordinates = new Array[Double](4)
    val vtkTetra = new vtkTetra()
    vtkTetra.BarycentricCoords(point.toArray, a.toArray, b.toArray, c.toArray, d.toArray, barycentricCoordinates)
    vtkTetra.Delete()
    barycentricCoordinates
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

    val normalized = barycentricCoordinates.map { e => if (Math.abs(e) <= 1E-8) 0.0 else e }
    val numberOfZeroEntries = countZeroEntries(normalized)
    hasOnlyStrictPositiveElements(normalized) || (numberOfZeroEntries == 2) || (numberOfZeroEntries == 3)
  }

  /**
   * Returns a random point lying within the tetrahedron defined by the indicated cell.
   *
   * A uniform distribution is used for sampling points.
   *
   * @param tc   Tetrahedral cell in which to draw a random point
   * @param rnd implicit Random object
   */

}

object TetrahedralMesh3D {
  def apply(points: IndexedSeq[Point[_3D]], topology: TetrahedralList): TetrahedralMesh3D = {
    TetrahedralMesh3D(
      UnstructuredPointsDomain(points.toIndexedSeq),
      topology
    )
  }
}

