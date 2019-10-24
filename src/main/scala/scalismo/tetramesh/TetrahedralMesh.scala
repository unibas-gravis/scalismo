/*
 * Copyright 2015 University of Basel, Graphics and Vision Research Group
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
package scalismo.tetramesh

import breeze.linalg.svd.SVD
import breeze.linalg.{DenseMatrix, DenseVector}
import com.jogamp.graph.geom.Triangle
import scalismo.common._
import scalismo.geometry._
import scalismo.mesh.TriangleCell
import scalismo.utils.Random
import vtk.vtkTetra

import scala.language.implicitConversions

/** Tetrahedral cell in a tetrahedral mesh. The cell relates 4 points with the given identifiers */
case class TetrahedralCell(ptId1: PointId, ptId2: PointId, ptId3: PointId, ptId4: PointId) extends Cell {
  /** Identifiers of the points belonging to the cell*/
  val pointIds = IndexedSeq(ptId1, ptId2, ptId3, ptId4)
  val triangles= List(TriangleCell(ptId1,ptId2,ptId3),TriangleCell(ptId1,ptId2,ptId4),
    TriangleCell(ptId1,ptId3,ptId4),TriangleCell(ptId2,ptId3,ptId4))

  /** Returns true if the given point identifier is part of the tetrahedral cell*/
  def containsPoint(ptId: PointId) = ptId1 == ptId || ptId2 == ptId || ptId3 == ptId || ptId4 == ptId

  def toIntVector4D = DenseVector[Int](ptId1.id, ptId2.id, ptId3.id, ptId4.id)
}

trait TetrahedralMesh[D] {
  def tetrahedralization: TetrahedralList
  def pointSet: UnstructuredPointsDomain[D]
  def transform(transform: Point[D] => Point[D]): TetrahedralMesh[D]

}

object TetrahedralMesh {

  def apply[D: NDSpace](pointSet: UnstructuredPointsDomain[D], topology: TetrahedralList)(implicit creator: Create[D]) = {
    creator.createTetrahedraleMesh(pointSet, topology)
  }

  def apply[D: NDSpace](points: IndexedSeq[Point[D]], topology: TetrahedralList)(implicit creator: Create[D]) = {
    creator.createTetrahedraleMesh(UnstructuredPointsDomain(points.toIndexedSeq), topology)
  }

  /** Typeclass for creating domains of arbitrary dimensionality */
  trait Create[D] extends UnstructuredPointsDomain.Create[D] {
    def createTetrahedraleMesh(pointSet: UnstructuredPointsDomain[D], topology: TetrahedralList): TetrahedralMesh[D]
  }

  trait Create3D extends Create[_3D] {
    override def createTetrahedraleMesh(pointSet: UnstructuredPointsDomain[_3D], topology: TetrahedralList) = {
      TetrahedralMesh3D(pointSet, topology)
    }
  }

  implicit def parametricToConcreteType3D(tetrahedralMesh: TetrahedralMesh[_3D]): TetrahedralMesh3D = {
    tetrahedralMesh.asInstanceOf[TetrahedralMesh3D]
  }

}

/** Standard 3D Gravis mesh, geometry only */

case class TetrahedralMesh3D(pointSet: UnstructuredPointsDomain[_3D], tetrahedralization: TetrahedralList) extends TetrahedralMesh[_3D] {

  // val position = SurfacePointProperty(triangulation, pointSet.points.toIndexedSeq)
  val tetrahedrons = tetrahedralization.tetrahedrons
  val cells = tetrahedrons

  lazy val operations: TetrahedralMesh3DOperations = TetrahedralMeshOperations(this)

  lazy val boundingBox = pointSet.boundingBox

  /**
   *  Returns a triangle mesh that is the image of this mesh by the given transform.
   *
   *  This method maps all mesh points to their images by the given transform while maintaining the same triangle cell relations.
   *
   *  @param transform A function that maps a given point to a new position. All instances of [[scalismo.registration.Transformation]] being descendants of <code>Function1[Point[_3D], Point[_3D] ]</code> are valid arguments.
   */
  override def transform(transform: Point[_3D] => Point[_3D]): TetrahedralMesh3D = {
    TetrahedralMesh3D(pointSet.points.map(transform).toIndexedSeq, tetrahedralization)
  }
  /**
   *  Returns the volume of the TetrahedralMesh.
   */
  lazy val volume: Double = {
    var sum = 0.0
    tetrahedrons.foreach(t => sum += computeTrahedronVolume(t))
    sum
  }

  /**
   *  Returns the area of the indicated tetrahedral cell.
   */
  def computeTetrahedronArea(t: TetrahedralCell): Double = {
    // compute are of the triangle using heron's formula
    val A = pointSet.point(t.ptId1)
    val B = pointSet.point(t.ptId2)
    val C = pointSet.point(t.ptId3)
    val D = pointSet.point(t.ptId3)

    def areatriangle(A: Point[_3D], B: Point[_3D], C: Point[_3D]): Double = {
      val a = (B - A).norm
      val b = (C - B).norm
      val c = (C - A).norm
      val s = (a + b + c) / 2
      val areaSquared = s * (s - a) * (s - b) * (s - c)
      // it can happen that the area is negative, due to a degenerate triangle.
      if (areaSquared <= 0.0) 0.0 else math.sqrt(areaSquared)
    }
    areatriangle(A, B, C) + areatriangle(A, C, D) + areatriangle(A, B, D) + areatriangle(B, C, D)
  }

  /**
   *  Returns the volume of the indicated tetrahedral cell.
   */

  def computeTrahedronVolume(terahedron: TetrahedralCell): Double = {
    val a = pointSet.point(terahedron.ptId1)
    val b = pointSet.point(terahedron.ptId2)
    val c = pointSet.point(terahedron.ptId3)
    val d = pointSet.point(terahedron.ptId4)

    val tetrahedron = new vtkTetra()
    Math.abs(tetrahedron.ComputeVolume(a.toArray,b.toArray,c.toArray,d.toArray))
  }

  /**
   *  Returns true if a given point is inside the tetrahedron defined by the indicated cell.
   *
   *
   */
  def isInsideTetrahedralCell(p: Point[_3D], t: TetrahedralCell): Boolean = {

   def allposif(v: DenseVector[Double]): Boolean = {
      if (v(0) > 0.0 && v(1) > 0.0 && v(2) > 0.0 && v(3) > 0.0) {
        true
      } else {
        false
      }

    }

    def threearezero(v: DenseVector[Double]): Boolean = {
      if ((v(0) == 0.0 && v(1) == 0.0 && v(2) == 0.0 && v(3) == 1.0) ||
        (v(0) == 0.0 && v(1) == 0.0 && v(2) == 1.0 && v(3) == 0.0) ||
        (v(0) == 0.0 && v(1) == 1.0 && v(2) == 0.0 && v(3) == 0.0) ||
        (v(0) == 1.0 && v(1) == 0.0 && v(2) == 0.0 && v(3) == 0.0)) {

        true

      } else {
        false
      }
    }


    def twoarezero(v: DenseVector[Double]): Boolean = {
      if((v(0) == 0.0 && v(1) == 0.0 && v(2) != 0.0 && v(3) != 0.0) ||
         (v(0) == 0.0 && v(1) != 0.0 && v(2) == 0.0 && v(3) != 0.0) ||
         (v(0) == 0.0 && v(1) != 0.0 && v(2) != 0.0 && v(3) == 0.0) ||
         (v(0) != 0.0 && v(1) == 0.0 && v(2) == 0.0 && v(3) != 0.0) ||
         (v(0) != 0.0 && v(1) == 0.0 && v(2) != 0.0 && v(3) == 0.0) ||
         (v(0) != 0.0 && v(1) != 0.0 && v(2) == 0.0 && v(3) == 0.0)) {

        true

      } else {
        false
      }

    }

    val a = pointSet.point(t.ptId1).toVector
    val b = pointSet.point(t.ptId2).toVector
    val c = pointSet.point(t.ptId3).toVector
    val d = pointSet.point(t.ptId4).toVector




    val bcoord= new Array[Double](4)//this to initialised the array where the result will be stored
    val tetrahedron = new vtkTetra()
    tetrahedron.BarycentricCoords(p.toArray,a.toArray,b.toArray,c.toArray,d.toArray,bcoord)
    val vec=DenseVector[Double](bcoord.apply(0),bcoord.apply(1),bcoord.apply(2),bcoord.apply(3))


    val normalisedvec=vec.map{e=>if ((e>= -1E-50)&&(e<=1E-50)) 0.0 else e}


    if (allposif(normalisedvec)) {
      true
    } else if (threearezero(normalisedvec)) {
      true
    } else if (twoarezero(normalisedvec)) {
      true
    } else {
      false
    }

  }

  /**
   *  Returns a random point lying within the tetrahedron defined by the indicated cell.
   *
   *  A uniform distribution is used for sampling points.
   *
   *  @param t Tetrahedral cell in which to draw a random point
   *  @param rnd implicit Random object
   */

  def samplePointInTetrahedralCell(t: TetrahedralCell)(implicit rnd: Random): Point[_3D] = {
    val A = pointSet.point(t.ptId1)
    val B = pointSet.point(t.ptId2)
    val C = pointSet.point(t.ptId3)
    val D = pointSet.point(t.ptId3)

    val u = rnd.scalaRandom.nextDouble()
    val d = rnd.scalaRandom.nextDouble()
    val z = rnd.scalaRandom.nextDouble()

    val centroid = (A.toVector + B.toVector + C.toVector + D.toVector) * (1.0 / 4)

    var p = Point3D(centroid(0) * u, centroid(1) * d, centroid(2) * z)

    while (isInsideTetrahedralCell(p, t) == false) {
      val u = rnd.scalaRandom.nextDouble()
      val d = rnd.scalaRandom.nextDouble()
      val z = rnd.scalaRandom.nextDouble()

      p = Point3D(centroid(0) * u, centroid(1) * d, centroid(2) * z)
    }
    p
  }

}

object TetrahedralMesh3D {
  def apply(points: IndexedSeq[Point[_3D]], topology: TetrahedralList): TetrahedralMesh3D = {
    TetrahedralMesh3D(UnstructuredPointsDomain(points.toIndexedSeq), topology)
  }
}

