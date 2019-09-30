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
import scalismo.common._
import scalismo.geometry.EuclideanVector._
import scalismo.geometry._
import scalismo.utils.Random

import scala.language.implicitConversions


/** Tetrahedral cell in a tetrahedral mesh. The cell relates 4 points with the given identifiers */
case class TetrahedralCell(ptId1: PointId, ptId2: PointId, ptId3: PointId,ptId4: PointId) extends Cell {
  /** Identifiers of the points belonging to the cell*/
  val pointIds = IndexedSeq(ptId1, ptId2, ptId3,ptId4)

  /** Returns true if the given point identifier is part of the tetrahedral cell*/
  def containsPoint(ptId: PointId) = ptId1 == ptId || ptId2 == ptId || ptId3 == ptId||ptId4 == ptId

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

  //lazy val operations: TriangleMesh3DOperations = MeshOperations(this)

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
   *  Returns the area of the indicated tetrahedral cell.
   */
  def computeTetrahedronArea(t: TetrahedralCell): Double = {
    // compute are of the triangle using heron's formula
    val A = pointSet.point(t.ptId1)
    val B = pointSet.point(t.ptId2)
    val C = pointSet.point(t.ptId3)
    val D= pointSet.point(t.ptId3)

    def areatriangle (A: Point[_3D],B: Point[_3D],C: Point[_3D]):Double= {
      val a = (B - A).norm
      val b = (C - B).norm
      val c = (C - A).norm
      val s = (a + b + c) / 2
      val areaSquared = s * (s - a) * (s - b) * (s - c)
      // it can happen that the area is negative, due to a degenerate triangle.
      if (areaSquared <= 0.0) 0.0 else math.sqrt(areaSquared)
    }
    areatriangle(A,B,C)+areatriangle(A,C,D)+areatriangle(A,B,D)+areatriangle(B,C,D)
  }

/**
  *  Returns true if a given point is inside the tetrahedron defined by the indicated cell.
  *
  *
  */
def isInTetrahedralCell(p: Point[_3D],t: TetrahedralCell)(implicit rnd: Random): Boolean = {
  val A = pointSet.point(t.ptId1)
  val B = pointSet.point(t.ptId2)
  val C = pointSet.point(t.ptId3)
  val D = pointSet.point(t.ptId3)

  def tetracoord(A: Point[_3D],B: Point[_3D],C: Point[_3D],D: Point[_3D]): DenseMatrix[Double]= {

    val v1 = B - A
    val v2 = C - A
    val v3 = D - A

    //mat defines an affine transform from the tetrahedron to the orthogonal system
    val mat = DenseMatrix(v1.toArray, v2.toArray, v3.toArray).t
    //The inverse matrix does the opposite (from orthogonal to tetrahedron)
    val SVD(u_1, d_1, vt_1) = breeze.linalg.svd(mat)
    val Dinv_1 = d_1.map(d => if (d > 1e-6) 1.0 / d else 0.0)
    val x = vt_1.t * breeze.linalg.diag(Dinv_1) * u_1.t
    x
  }

 def pointInside(A: Point[_3D],B: Point[_3D],C: Point[_3D],D: Point[_3D],p:Point[_3D]):Boolean={
   //Find the transform matrix from orthogonal to tetrahedron system
   val M1=tetracoord(A,B,C,D)
   //apply the transform to P
   val newp=M1*(p-A).toBreezeVector
   //perform test
  if ((newp(0)>=0 && newp(1)>=0 &&newp(2)>=0 )&&(newp(0)<=1 && newp(1)<=10 &&newp(2)<=0 )&&(newp.toScalaVector().toIterator.sum<=1)){
    true
  }else{
    false
  }

 }

  pointInside(A,B,C,D,p)
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

    val centroid=(A.toVector+B.toVector+C.toVector+D.toVector)*(1.0/4)

    var p=Point3D(centroid(0)*u,centroid(1)*d,centroid(2)*z)

    while (isInTetrahedralCell(p,t)==false){
      val u = rnd.scalaRandom.nextDouble()
      val d = rnd.scalaRandom.nextDouble()
      val z = rnd.scalaRandom.nextDouble()

      p=Point3D(centroid(0)*u,centroid(1)*d,centroid(2)*z)
    }
  p
  }

}

object TetrahedralMesh3D {
  def apply(points: IndexedSeq[Point[_3D]], topology: TetrahedralList): TetrahedralMesh3D = {
    TetrahedralMesh3D(UnstructuredPointsDomain(points.toIndexedSeq), topology)
  }
}

