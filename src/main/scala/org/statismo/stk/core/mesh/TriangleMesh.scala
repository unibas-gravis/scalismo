package org.statismo.stk.core
package mesh

import org.statismo.stk.core.common._
import org.statismo.stk.core.geometry.{ Point, _3D, Vector }
import org.statismo.stk.core.common.Cell
import scala.reflect.ClassTag
import org.statismo.stk.core.common.BoxDomain
import scala.collection.mutable

import spire.math.Numeric

/** Triangle cell in a triangle mesh. The cell relates 3 points with the given identifiers */
case class TriangleCell(ptId1: Int, ptId2: Int, ptId3: Int) extends Cell {
  
  /** Identifiers of the points belonging to the cell*/
  val pointIds = IndexedSeq(ptId1, ptId2, ptId3)

  /** Returns true if the given point identifier is part of the triangle cell*/
  def containsPoint(ptId: Int) = ptId1 == ptId || ptId2 == ptId || ptId3 == ptId
}
/**
 * 3-dimensional triangle mesh. 
 * 
 * Triangle meshes are currently the only supported representation of 3-dimensional meshes in the library.
 * 
 * */
class TriangleMesh private (meshPoints: IndexedSeq[Point[_3D]], val cells: IndexedSeq[TriangleCell], cellMapOpt: Option[mutable.HashMap[Int, Seq[TriangleCell]]]) 
	extends SpatiallyIndexedFiniteDiscreteDomain[_3D](meshPoints, meshPoints.size) {

  // a map that has for every point the neighboring cell ids
  private[this] val cellMap: mutable.HashMap[Int, Seq[TriangleCell]] = cellMapOpt.getOrElse(mutable.HashMap())

  private[this] def updateCellMapForPtId(ptId: Int, cell: TriangleCell): Unit = {
    val cellsForKey = cellMap.getOrElse(ptId, Seq[TriangleCell]())
    cellMap.update(ptId, cellsForKey :+ cell)
  }

  if (!cellMapOpt.isDefined)
    for (cell <- cells) {
      cell.pointIds.foreach(id => updateCellMapForPtId(id, cell))
    }
 
   /**
   * Returns the smallest continuous box domain that fully contains the triangle mesh.
   * 
   * The bounding box is always oriented along the dimensions of the space (i.e. this method does not return rotated boxes) 
   * */
  def boundingBox: BoxDomain[_3D] = {
    val minx = points.map(_(0)).min
    val miny = points.map(_(1)).min
    val minz = points.map(_(2)).min
    val maxx = points.map(_(0)).max
    val maxy = points.map(_(1)).max
    val maxz = points.map(_(2)).max
    BoxDomain[_3D](Point(minx, miny, minz), Point(maxx, maxy, maxz))
  }

  /** 
   *  Returns a triangle mesh that is the image of this mesh by the given transform.
   *  
   *  This method maps all mesh points to their images by the given transform while maintaining the same triangle cell relations.
   *  
   *  @param transform A function that maps a given point to a new position. All instances of [[Transformation]] being descendants of <code>Function1[Point[_3D], Point[_3D]]</code> are valid arguments. 
   *  */
  def warp(transform: Point[_3D] => Point[_3D]) = new TriangleMesh(meshPoints.par.map(transform).toIndexedSeq, cells, Some(cellMap))

   /**
   * Returns the identifiers of the mesh cells to which the given point identifier belongs */
  def cellsWithPointId(id: Int): Seq[TriangleCell] = cellMap(id)

  /** Returns a 3D vector that is orthogonal to the triangle defined by the cell points*/  
  def computeCellNormal(cell: TriangleCell): Vector[_3D] = {
    val pt1 = meshPoints(cell.ptId1)
    val pt2 = meshPoints(cell.ptId2)
    val pt3 = meshPoints(cell.ptId3)

    val u = pt2 - pt1
    val v = pt3 - pt1
    Vector.crossproduct(u, v)
  }
  
  
  /** 
   *  Returns surface normal at the closest mesh point to the indicated argument point.
   *  
   *  @param pt Point at which to evaluate the surface normal. Note that it does not need to be one of the mesh points.
   *  The returned vector is the normal at the closest mesh point to this point.  
   *  */
  def normalAtPoint(pt: Point[_3D]): Vector[_3D] = {
    val closestMeshPtId = findClosestPoint(pt)._2
    val neigborCells = cellsWithPointId(closestMeshPtId)
    val normalUnnormalized = neigborCells.foldLeft(Vector(0f, 0f, 0f))((acc, cell) => acc + computeCellNormal(cell)) * (1.0 / neigborCells.size)
    normalUnnormalized * (1.0 / normalUnnormalized.norm)
  }

  /** Area of the mesh surface.
   *  
   *  The computed area is the sum of all the triangle cell areas.
   *  */
  lazy val area = cells.map(triangle => computeTriangleArea(triangle)).sum

  
  /** 
   *  Returns the area of the indicated triangle cell.
   *  */
  def computeTriangleArea(t: TriangleCell): Double = {
    // compute are of the triangle using heron's formula
    val A = meshPoints(t.ptId1)
    val B = meshPoints(t.ptId2)
    val C = meshPoints(t.ptId3)
    val a = (B - A).norm
    val b = (C - B).norm
    val c = (C - A).norm
    val s = (a + b + c) / 2
    val areaSquared = s * (s - a) * (s - b) * (s - c)
    // it can happen that the area is negative, due to a degenerate triangle.
    if (areaSquared <= 0.0) 0.0 else math.sqrt(areaSquared)
  }

  
  /** 
   *  Returns a random point lying within the triangle defined by the indicated cell.
   *  
   *  A uniform distribution is used for sampling points.
   *  
   *  @param t Triangle cell in which to draw a random point
   *  @param seed Seed value for the random generator 
   *  */
  def samplePointInTriangleCell(t: TriangleCell, seed: Int): Point[_3D] = {
    val A = meshPoints(t.ptId1).toVector
    val B = meshPoints(t.ptId2).toVector
    val C = meshPoints(t.ptId3).toVector

    val rand = new scala.util.Random(seed)
    val u = rand.nextFloat()
    val d = rand.nextFloat()
    val v = if (d + u <= 1) d else 1 - u

    val s = A * u + B * v + C * (1 - (u + v))
    Point(s(0), s(1), s(2))
  }

  override lazy val hashCode = super.hashCode
}

/**
 * Factory for [[TriangleMesh]] instances.
 * */
object TriangleMesh {
  /**
   * Returns a 3D triangle mesh defined by the indicated points and cells
   * 
   * @param meshPoints Sequence of points defining the triangle mesh.
   * @param cells Sequence of the triangle cells defined over the mesh points. 
   * The identifiers used for defining the cells are the indices of points in meshPoints.    
   * */
  def apply(meshPoints: IndexedSeq[Point[_3D]], cells: IndexedSeq[TriangleCell]) = new TriangleMesh(meshPoints, cells, None)
}

/**
 * 3-dimensional triangle mesh with scalar values associated to mesh points.
 * @tparam S type of the scalar values defined over the mesh (Short, Int, Float, Double)
 * 
 * @constructor Returns a scalar mesh data given a triangle mesh and an array of values. 
 * The number of values and mesh points must be equal.
 * */
case class ScalarMeshData[S: Numeric: ClassTag](mesh: TriangleMesh, values: Array[S]) extends ScalarPointData[_3D, S] {
  require(mesh.numberOfPoints == values.size)

  override def numeric = implicitly[Numeric[S]]

  override val domain = mesh

  override def map[S2: Numeric: ClassTag](f: S => S2): ScalarMeshData[S2] = {
    ScalarMeshData(mesh, values.map(f))
  }
}

