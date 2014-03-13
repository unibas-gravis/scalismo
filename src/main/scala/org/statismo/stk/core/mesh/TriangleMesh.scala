package org.statismo.stk.core
package mesh

import common.DiscreteDomain
import org.statismo.stk.core.common.BoxedDomain
import org.statismo.stk.core.common.BoxedDomain3D
import org.statismo.stk.core.registration.Transformation
import org.statismo.stk.core.geometry.{ Point, ThreeD, Point3D }
import org.statismo.stk.core.mesh.kdtree.KDTreeMap
import org.statismo.stk.core.common.Cell
import org.statismo.stk.core.common.PointData
import org.statismo.stk.core.common.PointData
import scala.reflect.ClassTag
import org.statismo.stk.core.common.ScalarValue
import org.statismo.stk.core.geometry.Vector3D
import org.statismo.stk.core.geometry.DimTraits
import scala.collection.mutable.HashMap

case class TriangleCell(ptId1: Int, ptId2: Int, ptId3: Int) extends Cell {
  val pointIds = Vector(ptId1, ptId2, ptId3)

  def containsPoint(ptId: Int) = ptId1 == ptId || ptId2 == ptId || ptId3 == ptId
}

case class TriangleMesh(meshPoints: IndexedSeq[Point[ThreeD]], val cells: IndexedSeq[TriangleCell]) extends DiscreteDomain[ThreeD] {

  def dimensionality = 3
  def numberOfPoints = meshPoints.size
  def points = meshPoints.view

  def cellsWithPt(ptId: Int) = cells.filter(_.containsPoint(ptId))

  // a map that has for every point the neighboring cell ids
  private[this] val cellMap: HashMap[Int, Seq[TriangleCell]] = HashMap()

  private[this] def updateCellMapForPtId(ptId: Int, cell : TriangleCell): Unit = {
    val cellsForKey = cellMap.getOrElse(ptId, Seq[TriangleCell]())
    cellMap.update(ptId, cellsForKey :+ cell)
  }
  for (cell <- cells) {
    cell.pointIds.foreach(id => updateCellMapForPtId(id, cell))
  }
  
  //verify that there all points belong to a cell
  require(cellMap.size == meshPoints.size, {println("Provided mesh data contains points not belonging to any cell !")}) 

  private[this] lazy val kdTreeMap = KDTreeMap.fromSeq(points.zipWithIndex.toIndexedSeq)

  def isDefinedAt(pt: Point[ThreeD]) = {
    val (closestPt, _) = findClosestPoint(pt)
    closestPt == pt
  }

  def findClosestPoint(pt: Point[ThreeD]): (Point[ThreeD], Int) = {
    val nearestPtsAndIndices = (kdTreeMap.findNearest(pt, n = 1))
    nearestPtsAndIndices(0)
  }

  def findNClosestPoints(pt: Point[ThreeD], n: Int): Seq[(Point[ThreeD], Int)] = {
    kdTreeMap.findNearest(pt, n)
  }

  def boundingBox: BoxedDomain3D = {
    val minx = points.map(_(0)).min
    val miny = points.map(_(1)).min
    val minz = points.map(_(2)).min
    val maxx = points.map(_(0)).max
    val maxy = points.map(_(1)).max
    val maxz = points.map(_(2)).max
    BoxedDomain3D(Point3D(minx, miny, minz), Point3D(maxx, maxy, maxz))
  }

  def warp(transform: Function1[Point[ThreeD], Point[ThreeD]]) = new TriangleMesh(meshPoints.par.map(transform).toIndexedSeq, cells)

  def cellNeighbors(id: Int): Seq[TriangleCell] = cellMap(id)

  def computeCellNormal(cell: TriangleCell): Vector3D = {
    val pt1 = meshPoints(cell.ptId1)
    val pt2 = meshPoints(cell.ptId2)
    val pt3 = meshPoints(cell.ptId3)

    val u = pt2 - pt1
    val v = pt3 - pt1
    u.asInstanceOf[Vector3D].cross(v.asInstanceOf[Vector3D])
  }

  def normalAtPoint(pt: Point[ThreeD]): Vector3D = {
    val closestMeshPtId = findClosestPoint(pt)._2
    val neigborCells = cellNeighbors(closestMeshPtId)
    neigborCells.foldLeft(Vector3D(0, 0, 0))((acc, cell) => acc + computeCellNormal(cell)) * (1.0 / neigborCells.size)
  }

  lazy val area = cells.map(triangle => computeTriangleArea(triangle)).sum

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

  def samplePointInTriangleCell(t: TriangleCell): Point[ThreeD] = {
    val A = meshPoints(t.ptId1) - Point3D(0, 0, 0)
    val B = meshPoints(t.ptId2) - Point3D(0, 0, 0)
    val C = meshPoints(t.ptId3) - Point3D(0, 0, 0)

    val u = scala.util.Random.nextFloat()
    val d = scala.util.Random.nextFloat()
    val v = if (d + u <= 1) d else 1 - u

    val s = A * u + B * v + C * (1 - (u + v))
    Point3D(s(0), s(1), s(2))
  }
}

case class ScalarMeshData[S: ScalarValue: ClassTag](val mesh: TriangleMesh, val values: Array[S]) extends PointData[ThreeD, S] {
  require(mesh.numberOfPoints == values.size)
  val valueDimensionality = 1
  override val domain = mesh

  override def map[S2: ScalarValue: ClassTag](f: S => S2): PointData[ThreeD, S2] = {
    ScalarMeshData(mesh, values.map(f))
  }
}

