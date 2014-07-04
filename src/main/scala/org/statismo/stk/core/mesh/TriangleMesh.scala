package org.statismo.stk.core
package mesh

import org.statismo.stk.core.common._
import org.statismo.stk.core.geometry.{ Point, _3D}
import org.statismo.stk.core.common.Cell
import scala.reflect.ClassTag
import scala.collection.mutable.HashMap
import org.statismo.stk.core.common.BoxedDomain3D
import org.statismo.stk.core.geometry.Point
import org.statismo.stk.core.geometry.Vector


case class TriangleCell(ptId1: Int, ptId2: Int, ptId3: Int) extends Cell {
  val pointIds = IndexedSeq(ptId1, ptId2, ptId3)

  def containsPoint(ptId: Int) = ptId1 == ptId || ptId2 == ptId || ptId3 == ptId
}



case class TriangleMesh private (meshPoints: IndexedSeq[Point[_3D]], val cells: IndexedSeq[TriangleCell], cellMapOpt: Option[HashMap[Int, Seq[TriangleCell]]]) extends UnstructuredPointsDomainBase[_3D](meshPoints) {

  // a map that has for every point the neighboring cell ids
  private[this] val cellMap: HashMap[Int, Seq[TriangleCell]] = cellMapOpt.getOrElse(HashMap())


  private[this] def updateCellMapForPtId(ptId: Int, cell: TriangleCell): Unit = {
    val cellsForKey = cellMap.getOrElse(ptId, Seq[TriangleCell]())
    cellMap.update(ptId, cellsForKey :+ cell)
  }


  if (!cellMapOpt.isDefined)
    for (cell <- cells) {
      cell.pointIds.foreach(id => updateCellMapForPtId(id, cell))
    }

  //verify that there all points belong to a cell
  //require(cellMap.size == meshPoints.size, { println("Provided mesh data contains points not belonging to any cell !") })


  def cellsWithPt(ptId: Int) = cells.filter(_.containsPoint(ptId))

  def boundingBox: BoxedDomain3D = {
    val minx = points.map(_(0)).min
    val miny = points.map(_(1)).min
    val minz = points.map(_(2)).min
    val maxx = points.map(_(0)).max
    val maxy = points.map(_(1)).max
    val maxz = points.map(_(2)).max
    BoxedDomain3D(Point(minx, miny, minz), Point(maxx, maxy, maxz))
  }

  def warp(transform: Function1[Point[_3D], Point[_3D]]) = new TriangleMesh(meshPoints.par.map(transform).toIndexedSeq, cells, Some(cellMap))

  def cellNeighbors(id: Int): Seq[TriangleCell] = cellMap(id)

  def computeCellNormal(cell: TriangleCell): Vector[_3D] = {
    val pt1 = meshPoints(cell.ptId1)
    val pt2 = meshPoints(cell.ptId2)
    val pt3 = meshPoints(cell.ptId3)

    val u = pt2 - pt1
    val v = pt3 - pt1
    Vector.crossproduct(u, v)
  }

  def normalAtPoint(pt: Point[_3D]): Vector[_3D] = {
    val closestMeshPtId = findClosestPoint(pt)._2
    val neigborCells = cellNeighbors(closestMeshPtId)
    val normalUnnormalized = neigborCells.foldLeft(Vector(0f, 0f, 0f))((acc, cell) => acc + computeCellNormal(cell)) * (1.0 / neigborCells.size)
    normalUnnormalized * (1.0 / normalUnnormalized.norm)
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

  def samplePointInTriangleCell(t: TriangleCell): Point[_3D] = {
    val A = meshPoints(t.ptId1) - Point(0, 0, 0f)
    val B = meshPoints(t.ptId2) - Point(0, 0, 0f)
    val C = meshPoints(t.ptId3) - Point(0f, 0f, 0f)

    val u = scala.util.Random.nextFloat()
    val d = scala.util.Random.nextFloat()
    val v = if (d + u <= 1) d else 1 - u

    val s = A * u + B * v + C * (1 - (u + v))
    Point(s(0), s(1), s(2))
  }
}


object TriangleMesh {
  def apply(meshPoints: IndexedSeq[Point[_3D]], cells: IndexedSeq[TriangleCell]) = new TriangleMesh(meshPoints, cells, None)

}


case class ScalarMeshData[S: ScalarValue: ClassTag](val mesh: TriangleMesh, val values: Array[S]) extends ScalarPointData[_3D, S] {
  require(mesh.numberOfPoints == values.size)
  val valueDimensionality = 1
  override val domain = mesh

  override def map[S2: ScalarValue: ClassTag](f: S => S2): ScalarPointData[_3D, S2] = {
    ScalarMeshData(mesh, values.map(f))
  }
}

