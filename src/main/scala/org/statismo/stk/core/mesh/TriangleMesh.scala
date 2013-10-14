package org.statismo.stk.core
package mesh

import common.DiscreteDomain
import org.statismo.stk.core.common.BoxedDomain
import org.statismo.stk.core.common.BoxedDomain3D
import org.statismo.stk.core.registration.Transformation
import org.statismo.stk.core.geometry.{ Point, ThreeD, Point3D }
import org.statismo.stk.core.mesh.kdtree.KDTreeMap
import org.statismo.stk.core.common.Cell

case class TriangleCell(ptId1: Int, ptId2: Int, ptId3: Int) extends Cell {
  val pointIds = Vector(ptId1, ptId2, ptId3)
}


case class TriangleMesh(meshPoints: IndexedSeq[Point[ThreeD]], val cells: IndexedSeq[TriangleCell]) extends DiscreteDomain[ThreeD] {

  def dimensionality = 3
  def numberOfPoints = meshPoints.size
  def points = meshPoints.view

  val kdTreeMap = KDTreeMap.fromSeq(points.zipWithIndex.toIndexedSeq)

  def findClosestPoint(pt: Point[ThreeD]): (Point[ThreeD], Int) = {
    val nearestPtsAndIndices = (kdTreeMap.findNearest(pt, n = 1))
    val (nearestPt, nearestIdx) = nearestPtsAndIndices(0)

    (points(nearestIdx), nearestIdx)

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

  def warp(transform: Function1[Point[ThreeD], Point[ThreeD]]) = TriangleMesh(meshPoints.par.map(transform).toIndexedSeq, cells)

  val area = cells.map(triangle => computeTriangleArea(triangle)).sum
  
  private def computeTriangleArea(t : TriangleCell) : Double = {
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
  
}