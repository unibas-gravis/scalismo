package smptk
package mesh

import common.Cell
import smptk.common.DiscreteDomain
import kdtree.DimensionalOrdering._
import smptk.mesh.kdtree.KDTreeMap
import geometry.{Point, ThreeD, Point3D}

case class TriangleCell(ptId1: Int, ptId2: Int, ptId3: Int) extends Cell {
  val pointIds = Vector(ptId1, ptId2, ptId3)
}

case class TriangleMeshDomain(meshPoints: IndexedSeq[Point[ThreeD]], val cells: IndexedSeq[TriangleCell]) extends DiscreteDomain[ThreeD] {
  def dimensionality = 3
  def numberOfPoints = meshPoints.size
  def points = meshPoints.view
  

  val kdTreeMap = KDTreeMap.fromSeq(points.zipWithIndex.toIndexedSeq)

  def findClosestPoint(pt : Point[ThreeD]) : (Point[ThreeD], Int) = {
    val nearestPtsAndIndices = (kdTreeMap.findNearest(pt, n = 1))
    val (nearestPt, nearestIdx) = nearestPtsAndIndices(0)

    (points(nearestIdx), nearestIdx)

  }
}
