package smptk
package mesh

import common.DiscreteDomain
import smptk.common.BoxedDomain
import smptk.common.BoxedDomain3D
import smptk.registration.Transformation
import smptk.geometry.{ Point, ThreeD, Point3D }
import smptk.mesh.kdtree.KDTreeMap
import smptk.common.Cell

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

  def compose(transform: Transformation[ThreeD]) = TriangleMesh(points.toIndexedSeq.par.map(transform).toIndexedSeq, cells)

}