package smptk
package mesh

import common.Cell
import image.Geometry.{CoordVector3D, Point3D}
import smptk.common.DiscreteDomain

case class TriangleCell(ptId1: Int, ptId2: Int, ptId3: Int) extends Cell {
  val pointIds = Vector(ptId1, ptId2, ptId3)
}

case class TriangleMeshDomain(meshPoints: IndexedSeq[Point3D], val cells: IndexedSeq[TriangleCell]) extends DiscreteDomain[CoordVector3D] {
  def dimensionality = 3
  def numberOfPoints = meshPoints.size
  def points = meshPoints.view
}