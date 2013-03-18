package smptk
package mesh

import common.Cell
import image.Geometry.Point3D

case class TriangleCell(ptId1: Int, ptId2: Int, ptId3: Int) extends Cell {
  val pointIds = Vector(ptId1, ptId2, ptId3)
}

case class TriangleMeshDomain(val points: IndexedSeq[Point3D], val cells: IndexedSeq[TriangleCell]) {
}