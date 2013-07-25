package smptk
package mesh

import common.DiscreteDomain
import smptk.common.BoxedRegion
import smptk.common.BoxedRegion3D
import smptk.registration.Transformation
import smptk.geometry.{Point, ThreeD, Point3D}

case class TriangleMesh(val domain : TriangleMeshDomain)  {
  //def pointData : List[DataArray] = List()
  
  def boundingBox : BoxedRegion3D = {
	val minx = domain.points.map(_(0)).min
	val miny = domain.points.map(_(1)).min
	val minz = domain.points.map(_(2)).min
	val maxx = domain.points.map(_(0)).max
	val maxy = domain.points.map(_(1)).max
	val maxz = domain.points.map(_(2)).max
	BoxedRegion3D(Point3D(minx, miny, minz), Point3D(maxx, maxy, maxz))
  }
  
  def findClosestPoint(pt : Point[ThreeD]) = domain.findClosestPoint(pt)
  
  def compose(transform : Transformation[ThreeD]) = TriangleMesh(TriangleMeshDomain( domain.points.toIndexedSeq.par.map(transform).toIndexedSeq, domain.cells))
  
}