package smptk
package mesh

import common.DiscreteDomain
import image.Geometry.CoordVector3D
import smptk.common.BoxedRegion
import smptk.common.BoxedRegion3D
import smptk.registration.Transformation

case class TriangleMesh(val domain : TriangleMeshDomain)  {
  //def pointData : List[DataArray] = List()
  
  def boundingBox : BoxedRegion3D = {
	val minx = domain.points.map(_(0)).min
	val miny = domain.points.map(_(1)).min
	val minz = domain.points.map(_(2)).min
	val maxx = domain.points.map(_(0)).max
	val maxy = domain.points.map(_(1)).max
	val maxz = domain.points.map(_(2)).max
	BoxedRegion3D(CoordVector3D(minx, miny, minz), CoordVector3D(maxx, maxy, maxz))
  }
  
  def compose(transform : Transformation[CoordVector3D]) = TriangleMesh(TriangleMeshDomain( domain.points.toIndexedSeq.map(transform), domain.cells))
  
}