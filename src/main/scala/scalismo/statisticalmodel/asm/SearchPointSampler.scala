package scalismo.statisticalmodel.asm

import scalismo.geometry.{ _3D, Point }
import scalismo.mesh.TriangleMesh

import scala.collection.immutable

trait SearchPointSampler extends Function2[TriangleMesh, Int, immutable.Seq[Point[_3D]]] {

}

case class NormalDirectionSearchPointSampler(numberOfPoints: Int, searchDistance: Float) extends SearchPointSampler {

  override def apply(mesh: TriangleMesh, pointId: Int): immutable.Seq[Point[_3D]] = {
    val point = mesh.points(pointId)
    val interval = searchDistance * 2 / numberOfPoints

    val normalUnnormalized = mesh.normalAtPoint(point)
    val normal = normalUnnormalized * (1.0 / normalUnnormalized.norm)

    def samplePointsOnNormal(): immutable.Seq[Point[_3D]] = {
      //val interval = distToSearch * 2 / numPts.toFloat
      for (i <- -numberOfPoints / 2 to numberOfPoints / 2) yield {
        point + normal * i * interval
      }
    }

    samplePointsOnNormal()
  }
}

