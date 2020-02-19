package scalismo.mesh

import scalismo.color.RGBA
import scalismo.geometry.{_3D, Point}

/**
 * colored mesh with RGBA color per vertex
 * @param shape positions
 * @param color color of mesh surface, per point
 */
case class VertexColorMesh3D(shape: TriangleMesh3D, color: SurfacePointProperty[RGBA]) {
  require(shape.triangulation == color.triangulation)

  def transform(trafo: Point[_3D] => Point[_3D]): VertexColorMesh3D = {
    val s = shape.transform { trafo }
    copy(shape = s)
  }
}
