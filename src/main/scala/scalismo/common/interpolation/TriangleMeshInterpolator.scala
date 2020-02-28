package scalismo.common.interpolation

import scalismo.common.{DiscreteField, Field, RealSpace}
import scalismo.geometry.{_3D, Point}
import scalismo.mesh.{SurfacePointProperty, TriangleMesh}
import scalismo.mesh.boundingSpheres.{ClosestPointInTriangle, ClosestPointIsVertex, ClosestPointOnLine}
import scalismo.numerics.ValueInterpolator

import scala.language.higherKinds

/**
 * Interpolates a given discrete field defined on the vertices
 * of a triangle mesh (i.e. a MeshField) by means of a surface interpolation
 * on the surface defined by the mesh.
 */
case class TriangleMeshInterpolator[A: ValueInterpolator]() extends FieldInterpolator[_3D, TriangleMesh, A] {

  override def interpolate(field: DiscreteField[_3D, TriangleMesh, A]): Field[_3D, A] = {
    val mesh = field.domain

    // for this method to make sense, the field needs to be defined
    // on the mesh. There is no good way to check this rigorously.
    // A simple sanity check is, however, that the number of points
    // of the domain is the same.
    require(mesh.pointSet.numberOfPoints == field.values.size)

    val fieldOnSurface =
      SurfacePointProperty(mesh.triangulation, field.values.toIndexedSeq)

    val f = (pt: Point[_3D]) =>
      mesh.operations.closestPointOnSurface(pt) match {
        case ClosestPointIsVertex(pt, _, id) => fieldOnSurface(id)
        case ClosestPointInTriangle(pt, _, id, bc) =>
          fieldOnSurface(id, bc)
        case ClosestPointOnLine(pt, _, (id0, id1), d) =>
          ValueInterpolator[A].blend(fieldOnSurface(id0), fieldOnSurface(id1), d)
      }
    Field(RealSpace[_3D], f)
  }
}
