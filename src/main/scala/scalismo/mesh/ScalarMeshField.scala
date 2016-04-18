package scalismo.mesh

import scalismo.common.{ DiscreteScalarField, PointId, Scalar, ScalarArray }
import scalismo.geometry._3D

import scala.reflect.ClassTag

/**
 * 3-dimensional triangle mesh with scalar values associated to mesh points.
 *
 * @tparam S type of the scalar values defined over the mesh (Short, Int, Float, Double)
 * @constructor Returns a scalar mesh data given a triangle mesh and an array of values.
 * The number of values and mesh points must be equal.
 */
case class ScalarMeshField[S: Scalar: ClassTag](mesh: TriangleMesh[_3D], override val data: ScalarArray[S]) extends DiscreteScalarField[_3D, S](mesh.domain, data) {
  require(mesh.domain.numberOfPoints == data.size)

  override def values = data.iterator
  override val domain = mesh.domain

  override def apply(ptId: PointId) = data(ptId.id)
  override def isDefinedAt(ptId: PointId) = data.isDefinedAt(ptId.id)

  override def map[S2: Scalar: ClassTag](f: S => S2): ScalarMeshField[S2] = {
    ScalarMeshField(mesh, data.map(f))
  }
}