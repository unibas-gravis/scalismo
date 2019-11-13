package scalismo.tetramesh

import scalismo.common.{ DiscreteScalarField, PointId, Scalar, ScalarArray, UnstructuredPointsDomain }
import scalismo.geometry.{ Point, _3D }
import scalismo.mesh.ScalarMeshField

import scala.reflect.ClassTag

case class ScalarVolumeMeshField[S: Scalar: ClassTag](mesh: TetrahedralMesh[_3D], override val data: ScalarArray[S])
    extends DiscreteScalarField[_3D, UnstructuredPointsDomain[_3D], S](mesh.pointSet, data) {
  require(mesh.pointSet.numberOfPoints == data.size)

  override def values = data.iterator

  override val domain = mesh.pointSet

  override def apply(ptId: PointId) = data(ptId.id)

  override def isDefinedAt(ptId: PointId) = data.isDefinedAt(ptId.id)

  override def map[S2: Scalar: ClassTag](f: S => S2): ScalarVolumeMeshField[S2] = {
    ScalarVolumeMeshField(mesh, data.map(f))
  }

  def transform(transformation: Point[_3D] => Point[_3D]): ScalarVolumeMeshField[S] = {
    ScalarVolumeMeshField(mesh.transform(transformation), data)
  }

}

object ScalarVolumeMeshField {
  def apply[S: Scalar: ClassTag](mesh: TetrahedralMesh[_3D], data: Traversable[S]): ScalarVolumeMeshField[S] = {
    ScalarVolumeMeshField(mesh, ScalarArray(data.toArray))
  }
}

