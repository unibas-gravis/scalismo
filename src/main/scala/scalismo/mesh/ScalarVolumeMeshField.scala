package scalismo.mesh

import scalismo.common.DiscreteField.ScalarVolumeMeshField
import scalismo.common.{DiscreteField, Scalar, ScalarArray}
import scalismo.geometry._3D

import scala.reflect.ClassTag

object ScalarVolumeMeshField {

  def apply[S: Scalar: ClassTag](mesh: TetrahedralMesh[_3D], data: Traversable[S]): ScalarVolumeMeshField[S] = {
    DiscreteField[_3D, TetrahedralMesh, S](mesh, ScalarArray(data.toArray))
  }

  def apply[S: Scalar: ClassTag](mesh: TetrahedralMesh[_3D], data: ScalarArray[S]): ScalarVolumeMeshField[S] = {
    DiscreteField[_3D, TetrahedralMesh, S](mesh, data)
  }
}
