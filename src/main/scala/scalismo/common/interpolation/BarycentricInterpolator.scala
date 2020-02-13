package scalismo.common.interpolation

import scalismo.common._
import scalismo.geometry.{_3D, NDSpace, Point}
import scalismo.mesh.boundingSpheres._
import scalismo.mesh.{MeshOperations, TetrahedralMesh, TetrahedralMesh3DOperations}
import scalismo.numerics.ValueInterpolator

trait BarycentricInterpolator[D, A] extends FieldInterpolator[D, UnstructuredPointsDomain[D], A] {
  implicit protected val valueInterpolator: ValueInterpolator[A]
}

object BarycentricInterpolator {

  trait Create[D] {
    def createBarycentricInterpolator[A: ValueInterpolator](m: TetrahedralMesh[D]): BarycentricInterpolator[D, A]
  }

  implicit object create3D extends Create[_3D] {
    override def createBarycentricInterpolator[A: ValueInterpolator](
      m: TetrahedralMesh[_3D]
    ): BarycentricInterpolator[_3D, A] = new BarycentricInterpolator3D[A](m)
  }

  def apply[D: NDSpace, A: ValueInterpolator](
    m: TetrahedralMesh[D]
  )(implicit creator: Create[D]): BarycentricInterpolator[D, A] = {
    creator.createBarycentricInterpolator(m)
  }

}

case class BarycentricInterpolator3D[A: ValueInterpolator](mesh: TetrahedralMesh[_3D])
    extends BarycentricInterpolator[_3D, A] {

  val meshOps: TetrahedralMesh3DOperations = mesh.operations

  override protected val valueInterpolator: ValueInterpolator[A] = ValueInterpolator[A]

  override def interpolate(df: DiscreteField[_3D, UnstructuredPointsDomain[_3D], A]): Field[_3D, A] = {

    def interpolateBarycentric(p: Point[_3D]): A = {

      meshOps.closestPointToVolume(p) match {
        case ClosestPointIsVertex(_, _, pId)    => df(pId)
        case ClosestPointOnLine(_, _, pIds, bc) => ValueInterpolator[A].blend(df(pIds._1), df(pIds._2), bc)
        case ClosestPointInTriangleOfTetrahedron(_, _, tetId, triId, bc) =>
          val triangle = mesh.tetrahedralization.tetrahedron(tetId).triangles(triId.id)
          bc.interpolateProperty(df(triangle.ptId1), df(triangle.ptId2), df(triangle.ptId3))
        case ClosestPointInTetrahedron(_, _, tId, bc) =>
          val tetrahedron = mesh.tetrahedralization.tetrahedron(tId)
          bc.interpolateProperty(df(tetrahedron.ptId1),
                                 df(tetrahedron.ptId2),
                                 df(tetrahedron.ptId3),
                                 df(tetrahedron.ptId4))
      }
    }
    Field(RealSpace[_3D], interpolateBarycentric)
  }
}
