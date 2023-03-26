package scalismo.common.interpolation

import scalismo.common._
import scalismo.geometry.{_3D, NDSpace, Point}
import scalismo.mesh.boundingSpheres._
import scalismo.mesh.{TetrahedralMesh, TetrahedralMesh3DOperations}
import scalismo.numerics.ValueInterpolator

trait BarycentricInterpolator[D, A] extends FieldInterpolator[D, TetrahedralMesh, A] {
  implicit protected val valueInterpolator: ValueInterpolator[A]
}

object BarycentricInterpolator {

  trait Create[D] {
    def createBarycentricInterpolator[A: ValueInterpolator](): BarycentricInterpolator[D, A]
  }

  implicit object create3D extends Create[_3D] {
    override def createBarycentricInterpolator[A: ValueInterpolator](): BarycentricInterpolator[_3D, A] =
      new BarycentricInterpolator3D[A]()
  }

  def apply[D: NDSpace, A: ValueInterpolator]()(implicit creator: Create[D]): BarycentricInterpolator[D, A] = {
    creator.createBarycentricInterpolator()
  }

}

case class BarycentricInterpolator3D[A: ValueInterpolator]() extends BarycentricInterpolator[_3D, A] {

  override protected val valueInterpolator: ValueInterpolator[A] = ValueInterpolator[A]

  override def interpolate(df: DiscreteField[_3D, TetrahedralMesh, A]): Field[_3D, A] = {

    val mesh = df.domain
    val meshOps: TetrahedralMesh3DOperations = mesh.operations

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
                                 df(tetrahedron.ptId4)
          )
        case _ => {
          throw new IllegalStateException(
            "invalid closest point type encountered while interpolating. This should never happend"
          )
        }
      }
    }
    Field(EuclideanSpace3D, interpolateBarycentric)
  }
}
