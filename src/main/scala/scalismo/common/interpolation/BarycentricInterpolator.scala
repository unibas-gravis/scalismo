package scalismo.common.interpolation

import scalismo.common._
import scalismo.geometry.{_3D, NDSpace, Point}
import scalismo.mesh.boundingSpheres._
import scalismo.mesh.{MeshOperations, TetrahedralMesh}
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

  val meshOps = MeshOperations(mesh)

  override protected val valueInterpolator: ValueInterpolator[A] = ValueInterpolator[A]

  override def interpolate(df: DiscreteField[_3D, UnstructuredPointsDomain[_3D], A]): Field[_3D, A] = {

    def interpolateBarycentric(p: Point[_3D]): A = {

      meshOps.closestPointToVolume(p) match {
        case cp: ClosestPointIsVertex =>
          df(cp.pid)
        case cp: ClosestPointOnLine =>
          ValueInterpolator[A].blend(df(cp.pids._1), df(cp.pids._2), cp.bc)
        case cp: ClosestPointWithType =>
          val cell = cp match {
            case cp: ClosestPointInTriangleOfTetrahedron =>
              mesh.tetrahedralization.tetrahedron(cp.tetId)
            case cp: ClosestPointInTetrahedron =>
              mesh.tetrahedralization.tetrahedron(cp.tid)
          }
          val vertexValues = cell.pointIds.map(df(_))
          val barycentricCoordinates = mesh.getBarycentricCoordinates(p, cell)
          val valueCoordinatePairs = vertexValues.zip(barycentricCoordinates)
          ValueInterpolator[A].convexCombination(valueCoordinatePairs(0),
                                                 valueCoordinatePairs(1),
                                                 valueCoordinatePairs(2),
                                                 valueCoordinatePairs(3))
      }
    }
    Field(RealSpace[_3D], interpolateBarycentric)
  }
}
