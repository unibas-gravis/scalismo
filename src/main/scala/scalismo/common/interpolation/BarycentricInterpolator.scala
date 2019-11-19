package scalismo.common.interpolation

import scalismo.common._
import scalismo.geometry.{ NDSpace, Point, _3D }
import scalismo.mesh.{ TetrahedralCell, TetrahedralMesh }
import scalismo.numerics.ValueInterpolator

trait BarycentricInterpolator[D, A] extends FieldInterpolator[D, UnstructuredPointsDomain[D], A] {
  implicit protected val valueInterpolator: ValueInterpolator[A]
}

object BarycentricInterpolator {

  trait Create[D] {
    def createBarycentricInterpolator[A: ValueInterpolator](m: TetrahedralMesh[D]): BarycentricInterpolator[D, A]
  }

  implicit object create3D extends Create[_3D] {
    override def createBarycentricInterpolator[A: ValueInterpolator](m: TetrahedralMesh[_3D]): BarycentricInterpolator[_3D, A] = new BarycentricInterpolator3D[A](m)
  }

  def apply[D: NDSpace, A: ValueInterpolator](m: TetrahedralMesh[D])(implicit creator: Create[D]): BarycentricInterpolator[D, A] = {
    creator.createBarycentricInterpolator(m)
  }

}

case class BarycentricInterpolator3D[A: ValueInterpolator](m: TetrahedralMesh[_3D]) extends BarycentricInterpolator[_3D, A] {

  override protected val valueInterpolator: ValueInterpolator[A] = ValueInterpolator[A]

  private def getTetrahedralMeshCell(p: Point[_3D]): TetrahedralCell = {
    val closestPoints = m.pointSet.findNClosestPoints(p, 4)
    val adjacentTetrahedra = closestPoints.flatMap(cp => m.tetrahedralization.adjacentTetrahedronsForPoint(cp.id))
    val tetraId = adjacentTetrahedra.filter(tId => m.isInsideTetrahedralCell(p, m.tetrahedralization.tetrahedrons(tId.id))).head
    m.tetrahedralization.tetrahedrons(tetraId.id)
  }

  override def interpolate(df: DiscreteField[_3D, UnstructuredPointsDomain[_3D], A]): Field[_3D, A] = {

    def interpolateBarycentric(p: Point[_3D]): A = {
      val cell = getTetrahedralMeshCell(p)
      val vertexValues = cell.pointIds.map(df(_))
      val barycentricCoordinates = m.getBarycentricCoordinates(p, cell)
      val valueCoordinatePairs = vertexValues.zip(barycentricCoordinates)
      ValueInterpolator[A].convexCombination(valueCoordinatePairs(0), valueCoordinatePairs(1), valueCoordinatePairs(2), valueCoordinatePairs(3))
    }
    Field(RealSpace[_3D], interpolateBarycentric)
  }
}
