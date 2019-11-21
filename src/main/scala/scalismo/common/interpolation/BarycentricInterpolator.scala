package scalismo.common.interpolation

import scalismo.common._
import scalismo.geometry.{ NDSpace, Point, Point3D, _3D }
import scalismo.mesh.{ TetrahedralCell, TetrahedralMesh, TetrahedronId }
import scalismo.numerics.ValueInterpolator
import scalismo.utils.Memoize

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

  // Temporary solution, replace for Milestone M2!
  private def getTetrahedralMeshCell(p: Point[_3D]): Option[TetrahedralCell] = {

    def isInsideCell(tc: TetrahedralCell): Boolean = m.isInsideTetrahedralCell(p, tc)
    val isInsideCellMemoized = Memoize(isInsideCell, 1000)

    var found = false
    var cell: Option[TetrahedralCell] = None
    var neighbourhood = Set[TetrahedronId]()

    while (!found) {
      if (neighbourhood.isEmpty) {
        // start from closest vertex point
        val closestPoint = m.pointSet.findClosestPoint(p).id
        neighbourhood = m.tetrahedralization.adjacentTetrahedronsForPoint(closestPoint).toSet
      } else {
        // increase neighbourhood
        neighbourhood = neighbourhood.union(neighbourhood.flatMap(m.tetrahedralization.adjacentTetrahedronsForTetrahedron))
      }
      val filterResult = neighbourhood.filter(tId => isInsideCellMemoized(m.tetrahedralization.tetrahedrons(tId.id)))
      if (filterResult.nonEmpty) {
        cell = Option(m.tetrahedralization.tetrahedrons(filterResult.head.id))
        found = true
      }
    }
    cell
  }

  override def interpolate(df: DiscreteField[_3D, UnstructuredPointsDomain[_3D], A]): Field[_3D, A] = {

    def interpolateBarycentric(p: Point[_3D]): A = {
      val cell = getTetrahedralMeshCell(p).get
      val vertexValues = cell.pointIds.map(df(_))
      val barycentricCoordinates = m.getBarycentricCoordinates(p, cell)
      val valueCoordinatePairs = vertexValues.zip(barycentricCoordinates)
      ValueInterpolator[A].convexCombination(valueCoordinatePairs(0), valueCoordinatePairs(1), valueCoordinatePairs(2), valueCoordinatePairs(3))
    }
    Field(RealSpace[_3D], interpolateBarycentric)
  }
}
