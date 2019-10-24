package scalismo.tetramesh

import scalismo.common.{PointId, RealSpace}
import scalismo.geometry.{EuclideanVector, Point, _3D}
import scalismo.image.{DifferentiableScalarImage, ScalarImage}
import scalismo.mesh._
import scalismo.mesh.boundingSpheres._



object TetrahedralMeshOperations {

  def apply(mesh: TetrahedralMesh3D) = new TetrahedralMesh3DOperations(mesh)
}


class TetrahedralMesh3DOperations(private val mesh: TetrahedralMesh3D) {

  /**
    * Calculated data from mesh
    */
  private lazy val meshPoints = mesh.pointSet.points.toIndexedSeq

  /**
    * Bounding spheres based mesh operations
    */
  private lazy val tetrahedrons = BoundingSpheres.tetrahedronListFromTetrahedralMesh3D(mesh)
  private lazy val boundingSpheres = BoundingSpheres.createForTetrahedrons(tetrahedrons)

  private lazy val intersect: TetrahedralizedVolumeIntersectionIndex[_3D] = new LineTetrahedralMesh3DIntersectionIndex(boundingSpheres, mesh, tetrahedrons)
  def hasIntersection(point: Point[_3D], direction: EuclideanVector[_3D]): Boolean = intersect.hasIntersection(point, direction)
  def getIntersectionPoints(point: Point[_3D], direction: EuclideanVector[_3D]): Seq[Point[_3D]] = intersect.getIntersectionPoints(point, direction)
  def getIntersectionPointsOnSurface(point: Point[_3D], direction: EuclideanVector[_3D]): Seq[(TetrahedronId, BarycentricCoordinates)] = intersect.getVolumeIntersectionPoints(point, direction)

  private lazy val closestPointOnSurface: SurfaceSpatialIndex[_3D] = new TetrahedralMesh3DSpatialIndex(boundingSpheres, mesh, tetrahedrons)
  def shortestDistanceToSurfaceSquared(point: Point[_3D]): Double = closestPointOnSurface.getSquaredShortestDistance(point: Point[_3D])
  def closestPoint(point: Point[_3D]): ClosestPoint = closestPointOnSurface.getClosestPoint(point)
  def closestPointOnSurface(point: Point[_3D]): ClosestPointOnSurface = closestPointOnSurface.getClosestPointOnSurface(point)

  /**
    * Boundary predicates
    */
  private lazy val boundary: TetrahedralMeshBoundaryPredicates = MeshVolumeBoundaryPredicates(mesh)
  def pointIsOnBoundary(pid: PointId): Boolean = boundary.pointIsOnBoundary(pid)
  def edgeIsOnBoundary(pid1: PointId, pid2: PointId): Boolean = boundary.edgeIsOnBoundary(pid1, pid2)
  def triangleIsOnBoundary(tid: TetrahedronId): Boolean = boundary.tetrahedronIsOnBoundary(tid: TetrahedronId)

  /**
    * Returns a new [[TriangleMesh]] where all points satisfying the given predicate are removed.
    * All cells containing deleted points are also deleted.
    * @todo use MeshCompactifier to express this functionality. But first verify and test that it remains the same.
    */
  def clip(clipPointPredicate: Point[_3D] => Boolean): TetrahedralMesh[_3D] = {
    // predicate tested at the beginning, once.
    val remainingPoints = meshPoints.par.filter { !clipPointPredicate(_) }.zipWithIndex.toMap

    val remainingPointQuatriplet = mesh.cells.par.map {
      cell =>
        val points = cell.pointIds.map(pointId => meshPoints(pointId.id))
        (points, points.map(p => remainingPoints.get(p).isDefined).reduce(_ && _))
    }.filter(_._2).map(_._1)

    val points = remainingPointQuatriplet.flatten.distinct
    val pt2Id = points.zipWithIndex.toMap
    val cells = remainingPointQuatriplet.map { case vec => TetrahedralCell(PointId(pt2Id(vec(0))), PointId(pt2Id(vec(1))), PointId(pt2Id(vec(2))),PointId(pt2Id(vec(3))))}

    TetrahedralMesh3D(points.toIndexedSeq, TetrahedralList(cells.toIndexedSeq))
  }

  /**
    * Returns a new continuous [[DifferentiableScalarImage]] defined on 3-dimensional [[RealSpace]] which is the distance transform of the mesh
    */
  def toDistanceImage: DifferentiableScalarImage[_3D] = {
    def dist(pt: Point[_3D]): Float = Math.sqrt(shortestDistanceToSurfaceSquared(pt)).toFloat

    def grad(pt: Point[_3D]) = {
      val closestPt = closestPoint(pt).point
      val grad = EuclideanVector(pt(0) - closestPt(0), pt(1) - closestPt(1), pt(2) - closestPt(2))
      grad * (1.0 / grad.norm)
    }

    DifferentiableScalarImage(RealSpace[_3D], (pt: Point[_3D]) => dist(pt), (pt: Point[_3D]) => grad(pt))
  }

}
