/*
 * Copyright 2016 University of Basel, Graphics and Vision Research Group
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package scalismo.mesh

import scalismo.common.{DifferentiableField, EuclideanSpace, Field, PointId}
import scalismo.geometry.{_3D, EuclideanVector, Point}
import scalismo.mesh.MeshBoundaryPredicates.{fillTriangleOnBorderMap, TriangleSortedPointIds}
import scalismo.mesh.boundingSpheres.{
  BoundingSphereHelpers,
  BoundingSpheres,
  ClosestPoint,
  ClosestPointInTriangle,
  ClosestPointOnLine,
  ClosestPointWithType,
  LineTetrahedralMesh3DIntersectionIndex,
  LineTriangleMesh3DIntersectionIndex,
  SurfaceSpatialIndex,
  TetrahedralMesh3DSpatialIndex,
  TetrahedralizedVolumeIntersectionIndex,
  TriangleMesh3DSpatialIndex,
  TriangulatedSurfaceIntersectionIndex,
  VolumeSpatialIndex
}
import scalismo.utils.MeshConversion

import scala.collection.parallel.immutable.ParVector

object MeshOperations {
  def apply(mesh: TriangleMesh[_3D]) = new TriangleMesh3DOperations(mesh)
  def apply(mesh: TetrahedralMesh[_3D]) = new TetrahedralMesh3DOperations(mesh)
}

class TriangleMesh3DOperations(private val mesh: TriangleMesh[_3D]) {

  /**
   * Calculated data from mesh
   */
  private lazy val meshPoints = mesh.pointSet.points.toIndexedSeq

  /**
   * Bounding spheres based mesh operations
   */
  private lazy val triangles = BoundingSpheres.triangleListFromTriangleMesh3D(mesh)
  private lazy val boundingSpheres = BoundingSpheres.createForTriangles(triangles)

  private lazy val intersect: TriangulatedSurfaceIntersectionIndex[_3D] =
    new LineTriangleMesh3DIntersectionIndex(boundingSpheres, mesh, triangles)
  def hasIntersection(point: Point[_3D], direction: EuclideanVector[_3D]): Boolean =
    intersect.hasIntersection(point, direction)
  def getIntersectionPoints(point: Point[_3D], direction: EuclideanVector[_3D]): Seq[Point[_3D]] =
    intersect.getIntersectionPoints(point, direction)
  def getIntersectionPointsOnSurface(point: Point[_3D],
                                     direction: EuclideanVector[_3D]): Seq[(TriangleId, BarycentricCoordinates)] =
    intersect.getSurfaceIntersectionPoints(point, direction)

  private lazy val closestPointOnSurface: SurfaceSpatialIndex[_3D] =
    new TriangleMesh3DSpatialIndex(boundingSpheres, mesh, triangles)
  def shortestDistanceToSurfaceSquared(point: Point[_3D]): Double =
    closestPointOnSurface.getSquaredShortestDistance(point: Point[_3D])
  def closestPoint(point: Point[_3D]): ClosestPoint = closestPointOnSurface.getClosestPoint(point)
  def closestPointOnSurface(point: Point[_3D]): ClosestPointWithType =
    closestPointOnSurface.getClosestPointOnSurface(point)

  /**
   * Boundary predicates
   */
  private lazy val boundary: TriangularMeshBoundaryPredicates = MeshBoundaryPredicates(mesh)
  def pointIsOnBoundary(pid: PointId): Boolean = boundary.pointIsOnBoundary(pid)
  def edgeIsOnBoundary(pid1: PointId, pid2: PointId): Boolean = boundary.edgeIsOnBoundary(pid1, pid2)
  def triangleIsOnBoundary(tid: TriangleId): Boolean = boundary.triangleIsOnBoundary(tid: TriangleId)

  /**
   * Returns a new [[TriangleMesh]] where all points satisfying the given predicate are removed.
   * All cells containing deleted points are also deleted.
   * @todo use MeshCompactifier to express this functionality. But first verify and test that it remains the same.
   */
  def clip(clipPointPredicate: Point[_3D] => Boolean): TriangleMesh[_3D] = {
    // predicate tested at the beginning, once.
    val remainingPoints = new ParVector(meshPoints.toVector).filter { !clipPointPredicate(_) }.zipWithIndex.toMap

    val remainingPointTriplet = new ParVector(mesh.cells.toVector)
      .map { cell =>
        val points = cell.pointIds.map(pointId => meshPoints(pointId.id))
        (points, points.map(p => remainingPoints.get(p).isDefined).reduce(_ && _))
      }
      .filter(_._2)
      .map(_._1)

    val points = remainingPointTriplet.flatten.distinct
    val pt2Id = points.zipWithIndex.toMap
    val cells = remainingPointTriplet.map {
      case vec => TriangleCell(PointId(pt2Id(vec(0))), PointId(pt2Id(vec(1))), PointId(pt2Id(vec(2))))
    }

    TriangleMesh3D(points.toIndexedSeq, TriangleList(cells.toIndexedSeq))
  }

  /**
   * Returns a new continuous [[DifferentiableScalarImage]] defined on 3-dimensional [[RealSpace]] which is the distance transform of the mesh
   */
  def toDistanceImage: DifferentiableField[_3D, Float] = {
    def dist(pt: Point[_3D]): Float = Math.sqrt(shortestDistanceToSurfaceSquared(pt)).toFloat

    def grad(pt: Point[_3D]) = {
      val closestPt = closestPoint(pt).point
      val grad = EuclideanVector(pt(0) - closestPt(0), pt(1) - closestPt(1), pt(2) - closestPt(2))
      grad * (1.0 / grad.norm)
    }

    DifferentiableField(EuclideanSpace[_3D], (pt: Point[_3D]) => dist(pt), (pt: Point[_3D]) => grad(pt))
  }

  /**
   * Returns a new continuous binary [[ScalarImage]] defined on 3-dimensional [[EuclideanSpace]] , where the mesh surface is used to split the image domain.
   * Points lying on the space side pointed towards by the surface normals will have value 0. Points lying on the other side have
   * value 1. Hence if the mesh is a closed surface, points inside the surface have value 1 and points outside 0.
   *
   */
  def toBinaryImage: Field[_3D, Short] = {

    val meshOps = mesh.operations

    def inside(pt: Point[_3D]): Short = {
      val (closestPoint, normalAtClosestPoint) = meshOps.closestPointOnSurface(pt) match {
        case ClosestPointInTriangle(closestPoint, dist, triangleId, bcc) => {
          (closestPoint, mesh.vertexNormals.onSurface(triangleId, bcc))
        }
        case ClosestPointOnLine(closestPoint, _, (id1, id2), bc) => {
          val normalPt1 = mesh.vertexNormals(id1)
          val normalPt2 = mesh.vertexNormals(id2)
          val averagedNormal = (normalPt1 * bc) + (normalPt2 * (1.0 - bc))
          (closestPoint, averagedNormal / averagedNormal.norm)
        }
        case _ => {
          val closestMeshPt = mesh.pointSet.findClosestPoint(pt)
          (closestMeshPt.point, mesh.vertexNormals(closestMeshPt.id))
        }
      }
      val dotprod = normalAtClosestPoint dot (closestPoint - pt)
      if (dotprod > 0.0) 1 else 0
    }
    Field(EuclideanSpace[_3D], (pt: Point[_3D]) => inside(pt))
  }

  /**
   * mask points behind clipping plane
   *
   * @param point  point in clipping plane
   * @param normal normal vector of clipping plane
   */
  def maskWithPlane(point: Point[_3D], normal: EuclideanVector[_3D]): MeshCompactifier = {
    val n = normal.normalize
    maskPoints((pid: PointId) => (mesh.pointSet.point(pid) - point).dot(n) >= 0.0)
  }

  /**
   * Mask reduces the pointSet and triangulation of a mesh to keep only those parts
   * that evaluate to true for the passed in predicate.
   * @param pointFilter predicate that maps 3d locations to boolean ('true' = keep location).
   */
  def maskSpatially(pointFilter: (Point[_3D]) => Boolean): MeshCompactifier = {
    mask((pid: PointId) => pointFilter(mesh.pointSet.point(pid)), _ => true)
  }

  /**
   * Mask reduces the pointSet and triangulation of a mesh to keep only those parts
   * that evaluate to true for the passed in predicate.
   * @param pointFilter Predicate that maps PointId to boolean ('true' = keep location).
   */
  def maskPoints(pointFilter: (PointId) => Boolean): MeshCompactifier = {
    mask(pointFilter, _ => true)
  }

  /**
   * Mask a mesh to a subset of the triangles.
   * @param triangleFilter Predicate that maps TriangleId to boolean ('true' = keep triangle).
   */
  def maskTriangles(triangleFilter: (TriangleId) => Boolean): MeshCompactifier = {
    mask(_ => true, triangleFilter)
  }

  /**
   * Mask a mesh to a subset of points and triangles.
   * @param pointFilter Predicate that maps PointId to boolean ('true' = keep location).
   * @param triangleFilter Predicate that maps TriangleId to boolean ('true' = keep triangle).
   */
  def mask(pointFilter: (PointId) => Boolean, triangleFilter: (TriangleId) => Boolean): MeshCompactifier = {
    MeshCompactifier(mesh, pointFilter, triangleFilter)
  }

  /**
   * Reduces the triangle and points so that only used and valid locations and triangles remain.
   */
  def compact: MeshCompactifier = {
    mask(_ => true, _ => true)
  }

  /**
   * Attempts to reduce the number of vertices of a mesh to the given number of vertices.
   *
   * @param targetedNumberOfVertices The targeted number of vertices. Note that it is not guaranteed
   *                                 that this number is reached exactly
   * @return The decimated mesh
   */
  def decimate(targetedNumberOfVertices: Int): TriangleMesh[_3D] = {
    val refVtk = MeshConversion.meshToVtkPolyData(mesh)
    val decimate = new vtk.vtkQuadricDecimation()

    val reductionRate = 1.0 - (targetedNumberOfVertices / mesh.pointSet.numberOfPoints.toDouble)

    decimate.SetTargetReduction(reductionRate)

    decimate.SetInputData(refVtk)
    decimate.Update()
    val decimatedRefVTK = decimate.GetOutput()
    MeshConversion.vtkPolyDataToTriangleMesh(decimatedRefVTK).get
  }

}

class TetrahedralMesh3DOperations(private val mesh: TetrahedralMesh[_3D]) {

  /**
   * Calculated data from mesh
   */
  private lazy val meshPoints = mesh.pointSet.points.toIndexedSeq

  /**
   * Bounding spheres based mesh operations
   */
  private lazy val tetrahedrons = BoundingSpheres.tetrahedronListFromTetrahedralMesh3D(mesh)
  private lazy val boundingSpheres = BoundingSpheres.createForTetrahedrons(tetrahedrons)

  private lazy val closestPointIndex: VolumeSpatialIndex[_3D] =
    new TetrahedralMesh3DSpatialIndex(boundingSpheres, mesh, tetrahedrons)
  def shortestDistanceToVolumeSquared(point: Point[_3D]): Double =
    closestPointIndex.getSquaredShortestDistance(point: Point[_3D])
  def closestPoint(point: Point[_3D]): ClosestPoint = closestPointIndex.getClosestPoint(point)
  def closestPointToVolume(point: Point[_3D]): ClosestPointWithType =
    closestPointIndex.getClosestPointToVolume(point)

  private lazy val intersect: TetrahedralizedVolumeIntersectionIndex[_3D] =
    new LineTetrahedralMesh3DIntersectionIndex(boundingSpheres, mesh, tetrahedrons)
  def hasIntersection(point: Point[_3D], direction: EuclideanVector[_3D]): Boolean =
    intersect.hasIntersection(point, direction)
  def getIntersectionPoints(point: Point[_3D], direction: EuclideanVector[_3D]): Seq[Point[_3D]] =
    intersect.getIntersectionPoints(point, direction)
  def getIntersectionPointsOnSurface(point: Point[_3D],
                                     direction: EuclideanVector[_3D]): Seq[(TetrahedronId, BarycentricCoordinates4)] =
    intersect.getVolumeIntersectionPoints(point, direction)

  /**
   * Boundary predicates
   */
  private lazy val boundary: TetrahedralMeshBoundaryPredicates = MeshBoundaryPredicates(mesh)
  def pointIsOnBoundary(pid: PointId): Boolean = boundary.pointIsOnBoundary(pid)
  def edgeIsOnBoundary(pid1: PointId, pid2: PointId): Boolean = boundary.edgeIsOnBoundary(pid1, pid2)
  def tetrahedronIsOnBoundary(tid: TetrahedronId): Boolean = boundary.tetrahedronIsOnBoundary(tid)

  def getOuterSurface: TriangleMesh[_3D] = {
    val tetrahedrons = mesh.tetrahedralization.tetrahedrons
    val triangleOnBorder = fillTriangleOnBorderMap(tetrahedrons)
    val extractedSurface = TriangleMesh3D(
      mesh.pointSet,
      TriangleList(
        tetrahedrons
          .flatMap(_.triangles)
          .filter(tc => triangleOnBorder.contains(MeshBoundaryPredicates.TriangleSortedPointIds(tc.pointIds)))
      )
    ).operations.compact.transformedMesh

    // fix normals of triangular mesh pointing outwards:
    // We assume that all tetrahedrons are consistently orientated,
    // i.e. all have positive or all have a negative volume.
    val firstNonZeroSignedVolume = tetrahedrons.iterator
      .map(tet =>
        BoundingSphereHelpers.calculateSignedVolume(
          mesh.pointSet.point(tet.ptId1).toVector,
          mesh.pointSet.point(tet.ptId2).toVector,
          mesh.pointSet.point(tet.ptId3).toVector,
          mesh.pointSet.point(tet.ptId4).toVector
        )
      )
      .filter(d => d > 1e-8 || d < -1e-8)
      .next
    if (firstNonZeroSignedVolume > 0) {
      extractedSurface.copy(
        triangulation = TriangleList(extractedSurface.triangulation.triangles.map { tri =>
          TriangleCell(tri.ptId1, tri.ptId3, tri.ptId2)
        })
      )
    } else {
      extractedSurface
    }
  }

  /**
   * Returns a new [[TriangleMesh]] where all points satisfying the given predicate are removed.
   * All cells containing deleted points are also deleted.
   * @todo use MeshCompactifier to express this functionality. But first verify and test that it remains the same.
   */
  def clip(clipPointPredicate: Point[_3D] => Boolean): TetrahedralMesh[_3D] = {
    // predicate tested at the beginning, once.
    val remainingPoints = new ParVector(meshPoints.toVector)
      .filter { !clipPointPredicate(_) }
      .zipWithIndex
      .toMap

    val remainingPointQuatriplet = new ParVector(mesh.cells.toVector)
      .map { cell =>
        val points = cell.pointIds.map(pointId => meshPoints(pointId.id))
        (points, points.map(p => remainingPoints.get(p).isDefined).reduce(_ && _))
      }
      .filter(_._2)
      .map(_._1)

    val points = remainingPointQuatriplet.flatten.distinct
    val pt2Id = points.zipWithIndex.toMap
    val cells = remainingPointQuatriplet.map {
      case vec =>
        TetrahedralCell(PointId(pt2Id(vec(0))), PointId(pt2Id(vec(1))), PointId(pt2Id(vec(2))), PointId(pt2Id(vec(3))))
    }

    TetrahedralMesh3D(points.toIndexedSeq, TetrahedralList(cells.toIndexedSeq))
  }
}
