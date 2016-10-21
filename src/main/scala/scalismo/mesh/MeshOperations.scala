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

import scalismo.common.PointId
import scalismo.geometry.{ Point, Vector, _3D }
import scalismo.mesh.boundingSpheres._

object MeshOperations {
  def apply(mesh: TriangleMesh3D) = new TriangleMesh3DOperations(mesh)
}

class TriangleMesh3DOperations(mesh: TriangleMesh3D) {

  /**
   * Bounding spheres based mesh operations
   */
  private lazy val triangles = BoundingSpheres.triangleListFromTriangleMesh3D(mesh)
  private lazy val boundingSpheres = BoundingSpheres.createForTriangles(triangles)

  private lazy val intersect: TriangulatedSurfaceIntersectionIndex[_3D] = new LineTriangleMesh3DIntersectionIndex(boundingSpheres, mesh, triangles)
  def hasIntersection(point: Point[_3D], direction: Vector[_3D]): Boolean = intersect.hasIntersection(point, direction)
  def getIntersectionPoints(point: Point[_3D], direction: Vector[_3D]): Seq[Point[_3D]] = intersect.getIntersectionPoints(point, direction)
  def getIntersectionPointsOnSurface(point: Point[_3D], direction: Vector[_3D]): Seq[(TriangleId, BarycentricCoordinates)] = intersect.getSurfaceIntersectionPoints(point, direction)

  private lazy val closestPointOnSurface: SurfaceSpatialIndex[_3D] = new TriangleMesh3DSpatialIndex(boundingSpheres, mesh, triangles)
  def shortestDistanceToSurface(point: Point[_3D]): Double = closestPointOnSurface.getSquaredShortestDistance(point: Point[_3D])
  def closestPointOnSurfaceWithSquaredDistance(point: Point[_3D]): (Point[_3D], Double) = closestPointOnSurface.getClosestPoint(point)
  def closestPointOnSurface(point: Point[_3D]): ClosestPoint = closestPointOnSurface.getClosestPointMeta(point)

  /**
   * Boundary predicates
   */
  private lazy val boundary: TriangularMeshBoundaryPredicates = MeshBoundaryPredicates(mesh)
  def pointIsOnBoundary(pid: PointId): Boolean = boundary.pointIsOnBoundary(pid)
  def edgeIsOnBoundary(pid1: PointId, pid2: PointId): Boolean = boundary.edgeIsOnBoundary(pid1, pid2)
  def triangleIsOnBoundary(tid: TriangleId): Boolean = boundary.triangleIsOnBoundary(tid: TriangleId)

}