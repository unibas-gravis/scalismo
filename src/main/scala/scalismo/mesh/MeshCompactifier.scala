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
import scalismo.geometry._3D

/**
 * a general operation on mesh, can also alter surface properties
 */
trait MeshManipulation {

  /**
   * get the transformed mesh
   */
  def transformedMesh: TriangleMesh[_3D]

  /**
   * apply operation to a surface property default implementation: warps old surface property (general but inefficient)
   *
   * @param property
   *   surface property to transform
   */
  def applyToSurfaceProperty[A](property: MeshSurfaceProperty[A]): MeshSurfaceProperty[A] =
    WarpedMeshSurfaceProperty(property, meshSurfaceCorrespondence)

  /**
   * correspondence on new surface, returns old surface coordinates for each new point on surface
   */
  def meshSurfaceCorrespondence: MeshSurfaceCorrespondence
}

/**
 * compact a mesh: remove unreferenced points and triangles with invalid points, also respects external filters for
 * points and triangles
 *
 * @param mesh
 *   mesh to compact
 * @param pointFilter
 *   filter to remove points, keeps on true
 * @param triangleFilter
 *   filter to remove triangles, keeps on true
 */
class MeshCompactifier(mesh: TriangleMesh[_3D], pointFilter: PointId => Boolean, triangleFilter: TriangleId => Boolean)
    extends MeshManipulation {
  private val invalidPoint = PointId(-1)

  private val meshPoints: Int = mesh.pointSet.numberOfPoints

  private val pointValidity: Array[Boolean] = {
    mesh.pointSet.pointIds.map(pointFilter).toArray
  }

  @inline
  private def isPointValid(pointId: PointId) =
    pointId.id < meshPoints &&
      pointId != invalidPoint &&
      pointValidity(pointId.id)

  @inline
  private def isTriangleValid(triangleId: TriangleId): Boolean = {
    val t = mesh.triangulation.triangle(triangleId)
    triangleId != TriangleId.invalid &&
    triangleFilter(triangleId) &&
    isPointValid(t.ptId1) &&
    isPointValid(t.ptId2) &&
    isPointValid(t.ptId3)
  }

  private val newTriangles: IndexedSeq[TriangleId] = {
    mesh.triangulation.triangleIds.filter(isTriangleValid)
  }

  // find valid points: points referenced by valid triangles
  private val newPoints: IndexedSeq[PointId] = {
    newTriangles.iterator.map { mesh.triangulation.triangle }.flatMap { _.pointIds }.toIndexedSeq.distinct.sortBy {
      _.id
    }
  }

  private val numberOfPoints = newPoints.size
  assert(numberOfPoints <= mesh.pointSet.numberOfPoints)

  private val fwdIndex = Array.fill(mesh.pointSet.numberOfPoints)(invalidPoint)
  for (newId <- 0 until numberOfPoints) {
    val oldId = newPoints(newId)
    fwdIndex(oldId.id) = PointId(newId)
  }

  /** find new id for old point id */
  def pointFwdMap(oldId: PointId) = fwdIndex(oldId.id)

  /** find old id for new point id */
  def pointBackMap(newId: PointId) = newPoints(newId.id)

  /** find old id for new triangle id */
  def triangleBackMap(newId: TriangleId): TriangleId = newTriangles(newId.id)

  override val transformedMesh: TriangleMesh[_3D] = {
    val points = newPoints.map {
      mesh.pointSet.point
    }
    val triangles = newTriangles.map { tid =>
      val t = mesh.triangulation.triangle(tid)
      TriangleCell(pointFwdMap(t.ptId1), pointFwdMap(t.ptId2), pointFwdMap(t.ptId3))
    }
    TriangleMesh3D(points, TriangleList(triangles))
  }

  override def applyToSurfaceProperty[A](property: MeshSurfaceProperty[A]): MeshSurfaceProperty[A] = {
    require(property.triangulation == mesh.triangulation, "surface property is not compatible with mesh")
    property match {
      case trProp: TriangleProperty[A] =>
        val newTriangleData = transformedMesh.triangulation.triangleIds.map { tId =>
          trProp.onTriangle(triangleBackMap(tId))
        }
        TriangleProperty(transformedMesh.triangulation, newTriangleData)
      case ptProp: SurfacePointProperty[A] =>
        val newPointData = transformedMesh.pointSet.pointIds.map { pId =>
          ptProp.atPoint(pointBackMap(pId))
        }.toIndexedSeq
        SurfacePointProperty(transformedMesh.triangulation, newPointData)(ptProp.interpolator)
      case _ => super.applyToSurfaceProperty(property) // inefficient default warping
    }
  }

  /**
   * new surface correspondence: maps new triangle id to old
   */
  override def meshSurfaceCorrespondence: MeshSurfaceCorrespondence = new MeshSurfaceCorrespondence {
    override def triangulation: TriangleList = transformedMesh.triangulation

    override def targetTriangulation: TriangleList = mesh.triangulation

    override def correspondingPoint(triangleId: TriangleId,
                                    bcc: BarycentricCoordinates
    ): (TriangleId, BarycentricCoordinates) =
      (triangleBackMap(triangleId), bcc)
  }
}

object MeshCompactifier {
  def apply(mesh: TriangleMesh[_3D],
            pointFilter: PointId => Boolean,
            triangleFilter: TriangleId => Boolean
  ): MeshCompactifier =
    new MeshCompactifier(mesh, pointFilter, triangleFilter)
}
