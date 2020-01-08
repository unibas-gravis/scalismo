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

/**
 * express correspondence of a triangulated surface to another triangulation, maps points on this surface to target surface
 */
trait MeshSurfaceCorrespondence extends MeshSurfaceProperty[(TriangleId, BarycentricCoordinates)] {

  /**
   * get corresponding point on target surface
   * @param triangleId triangle on this surface
   * @param bcc barycentric coordinates on this surface
   * @return corresponding triangle and barycentric coordinates on target surface
   */
  def correspondingPoint(triangleId: TriangleId, bcc: BarycentricCoordinates): (TriangleId, BarycentricCoordinates)

  /**
   * triangulation of this surface
   * @return
   */
  override def triangulation: TriangleList

  /**
   * triangulation of target surface
   * @return
   */
  def targetTriangulation: TriangleList

  override def onSurface(triangleId: TriangleId, bcc: BarycentricCoordinates): (TriangleId, BarycentricCoordinates) =
    correspondingPoint(triangleId: TriangleId, bcc: BarycentricCoordinates)
}
