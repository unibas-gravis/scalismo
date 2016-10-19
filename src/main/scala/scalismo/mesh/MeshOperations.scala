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

import scalismo.mesh.boundingSpheres.{ BoundingSpheres, LineTriangleMesh3DIntersectionIndex, TriangleMesh3DSpatialIndex }

object MeshOperations {
  def apply(mesh: TriangleMesh3D) = new TriangleMesh3DOperations(mesh)
}

class TriangleMesh3DOperations(mesh: TriangleMesh3D) {
  lazy val triangles = BoundingSpheres.triangleListFromTriangleMesh3D(mesh)
  lazy val boundingSpheres = BoundingSpheres.createForTriangles(triangles)
  lazy val intersect = new LineTriangleMesh3DIntersectionIndex(boundingSpheres, mesh, triangles)
  lazy val closestPointOnSurface = new TriangleMesh3DSpatialIndex(boundingSpheres, mesh, triangles)
}