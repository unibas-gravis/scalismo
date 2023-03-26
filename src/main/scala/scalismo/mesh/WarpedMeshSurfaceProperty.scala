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
 * use a target surface property through a surface correspondence Each lookup on this surface returns the property at
 * the corresponding point of the underlying surface
 * @param underlyingProperty
 *   surface property on target surface (underlying)
 * @param correspondence
 *   surface correspondence field mapping points in this triangulation to underlying triangulation
 * @tparam A
 *   type of property
 */
case class WarpedMeshSurfaceProperty[A](underlyingProperty: MeshSurfaceProperty[A],
                                        correspondence: MeshSurfaceCorrespondence
) extends MeshSurfaceProperty[A] {
  require(underlyingProperty.triangulation == correspondence.targetTriangulation,
          "correspondence is not compatible with underlying property"
  )

  override def onSurface(triangleId: TriangleId, bcc: BarycentricCoordinates): A = {
    val oldSurfacePoint = correspondence.onSurface(triangleId, bcc)
    underlyingProperty(oldSurfacePoint._1, oldSurfacePoint._2)
  }

  override def triangulation: TriangleList = correspondence.triangulation
}
