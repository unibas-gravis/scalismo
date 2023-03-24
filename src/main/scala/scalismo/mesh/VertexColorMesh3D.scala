/*
 * Copyright 2020 University of Basel, Graphics and Vision Research Group
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

import scalismo.color.RGBA
import scalismo.geometry.{_3D, Point}

/**
 * colored mesh with RGBA color per vertex
 * @param shape
 *   positions
 * @param color
 *   color of mesh surface, per point
 */
case class VertexColorMesh3D(shape: TriangleMesh3D, color: SurfacePointProperty[RGBA]) {
  require(shape.triangulation == color.triangulation)

  def transform(trafo: Point[_3D] => Point[_3D]): VertexColorMesh3D = {
    val s = shape.transform { trafo }
    copy(shape = s)
  }
}
