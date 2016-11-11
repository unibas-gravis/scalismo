/*
 * Copyright 2015 University of Basel, Graphics and Vision Research Group
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

case class LineCoordinates(a: Double) {

  /** perform bcc interpolation: interpolate vertex values within line, needs Interpolation[T] */
  def interpolateProperty[A](v1: A, v2: A)(implicit blender: Interpolator[A]): A = {
    blender.blend(v1, v2, a)
  }

}

object LineCoordinates {

  /** coordinates of first vertex */
  val v0 = new LineCoordinates(0.0)

  /** coordinates of second vertex */
  val v1 = new LineCoordinates(1.0)

  def canonical(vertexIndex: Int): LineCoordinates = vertexIndex match {
    case 0 => v0
    case 1 => v1
    case _ => throw new IndexOutOfBoundsException("LineCoordinates can only handle 2 vertices 0-1")
  }
}