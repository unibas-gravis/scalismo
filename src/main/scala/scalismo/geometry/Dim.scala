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
package scalismo.geometry

/** a marker trait only meant to distinguish the dimension */
sealed trait Dim

trait _1D extends Dim
trait _2D extends Dim
trait _3D extends Dim


trait NDSpace[D]
    extends EuclideanVector.Create[D]
    with Point.Create[D]
    with IntVector.Create[D] {
  def dimensionality: Int
}

object NDSpace {
  def apply[D](implicit ndSpace: NDSpace[D]): NDSpace[D] = ndSpace
}

object Dim {
  implicit object OneDSpace extends NDSpace[_1D]
      with EuclideanVector.Create1D
      with Point.Create1D
      with IntVector.Create1D {
    override val dimensionality = 1
  }

  implicit object TwoDSpace extends NDSpace[_2D]
      with EuclideanVector.Create2D
      with Point.Create2D
      with IntVector.Create2D {
    override val dimensionality = 2
  }

  implicit object ThreeDSpace extends NDSpace[_3D]
      with EuclideanVector.Create3D
      with Point.Create3D
      with IntVector.Create3D {
    override val dimensionality = 3
  }


}
