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

import scala.language.implicitConversions
import scala.reflect.ClassTag

class Index[D <: Dim: NDSpace] private (val data: Array[Int]) extends Coordinate[D, Int] with CoordinateOps[D, Int, Index[D]] {

  override val classTagScalar: ClassTag[Int] = implicitly[ClassTag[Int]]
  override def createConcreteRepresentation(data: Array[Int]): Index[D] = new Index[D](data)

  override def canEqual(other: Any): Boolean = other.isInstanceOf[Index[D]]

}

object Index {

  def apply[D <: Dim: NDSpace](d: Array[Int]) = new Index[D](d)
  def apply(i: Int): Index[_1D] = new Index[_1D](Array(i))
  def apply(i: Int, j: Int): Index[_2D] = new Index[_2D](Array(i, j))
  def apply(i: Int, j: Int, k: Int): Index[_3D] = new Index[_3D](Array(i, j, k))

  object implicits {
    implicit def index1DToInt(v: Index[_1D]) = v(0)
    implicit def intToindex1De(i: Int): Index[_1D] = Index(i)
    implicit def tupleOfIntToindex2D(t: (Int, Int)): Index[_2D] = Index(t._1, t._2)
    implicit def tupleOfIntToindex3D(t: (Int, Int, Int)): Index[_3D] = Index(t._1, t._2, t._3)
  }
}

