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

import breeze.linalg.DenseVector

import scala.reflect.ClassTag

/**
 * The basic n-tuple in R^n^ with scalar type S
 */
abstract class Coordinate[D: NDSpace, @specialized(Int, Float, Double) S] {
  val dimensionality: Int = implicitly[NDSpace[D]].dimensionality

  private[scalismo] val data: Array[S]

  def apply(i: Int): S = data(i)

  def toBreezeVector = DenseVector(data)

  override def hashCode = data.toSeq.hashCode()

  override def equals(other: Any): Boolean = other match {
    case that: Coordinate[D @unchecked, S @unchecked] => that.canEqual(this) && this.data.sameElements(that.data)
    case _                                            => false
  }

  protected def canEqual(other: Any): Boolean

  override def toString = data.toSeq.toString()
}

/**
 * Implementation trait for methods that are common to all representation of coordinates
 * @tparam D Dimension
 * @tparam Scalar The scalar type of the individual coordinates
 * @tparam Repr The concrete representation of a Coordinate (e.g. vector, point)
 */
private[scalismo] trait CoordinateOps[D, Scalar, Repr <: Coordinate[D, Scalar]] { self: Coordinate[D, Scalar] =>

  implicit val classTagScalar: ClassTag[Scalar]

  protected def createConcreteRepresentation(data: Array[Scalar]): Repr

  def mapWithIndex(f: (Scalar, Int) => Scalar): Repr = {
    val newData = new Array[Scalar](self.dimensionality)
    var i = 0
    while (i < self.dimensionality) {
      newData(i) = f(self.data(i), i)
      i += 1
    }
    createConcreteRepresentation(newData)
  }

  def map(f: Scalar => Scalar): Repr = {
    mapWithIndex({ case (v, _) => f(v) })
  }

}
