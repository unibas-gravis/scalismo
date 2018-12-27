/*
 * Copyright 2016 University of Basel, Graphics and Vision Research Group
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

package scalismo.common

import scalismo.geometry.{ SpatialVector, _2D, _3D }

import scala.annotation.switch

/** Vectorizer linearize an object to and from an Array */
trait ComponentRepresentation[A] {

  /** Length of array linked to type T */
  val size: Int

  /** access specific components directly, with slow default */
  def component(color: A, index: Int): Double

  /** Create an instance from an array */
  def fromArray(arr: Array[Double]): A

  def fromComponents(comp: Int => Double): A

  /** Generate a new array for instance */
  def toArray(color: A): Array[Double] = intoArray(color, new Array[Double](size))

  /** vectorize into an existing array, returns array */
  def intoArray(color: A, array: Array[Double]): Array[Double] = {
    require(array.length >= size, "target Array is too small")
    var i = 0
    while (i < size) {
      array(i) = component(color, i)
      i += 1
    }
    array
  }
}

object ComponentRepresentation {

  def apply[A](implicit vec: ComponentRepresentation[A]): ComponentRepresentation[A] = vec

  implicit object VectorComponents2D extends ComponentRepresentation[SpatialVector[_2D]] {
    override def fromArray(arr: Array[Double]): SpatialVector[_2D] = SpatialVector(arr(0), arr(1))
    override def toArray(value: SpatialVector[_2D]): Array[Double] = Array(value.x, value.y)
    override val size: Int = 2
    override def intoArray(vec: SpatialVector[_2D], array: Array[Double]): Array[Double] = {
      require(array.length >= size)
      array(0) = vec.x
      array(1) = vec.y
      array
    }
    override def component(color: SpatialVector[_2D], index: Int): Double = (index: @switch) match {
      case 0 => color.x
      case 1 => color.y
      case _ => throw new Exception(s"index ($index) out of bounds, Vector[_2D] can only handle 0 and 1")
    }

    override def fromComponents(comp: (Int) => Double): SpatialVector[_2D] = SpatialVector(comp(0), comp(1))
  }

  implicit object VectorComponents3D extends ComponentRepresentation[SpatialVector[_3D]] {
    override def fromArray(arr: Array[Double]): SpatialVector[_3D] = SpatialVector(arr(0), arr(1), arr(2))
    override def toArray(value: SpatialVector[_3D]): Array[Double] = Array(value.x, value.y, value.z)
    override val size: Int = 3
    override def intoArray(vec: SpatialVector[_3D], array: Array[Double]): Array[Double] = {
      require(array.length >= size)
      array(0) = vec.x
      array(1) = vec.y
      array(2) = vec.z
      array
    }
    override def component(color: SpatialVector[_3D], index: Int): Double = (index: @switch) match {
      case 0 => color.x
      case 1 => color.y
      case 2 => color.z
      case _ => throw new Exception(s"index ($index) out of bounds, Vector[_3D] can only handle 0, 1 and 2")
    }
    override def fromComponents(comp: (Int) => Double): SpatialVector[_3D] = SpatialVector(comp(0), comp(1), comp(2))
  }
}