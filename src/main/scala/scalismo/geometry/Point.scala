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

/**
 * An n-dimensional Point
 */
class Point[D <: Dim: NDSpace] private (private[scalismo] override val data: Array[Float]) extends Coordinate[D, Float] with CoordinateOps[D, Float, Point[D]] {

  override val classTagScalar: ClassTag[Float] = implicitly[ClassTag[Float]]
  override def createConcreteRepresentation(data: Array[Float]): Point[D] = new Point(data)

  def +(that: Vector[D]): Point[D] = {
    val newData = new Array[Float](dimensionality)
    var i = 0
    while (i < dimensionality) {
      newData(i) = this.data(i) + that.data(i)
      i += 1
    }
    Point[D](newData)
  }

  def -(that: Vector[D]): Point[D] = {
    val newData = new Array[Float](dimensionality)
    var i = 0
    while (i < dimensionality) {
      newData(i) = this.data(i) - that.data(i)
      i += 1
    }
    Point[D](newData)
  }

  def -(that: Point[D]): Vector[D] = {
    val newData = new Array[Float](dimensionality)
    var i = 0
    while (i < dimensionality) {
      newData(i) = this.data(i) - that.data(i)
      i += 1
    }
    Vector[D](newData)
  }

  def toVector: Vector[D] = Vector[D](data)

  protected override def canEqual(other: Any): Boolean = other.isInstanceOf[Point[D]]
}

object Point {

  def apply[D <: Dim: NDSpace](d: Array[Float]) = new Point[D](d)
  def apply(x: Float): Point[_1D] = new Point[_1D](Array(x))
  def apply(x: Float, y: Float): Point[_2D] = new Point[_2D](Array(x, y))
  def apply(x: Float, y: Float, z: Float): Point[_3D] = new Point[_3D](Array(x, y, z))

  object implicits {
    implicit def point1DToFloat(p: Point[_1D]) = p(0)
    implicit def floatToPoint1D(f: Float): Point[_1D] = Point(f)
    implicit def tupleOfFloatToPoint2D(t: (Float, Float)): Point[_2D] = Point(t._1, t._2)
    implicit def tupleOfFloatToPoint3D(t: (Float, Float, Float)): Point[_3D] = Point(t._1.toFloat, t._2.toFloat, t._3.toFloat)
  }
}
