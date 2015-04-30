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

import scala.language.implicitConversions
import scala.reflect.ClassTag

/**
 * An n-dimensional Vector
 */
class Vector[D <: Dim: NDSpace] private (private[scalismo] override val data: Array[Float]) extends Coordinate[D, Float] with CoordinateOps[D, Float, Vector[D]] {

  override val classTagScalar = implicitly[ClassTag[Float]]
  override def createRepr(data: Array[Float]) = new Vector(data)

  def norm: Double = math.sqrt(norm2)

  def norm2: Double = {
    var norm2 = 0.0
    var i = 0
    while (i < dimensionality) {
      norm2 += data(i) * data(i)
      i += 1
    }
    norm2
  }

  def +(that: Vector[D]): Vector[D] = {
    val newData = new Array[Float](dimensionality)
    var i = 0
    while (i < dimensionality) {
      newData(i) = this.data(i) + that.data(i)
      i += 1
    }
    Vector[D](newData)
  }

  def -(that: Vector[D]): Vector[D] = {
    val newData = new Array[Float](dimensionality)
    var i = 0
    while (i < dimensionality) {
      newData(i) = this.data(i) - that.data(i)
      i += 1
    }
    Vector[D](newData)
  }

  def *(s: Double): Vector[D] = {
    val newData = new Array[Float](dimensionality)
    var i = 0
    val sFloat = s.toFloat
    while (i < dimensionality) {
      newData(i) = this.data(i) * sFloat
      i += 1
    }
    Vector[D](newData)
  }

  def toPoint: Point[D] = Point[D](data)

  def dot(that: Vector[D]): Float = {
    val d = dimensionality

    var dotprod = 0f
    var i = 0
    while (i < d) {
      dotprod += this(i) * that(i)
      i += 1
    }
    dotprod
  }

  def outer(that: Vector[D]): SquareMatrix[D] = {

    require(that.dimensionality == dimensionality)
    val d = dimensionality

    val data = new Array[Float](d * d)
    var i = 0
    var j = 0
    while (i < d) {
      j = 0
      while (j < d) {
        data(j * d + i) = this(i) * that(j)
        j += 1
      }
      i += 1
    }
    SquareMatrix[D](data)
  }

  protected override def canEqual(other: Any): Boolean = other.isInstanceOf[Vector[D]]
}

object Vector {

  def apply[D <: Dim: NDSpace](d: Array[Float]) = new Vector[D](d)
  def apply(x: Float): Vector[_1D] = new Vector[_1D](Array(x))
  def apply(x: Float, y: Float): Vector[_2D] = new Vector[_2D](Array(x, y))
  def apply(x: Float, y: Float, z: Float): Vector[_3D] = new Vector[_3D](Array(x, y, z))

  def zeros[D <: Dim: NDSpace] = {
    val dim = implicitly[NDSpace[D]].dimensionality
    new Vector[D](Array.fill[Float](dim)(0f))
  }

  def crossproduct(u: Vector[_3D], v: Vector[_3D]): Vector[_3D] = {
    Vector(u(1) * v(2) - u(2) * v(1), u(2) * v(0) - u(0) * v(2), u(0) * v(1) - u(1) * v(0))
  }

  def fromBreezeVector[D <: Dim: NDSpace](breeze: DenseVector[Float]): Vector[D] = {
    val dim = implicitly[NDSpace[D]].dimensionality
    require(breeze.size == dim, s"Invalid size of breeze vector (${breeze.size} != $dim)")
    Vector.apply[D](breeze.data)
  }

  object implicits {
    implicit def vector1DToFloat(v: Vector[_1D]): Float = v(0)
    implicit def floatToVector1D(f: Float): Vector[_1D] = Vector(f)
    implicit def tupleOfFloatToVector2D(t: (Float, Float)): Vector[_2D] = Vector(t._1, t._2)
    implicit def tupleOfFloatToVector3D(t: (Float, Float, Float)): Vector[_3D] = Vector(t._1, t._2, t._3)
  }
}

