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

import algebra.ring.Ring
import breeze.linalg.DenseVector
import spire.algebra.LeftModule
import spire.implicits._

import scala.language.implicitConversions

sealed abstract class IntVector[D: NDSpace] {
  def apply(a: Int): Int

  def dimensionality: Int = implicitly[NDSpace[D]].dimensionality

  def toArray: Array[Int]

  @deprecated("real data is now private, use toArray", "")
  def data = toArray

  def toBreezeVector: DenseVector[Int] = DenseVector(toArray)

  def mapWithIndex(f: (Int, Int) => Int): IntVector[D]

  def map(f: Int => Int): IntVector[D] = mapWithIndex((v, i) => f(v))
}

/** 1D point */
case class IntVector1D(i: Int) extends IntVector[_1D] {
  override def apply(a: Int): Int = a match {
    case 0 => i
    case _ => throw new IndexOutOfBoundsException("Index1D has only 1 element")
  }

  override def toArray = Array(i)

  override def mapWithIndex(f: (Int, Int) => Int): IntVector1D = IntVector1D(f(i, 0))
}

/** 2D point */
case class IntVector2D(i: Int, j: Int) extends IntVector[_2D] {
  override def apply(a: Int): Int = a match {
    case 0 => i
    case 1 => j
    case _ => throw new IndexOutOfBoundsException("Index2D has only 2 elements")
  }

  override def toArray = Array(i, j)

  override def mapWithIndex(f: (Int, Int) => Int): IntVector2D = IntVector2D(f(i, 0), f(j, 1))
}

/** 3D point */
case class IntVector3D(i: Int, j: Int, k: Int) extends IntVector[_3D] {
  override def apply(a: Int): Int = a match {
    case 0 => i
    case 1 => j
    case 2 => k
    case _ => throw new IndexOutOfBoundsException("Index3D has only 3 elements")
  }

  override def toArray = Array(i, j, k)

  override def mapWithIndex(f: (Int, Int) => Int): IntVector3D = IntVector3D(f(i, 0), f(j, 1), f(k, 2))
}

object IntVector {

  /** creation typeclass */
  trait Create[D] {
    def createIndex(data: Array[Int]): IntVector[D]
  }

  trait Create1D extends Create[_1D] {
    override def createIndex(d: Array[Int]) = {
      require(d.length == 1)
      IntVector1D(d(0))
    }
  }

  trait Create2D extends Create[_2D] {
    override def createIndex(d: Array[Int]) = {
      require(d.length == 2)
      IntVector2D(d(0), d(1))
    }
  }

  trait Create3D extends Create[_3D] {
    override def createIndex(d: Array[Int]) = {
      require(d.length == 3)
      IntVector3D(d(0), d(1), d(2))
    }
  }

  def apply[D: NDSpace](d: Array[Int])(implicit builder: Create[D]) = builder.createIndex(d)
  def apply(x: Int): IntVector1D = IntVector1D(x)
  def apply(x: Int, y: Int): IntVector2D = IntVector2D(x, y)
  def apply(x: Int, y: Int, z: Int): IntVector3D = IntVector3D(x, y, z)

  def zeros[D: NDSpace] = {
    IntVector(Array.fill(NDSpace[D].dimensionality)(0))
  }

  /** spire Module implementation for Index (no scalar division) */
  implicit def spireModule[D: NDSpace]: LeftModule[IntVector[D], Int] =
    new spire.algebra.LeftModule[IntVector[D], Int] {
      override def scalar: Ring[Int] = Ring[Int]
      override def timesl(r: Int, v: IntVector[D]): IntVector[D] = v.map(i => i * r)
      override def negate(x: IntVector[D]): IntVector[D] = x.map(i => -i)
      override def zero: IntVector[D] = zeros[D]
      override def plus(x: IntVector[D], y: IntVector[D]): IntVector[D] = x.mapWithIndex((v, i) => v + y(i))
    }

  object implicits {
    implicit def index1DToInt(ind: IntVector[_1D]): Int = ind.i
    implicit def intToIndex1D(f: Int): IntVector1D = IntVector1D(f)
    implicit def tupleOfIntToIndex2D(t: (Int, Int)): IntVector2D = IntVector2D(t._1, t._2)
    implicit def tupleOfIntToIndex3D(t: (Int, Int, Int)): IntVector3D = IntVector3D(t._1.toInt, t._2.toInt, t._3.toInt)
  }

  implicit def parametricToConcrete1D(p: IntVector[_1D]): IntVector1D = p.asInstanceOf[IntVector1D]
  implicit def parametricToConcrete2D(p: IntVector[_2D]): IntVector2D = p.asInstanceOf[IntVector2D]
  implicit def parametricToConcrete3D(p: IntVector[_3D]): IntVector3D = p.asInstanceOf[IntVector3D]

}
