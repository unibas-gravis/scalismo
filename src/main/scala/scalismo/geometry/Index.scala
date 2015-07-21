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
import spire.algebra.{ Rng, Field }

import scala.language.implicitConversions
import scala.reflect.ClassTag

sealed abstract class Index[D <: Dim: NDSpace] {
  def apply(a: Int): Int

  val dimensionality: Int = implicitly[NDSpace[D]].dimensionality

  def toArray: Array[Int]

  @deprecated("real data is now private, use toArray", "")
  def data = toArray

  def toBreezeVector: DenseVector[Int] = DenseVector(toArray)

  def mapWithIndex(f: (Int, Int) => Int): Index[D]

  def map(f: Int => Int): Index[D] = mapWithIndex((v, i) => f(v))
}

/** 1D point */
case class Index1D(i: Int) extends Index[_1D] {
  override def apply(a: Int): Int = a match {
    case 0 => i
    case _ => throw new IndexOutOfBoundsException("Index1D has only 1 element")
  }

  override def toArray = Array(i)

  override def mapWithIndex(f: (Int, Int) => Int): Index1D = Index1D(f(i, 0))
}

/** 2D point */
case class Index2D(i: Int, j: Int) extends Index[_2D] {
  override def apply(a: Int): Int = a match {
    case 0 => i
    case 1 => j
    case _ => throw new IndexOutOfBoundsException("Index2D has only 2 elements")
  }

  override def toArray = Array(i, j)

  override def mapWithIndex(f: (Int, Int) => Int): Index2D = Index2D(f(i, 0), f(j, 1))
}

/** 3D point */
case class Index3D(i: Int, j: Int, k: Int) extends Index[_3D] {
  override def apply(a: Int): Int = a match {
    case 0 => i
    case 1 => j
    case 2 => k
    case _ => throw new IndexOutOfBoundsException("Index3D has only 3 elements")
  }

  override def toArray = Array(i, j, k)

  override def mapWithIndex(f: (Int, Int) => Int): Index3D = Index3D(f(i, 0), f(j, 1), f(k, 2))
}

object Index {

  /** creation typeclass */
  trait Create[D <: Dim] {
    def createIndex(data: Array[Int]): Index[D]
  }

  trait Create1D extends Create[_1D] {
    override def createIndex(d: Array[Int]) = {
      require(d.length == 1)
      Index1D(d(0))
    }
  }

  trait Create2D extends Create[_2D] {
    override def createIndex(d: Array[Int]) = {
      require(d.length == 2)
      Index2D(d(0), d(1))
    }
  }

  trait Create3D extends Create[_3D] {
    override def createIndex(d: Array[Int]) = {
      require(d.length == 3)
      Index3D(d(0), d(1), d(2))
    }
  }

  def apply[D <: Dim: NDSpace](d: Array[Int])(implicit builder: Create[D]) = builder.createIndex(d)
  def apply(x: Int): Index1D = Index1D(x)
  def apply(x: Int, y: Int): Index2D = Index2D(x, y)
  def apply(x: Int, y: Int, z: Int): Index3D = Index3D(x, y, z)

  def zeros[D <: Dim: NDSpace](implicit builder: Create[D]) = {
    Index(Array.fill(NDSpace[D].dimensionality)(0))
  }

  /** spire Module implementation for Index (no scalar division) */
  implicit def spireModule[D <: Dim: NDSpace] = new spire.algebra.Module[Index[D], Int] {
    override implicit def scalar: Rng[Int] = Rng[Int]
    override def timesl(r: Int, v: Index[D]): Index[D] = v.map(i => i * r)
    override def negate(x: Index[D]): Index[D] = x.map(i => -i)
    override def zero: Index[D] = zeros[D]
    override def plus(x: Index[D], y: Index[D]): Index[D] = x.mapWithIndex((v, i) => v + y(i))
  }

  object implicits {
    implicit def point1DToInt(ind: Index[_1D]): Int = ind.i
    implicit def floatToIndex1D(f: Int): Index1D = Index1D(f)
    implicit def tupleOfIntToIndex2D(t: (Int, Int)): Index2D = Index2D(t._1, t._2)
    implicit def tupleOfIntToIndex3D(t: (Int, Int, Int)): Index3D = Index3D(t._1.toInt, t._2.toInt, t._3.toInt)
  }

  implicit def parametricToConcrete1D(p: Index[_1D]): Index1D = p.asInstanceOf[Index1D]
  implicit def parametricToConcrete2D(p: Index[_2D]): Index2D = p.asInstanceOf[Index2D]
  implicit def parametricToConcrete3D(p: Index[_3D]): Index3D = p.asInstanceOf[Index3D]

}
