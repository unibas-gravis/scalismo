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
package scalismo.common

import scalismo.geometry.{ Dim, Vector }
import scala.reflect.ClassTag

/**
 * Defines a discrete set of values, where each associated to a point of the domain.
 */
trait DiscreteField[D <: Dim, A] extends PartialFunction[Int, A] { self =>

  def domain: DiscreteDomain[D]

  def values: Iterator[A]
  def pointsWithValues = domain.points zip values
  def pointsWithIds = domain.points.zipWithIndex

  def foreach(f: A => Unit): Unit = values.foreach(f)

  // TODO conceptually, we should have a map here too, but it becomes tricky to
  // do since the overloaded functions will all require their own version of map
  // Maybe a trick with CanBuildFrom and Builder, similar to the scala collectiosn would be required.
}

/**
 *
 */
class DiscreteScalarField[D <: Dim, A: Scalar: ClassTag](val domain: DiscreteDomain[D], private[scalismo] val data: Array[A]) extends DiscreteField[D, A] {

  /** map the function f over the values, but ensures that the result is scalar valued as well */
  def map[B: Scalar: ClassTag](f: A => B): DiscreteScalarField[D, B] = {
    new DiscreteScalarField(domain, data.map(f))
  }

  override def values = data.iterator
  override def apply(ptId: Int) = data(ptId)
  override def isDefinedAt(ptId: Int) = data.isDefinedAt(ptId)

  override def equals(other: Any): Boolean =
    other match {

      case that: DiscreteScalarField[D, A] =>
        (that canEqual this) &&
          data.deep == that.data.deep &&
          domain == that.domain

      case _ => false
    }

  def canEqual(other: Any): Boolean =
    other.isInstanceOf[DiscreteField[D, A]]

  override lazy val hashCode: Int = data.hashCode() + domain.hashCode()

}

/**
 *
 */
class DiscreteVectorField[D <: Dim, DO <: Dim] private (val domain: DiscreteDomain[D], private[scalismo] val data: IndexedSeq[Vector[DO]]) extends DiscreteField[D, Vector[DO]] {

  override def values = data.iterator
  override def apply(ptId: Int) = data(ptId)
  override def isDefinedAt(ptId: Int) = data.isDefinedAt(ptId)

  /** map the function f over the values, but ensures that the result is scalar valued as well */
  def map(f: Vector[DO] => Vector[DO]): DiscreteVectorField[D, DO] = new DiscreteVectorField(domain, data.map(f))

}

object DiscreteVectorField {

  def apply[D <: Dim, DO <: Dim](domain: DiscreteDomain[D], data: IndexedSeq[Vector[DO]]) = {
    new DiscreteVectorField(domain, data)
  }
}