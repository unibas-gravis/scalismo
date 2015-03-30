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
import spire.math.Numeric
import scalismo.geometry.NDSpace
import scalismo.geometry.Point
import scalismo.common.DiscreteDomain._

/**
 * Defines a discrete set of values, where each associated to a point of the domain.
 */
trait DiscreteField[D <: Dim, A] extends PartialFunction[Int, A] { self =>

  def domain: DiscreteDomain[D]

  def values: Iterator[A]
  def pointsWithValues = domain.points zip values
  def pointsWithIds = domain.points.zipWithIndex

  def foreach(f: A => Unit): Unit = values.foreach(f)

  def interpolateNearestNeighbor(): Field[D, A]
  // TODO conceptually, we should have a map here too, but it becomes tricky to
  // do since the overloaded functions will all require their own version of map
  // Maybe a trick with CanBuildFrom and Builder, similar to the scala collectiosn would be required.
}

/**
 *
 */
class DiscreteScalarField[D <: Dim: NDSpace: CanBound, A: Numeric: ClassTag](val domain: DiscreteDomain[D], private[scalismo] val data: Array[A]) extends DiscreteField[D, A] {

  /** map the function f over the values, but ensures that the result is scalar valued as well */
  def map[B: Numeric: ClassTag](f: A => B): DiscreteScalarField[D, B] = {
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

  def interpolateNearestNeighbor(): ScalarField[D, A] = {
    val indexedDomain = SpatiallyIndexedDiscreteDomain(domain.points.toIndexedSeq, domain.numberOfPoints)
    ScalarField(domain.boundingBox, (p: Point[D]) => apply(indexedDomain.findClosestPoint(p)._2))
  }
  override lazy val hashCode: Int = data.hashCode() + domain.hashCode()

}

/**
 *
 */
class DiscreteVectorField[D <: Dim: NDSpace: CanBound, DO <: Dim] private (val domain: DiscreteDomain[D], private[scalismo] val data: IndexedSeq[Vector[DO]]) extends DiscreteField[D, Vector[DO]] {

  override def values = data.iterator
  override def apply(ptId: Int) = data(ptId)
  override def isDefinedAt(ptId: Int) = data.isDefinedAt(ptId)

  /**
   * Returns a continuous vector field, where the value at each point is that of the closest point in the discrete set
   * *
   */
  def interpolateNearestNeighbor(): VectorField[D, DO] = {
    val indexedDomain = SpatiallyIndexedDiscreteDomain(domain.points.toIndexedSeq, domain.numberOfPoints)
    VectorField(domain.boundingBox, (p: Point[D]) => apply(indexedDomain.findClosestPoint(p)._2))
  }

  /** map the function f over the values, but ensures that the result is scalar valued as well */
  def map(f: Vector[DO] => Vector[DO]): DiscreteVectorField[D, DO] = new DiscreteVectorField(domain, data.map(f))

}

object DiscreteVectorField {

  def apply[D <: Dim: NDSpace: CanBound, DO <: Dim](domain: DiscreteDomain[D], data: IndexedSeq[Vector[DO]]) = {
    new DiscreteVectorField(domain, data)
  }
}