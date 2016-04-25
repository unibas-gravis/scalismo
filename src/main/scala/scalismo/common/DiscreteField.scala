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

import breeze.linalg.DenseVector
import scalismo.geometry.{ Dim, NDSpace, Point, Vector }

import scala.reflect.ClassTag

/**
 * Defines a discrete set of values, where each associated to a point of the domain.
 */
trait DiscreteField[D <: Dim, A] extends PartialFunction[PointId, A] { self =>

  def domain: DiscreteDomain[D]

  def values: Iterator[A]
  def valuesWithIds = values zip domain.pointIds
  def pointsWithValues = domain.points zip values
  def pointsWithIds = domain.points.zipWithIndex

  def foreach(f: A => Unit): Unit = values.foreach(f)
  /**
   * Returns a continuous field, where the value at each point is that of the closest point in the discrete set
   * *
   */
  def interpolateNearestNeighbor(): Field[D, A]
  // TODO conceptually, we should have a map here too, but it becomes tricky to
  // do since the overloaded functions will all require their own version of map
  // Maybe a trick with CanBuildFrom and Builder, similar to the scala collections would be required.
}

/**
 *
 */

class DiscreteScalarField[D <: Dim: NDSpace, A: Scalar: ClassTag](val domain: DiscreteDomain[D], private[scalismo] val data: ScalarArray[A]) extends DiscreteField[D, A] {

  /** map the function f over the values, but ensures that the result is scalar valued as well */
  def map[B: Scalar: ClassTag](f: A => B): DiscreteScalarField[D, B] = {
    new DiscreteScalarField(domain, data.map(f))
  }

  override def values = data.iterator
  override def apply(ptId: PointId) = data(ptId.id)
  override def isDefinedAt(ptId: PointId) = data.isDefinedAt(ptId.id)

  override def equals(other: Any): Boolean =
    other match {

      case that: DiscreteScalarField[D, A] =>
        (that canEqual this) &&
          domain == that.domain &&
          data == that.data

      case _ => false
    }

  def canEqual(other: Any): Boolean =
    other.isInstanceOf[DiscreteField[D, A]]

  def interpolateNearestNeighbor(): ScalarField[D, A] = {
    ScalarField(RealSpace[D], (p: Point[D]) => apply(domain.findClosestPoint(p).id))
  }
  override lazy val hashCode: Int = data.hashCode() + domain.hashCode()

}

object DiscreteScalarField {
  def apply[D <: Dim: NDSpace, A: Scalar: ClassTag](domain: DiscreteDomain[D], data: ScalarArray[A]): DiscreteScalarField[D, A] = {
    new DiscreteScalarField[D, A](domain, data)
  }
}

/**
 *
 */
class DiscreteVectorField[D <: Dim: NDSpace, DO <: Dim: NDSpace](val domain: DiscreteDomain[D], private[scalismo] val data: IndexedSeq[Vector[DO]]) extends DiscreteField[D, Vector[DO]] {

  override def values = data.iterator
  override def apply(ptId: PointId) = data(ptId.id)
  override def isDefinedAt(ptId: PointId) = data.isDefinedAt(ptId.id)

  def interpolateNearestNeighbor(): VectorField[D, DO] = {
    VectorField(RealSpace[D], (p: Point[D]) => apply(domain.findClosestPoint(p).id))
  }

  /** map the function f over the values, but ensures that the result is scalar valued as well */
  def map(f: Vector[DO] => Vector[DO]): DiscreteVectorField[D, DO] = new DiscreteVectorField(domain, data.map(f))

  def asBreezeVector: DenseVector[Double] = {
    val d = implicitly[NDSpace[DO]].dimensionality
    val v = DenseVector.zeros[Double](domain.numberOfPoints * d)
    for ((pt, i) <- domain.pointsWithId) {
      v(i.id * d until (i.id + 1) * d) := data(i.id).toBreezeVector
    }
    v
  }

}

object DiscreteVectorField {
  def apply[D <: Dim: NDSpace, DO <: Dim: NDSpace](domain: DiscreteDomain[D], data: IndexedSeq[Vector[DO]]) = {
    new DiscreteVectorField(domain, data)
  }

  /**
   * Create a discreteVectorField for the given domain, where the data is represented as a dense vector.
   * If n is the number o fpoints in the domain and d the dimensionality (DO),
   * the vector is ordered as (v_11, v_12, ... v_1d, ...v_n1, v_n2, v_nd)
   */
  def fromDenseVector[D <: Dim: NDSpace, DO <: Dim: NDSpace](domain: DiscreteDomain[D],
    vec: DenseVector[Double]): DiscreteVectorField[D, DO] = {
    val dim = implicitly[NDSpace[DO]].dimensionality
    val vectors =
      for (v <- vec.toArray.grouped(dim))
        yield Vector.apply[DO](v)

    DiscreteVectorField[D, DO](domain, vectors.toIndexedSeq)
  }

}