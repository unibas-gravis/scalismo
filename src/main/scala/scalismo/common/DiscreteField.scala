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
import scalismo.common.interpolation.FieldInterpolator
import scalismo.geometry.{ Dim, NDSpace, Point, EuclideanVector }

import scala.reflect.ClassTag

/**
 * Defines a discrete set of values, where each associated to a point of the domain.
 */

class DiscreteField[D <: Dim, +DDomain <: DiscreteDomain[D], A](val domain: DDomain, val data: IndexedSeq[A]) extends PartialFunction[PointId, A] { self =>

  def values: Iterator[A] = data.iterator
  override def apply(ptId: PointId) = data(ptId.id)
  override def isDefinedAt(ptId: PointId) = data.isDefinedAt(ptId.id)

  def valuesWithIds = values zip domain.pointIds
  def pointsWithValues = domain.points zip values
  def pointsWithIds = domain.points.zipWithIndex

  def foreach(f: A => Unit): Unit = values.foreach(f)
  /**
   * Returns a continuous field, where the value at each point is that of the closest point in the discrete set
   *
   */
  @deprecated("please use the [[interpolate]] method with a [[NearestNeighborInterpolator]] instead", "0.16")
  def interpolateNearestNeighbor(): Field[D, A] = Field(RealSpace[D], (p: Point[D]) => apply(domain.findClosestPoint(p).id))

  /**
   * Interpolates the discrete field using the given interpolator.
   * @param interpolator Implements an interpolation scheme (e.g. Nearest Neighbor, B-Spline, ...)
   * @return A continuous field of the same type.
   */
  def interpolate(interpolator: FieldInterpolator[D, DDomain, A]): Field[D, A] = {
    interpolator.interpolate(this)
  }

  override def equals(other: Any): Boolean =
    other match {

      case that: DiscreteField[D, DDomain, A] =>
        (that canEqual this) &&
          domain == that.domain &&
          data == that.data

      case _ => false
    }

  def canEqual(other: Any): Boolean =
    other.isInstanceOf[DiscreteField[D, DDomain, A]]

}

object DiscreteField {
  def apply[D <: Dim, DDomain <: DiscreteDomain[D], A](domain: DDomain, data: IndexedSeq[A]): DiscreteField[D, DDomain, A] = new DiscreteField[D, DDomain, A](domain, data)


  private[scalismo] def createFromDenseVector[D <: Dim, DDomain <: DiscreteDomain[D], A](domain: DDomain, d: DenseVector[Double])(implicit vectorizer: Vectorizer[A]) = {
    val dim = vectorizer.dim
    val data = d.toArray.grouped(dim).map(e => vectorizer.unvectorize(DenseVector(e))).toIndexedSeq
    new DiscreteField[D, DDomain, A](domain, data)
  }

  private[scalismo] def vectorize[D <: Dim, DDomain <: DiscreteDomain[D], A](field: DiscreteField[D, DDomain, A])(implicit vectorizer: Vectorizer[A]): DenseVector[Double] = {
    val dim = vectorizer.dim
    val fullDim = field.valuesWithIds.length * dim
    val M = DenseVector.zeros[Double](fullDim)
    for (i <- field.valuesWithIds) {
      val m = vectorizer.vectorize(i._1)
      for (x <- 0 until dim) {
        M(i._2.id * dim + x) = m(x)
      }
    }
    M
  }

  private[scalismo] def vectorize[D <: Dim, A](values: IndexedSeq[A])(implicit vectorizer: Vectorizer[A]): DenseVector[Double] = {
    val dim = vectorizer.dim
    val valuesWithIndex = values.zipWithIndex
    val fullDim = valuesWithIndex.length * dim
    val M = DenseVector.zeros[Double](fullDim)
    for (i <- valuesWithIndex) {
      val m = vectorizer.vectorize(i._1)
      for (x <- 0 until dim) {
        M(i._2 * dim + x) = m(x)
      }
    }
    M
  }
}

/**
 *
 */

class DiscreteScalarField[D <: Dim: NDSpace, +DDomain <: DiscreteDomain[D], A: Scalar: ClassTag](domain: DDomain, data: ScalarArray[A]) extends DiscreteField[D, DDomain, A](domain, data) {
  //class DiscreteScalarField[D <: Dim: NDSpace, A: Scalar: ClassTag](val domain: DiscreteDomain[D], private[scalismo] val data: ScalarArray[A]) extends DiscreteField[D, A] {

  /** map the function f over the values, but ensures that the result is scalar valued as well */
  def map[B: Scalar: ClassTag](f: A => B): DiscreteScalarField[D, DDomain, B] = {
    new DiscreteScalarField(domain, data.map(f))
  }

  override def values = data.iterator
  override def apply(ptId: PointId) = data(ptId.id)
  override def isDefinedAt(ptId: PointId) = data.isDefinedAt(ptId.id)

  override def equals(other: Any): Boolean =
    other match {

      case that: DiscreteScalarField[D, DDomain, A] =>
        (that canEqual this) &&
          domain == that.domain &&
          data == that.data

      case _ => false
    }

  override def canEqual(other: Any): Boolean =
    other.isInstanceOf[DiscreteField[D, DDomain, A]]

  @deprecated("please use the [interpolate] method with a [NearestNeighborInterpolator] instead", "0.16")
  override def interpolateNearestNeighbor(): ScalarField[D, A] = {
    ScalarField(RealSpace[D], (p: Point[D]) => apply(domain.findClosestPoint(p).id))
  }
  override lazy val hashCode: Int = data.hashCode() + domain.hashCode()

}

object DiscreteScalarField {
  def apply[D <: Dim: NDSpace, DDomain <: DiscreteDomain[D], A: Scalar: ClassTag](domain: DDomain, data: ScalarArray[A]): DiscreteScalarField[D, DDomain, A] = {
    new DiscreteScalarField[D, DDomain, A](domain, data)
  }

  def apply[D <: Dim: NDSpace, DDomain <: DiscreteDomain[D], A: Scalar: ClassTag](domain: DDomain, data: Traversable[A]): DiscreteScalarField[D, DDomain, A] = {
    new DiscreteScalarField[D, DDomain, A](domain, ScalarArray(data.toArray))
  }
}

@deprecated("This will be removed in future versions. Please use DiscreteField class instead (e.g. DiscreteField[_3D,Vector[_3D]] instead of DiscreteVectorField[_3D,_3D])", "since 0.15")
class DiscreteVectorField[D <: Dim: NDSpace, DDomain <: DiscreteDomain[D], DO <: Dim: NDSpace](domain: DDomain, data: IndexedSeq[EuclideanVector[DO]]) extends DiscreteField[D, DDomain, EuclideanVector[DO]](domain, data) {

  override def values = data.iterator
  override def apply(ptId: PointId) = data(ptId.id)
  override def isDefinedAt(ptId: PointId) = data.isDefinedAt(ptId.id)

  override def interpolateNearestNeighbor(): VectorField[D, DO] = {
    VectorField(RealSpace[D], (p: Point[D]) => apply(domain.findClosestPoint(p).id))
  }

  /** map the function f over the values, but ensures that the result is scalar valued as well */
  def map(f: EuclideanVector[DO] => EuclideanVector[DO]): DiscreteVectorField[D, DDomain, DO] = new DiscreteVectorField(domain, data.map(f))

  def asBreezeVector: DenseVector[Double] = {
    val d = implicitly[NDSpace[DO]].dimensionality
    val v = DenseVector.zeros[Double](domain.numberOfPoints * d)
    for ((pt, i) <- domain.pointsWithId) {
      v(i.id * d until (i.id + 1) * d) := data(i.id).toBreezeVector
    }
    v
  }

}

@deprecated("This will be removed in future versions. Please use DiscreteField class instead (e.g. DiscreteField[_3D,Vector[_3D]] instead of DiscreteVectorField[_3D,_3D])", "since 0.15")
object DiscreteVectorField {
  def apply[D <: Dim: NDSpace, DDomain <: DiscreteDomain[D], DO <: Dim: NDSpace](domain: DDomain, data: IndexedSeq[EuclideanVector[DO]]): DiscreteVectorField[D, DDomain, DO] = {
    new DiscreteVectorField(domain, data)
  }

  /**
   * Create a discreteVectorField for the given domain, where the data is represented as a dense vector.
   * If n is the number o fpoints in the domain and d the dimensionality (DO),
   * the vector is ordered as (v_11, v_12, ... v_1d, ...v_n1, v_n2, v_nd)
   */
  def fromDenseVector[D <: Dim: NDSpace, DDomain <: DiscreteDomain[D], DO <: Dim: NDSpace](domain: DDomain,
    vec: DenseVector[Double]): DiscreteVectorField[D, DDomain, DO] = {
    val dim = implicitly[NDSpace[DO]].dimensionality
    val vectors =
      for (v <- vec.toArray.grouped(dim))
        yield EuclideanVector.apply[DO](v)

    DiscreteVectorField[D, DDomain, DO](domain, vectors.toIndexedSeq)
  }

}