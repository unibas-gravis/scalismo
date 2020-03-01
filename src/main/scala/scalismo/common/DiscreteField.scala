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
import scalismo.common.DiscreteField.ScalarMeshField
import scalismo.common.interpolation.{DifferentiableFieldInterpolator, FieldInterpolator}
import scalismo.geometry.{_3D, EuclideanVector, IntVector, NDSpace, Point}
import scalismo.image.DiscreteImageDomain
import scalismo.image.DiscreteScalarImage.DiscreteScalarImage
import scalismo.mesh.{TetrahedralMesh, TriangleMesh}
import scalismo.registration.Transformation

import scala.language.higherKinds
import scala.reflect.ClassTag

/**
 * Defines a discrete set of values, where each associated to a point of the domain.
 */
class DiscreteField[D, DDomain[D] <: DiscreteDomain[D], A](val domain: DDomain[D], val data: IndexedSeq[A])
    extends PartialFunction[PointId, A] { self =>

  private val pointSet = domain.pointSet

  def values: Iterator[A] = data.iterator

  override def apply(ptId: PointId) = {
    data(ptId.id)
  }
  override def isDefinedAt(ptId: PointId) = data.isDefinedAt(ptId.id)

  def valuesWithIds = values zip pointSet.pointIds
  def pointsWithValues = pointSet.points zip values
  def pointsWithIds = pointSet.points.zipWithIndex

  def foreach(f: A => Unit): Unit = values.foreach(f)

  def map[B](f: A => B): DiscreteField[D, DDomain, B] = {
    new DiscreteField(this.domain, data.map(f))
  }

  def transform(
    transformation: Transformation[D]
  )(implicit canWarp: DomainWarp[D, DDomain],
    canWarpField: DiscreteFieldWarp[D, DDomain, A]): DiscreteField[D, DDomain, A] = {
    canWarpField.transform(this, transformation)
  }

  /**
   * Interpolates the discrete field using the given interpolator.
   * @param interpolator Implements an interpolation scheme (e.g. Nearest Neighbor, B-Spline, ...)
   * @return A continuous field of the same type.
   */
  def interpolate(interpolator: FieldInterpolator[D, DDomain, A]): Field[D, A] = {
    interpolator.interpolate(this)
  }

  /**
   * Interpolates the discrete field using the given interpolator.
   * @param interpolator Implements an interpolation scheme (e.g. Nearest Neighbor, B-Spline, ...)
   * @return A continuous field of the same type.
   */
  def interpolateDifferentiable(
    interpolator: DifferentiableFieldInterpolator[D, DDomain, A, EuclideanVector[D]]
  )(implicit scalar: Scalar[A]): DifferentiableField[D, A] = {
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

  type ScalarVolumeMeshField[Value] = DiscreteField[_3D, TetrahedralMesh, Value]
  implicit class ScalarVolumeMeshFieldOps[Value](df: DiscreteField[_3D, TetrahedralMesh, Value]) {
    def mesh: TetrahedralMesh[_3D] = df.domain
  }

  type ScalarMeshField[Value] = DiscreteField[_3D, TriangleMesh, Value]
  implicit class ScalarMeshFieldOps[Value: Scalar: ClassTag](df: DiscreteField[_3D, TriangleMesh, Value]) {
    def mesh: TriangleMesh[_3D] = df.domain
  }

  def apply[D, DDomain[D] <: DiscreteDomain[D], A](domain: DDomain[D],
                                                   data: IndexedSeq[A]): DiscreteField[D, DDomain, A] =
    new DiscreteField[D, DDomain, A](domain, data)

  def apply[D, DDomain[D] <: DiscreteDomain[D], A](domain: DDomain[D],
                                                   values: Point[D] => A): DiscreteField[D, DDomain, A] = {
    val valueSeq = domain.pointSet.points.map(values).toIndexedSeq
    new DiscreteField(domain, valueSeq)
  }

  private[scalismo] def createFromDenseVector[D, DDomain[D] <: DiscreteDomain[D], A](
    domain: DDomain[D],
    d: DenseVector[Double]
  )(implicit vectorizer: Vectorizer[A]) = {
    val dim = vectorizer.dim
    val data = d.toArray.grouped(dim).map(e => vectorizer.unvectorize(DenseVector(e))).toIndexedSeq
    new DiscreteField(domain, data)
  }

  private[scalismo] def vectorize[D, DDomain[D] <: DiscreteDomain[D], A](
    field: DiscreteField[D, DDomain, A]
  )(implicit vectorizer: Vectorizer[A]): DenseVector[Double] = {
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

  private[scalismo] def vectorize[D, A](
    values: IndexedSeq[A]
  )(implicit vectorizer: Vectorizer[A]): DenseVector[Double] = {
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

  implicit class DiscreteImage[D: NDSpace, A](discreteField: DiscreteField[D, DiscreteImageDomain, A]) {

    //private val pointSet = discreteField.pointSet
    //val dimensionality = ndSpace.dimensionality

    def apply(idx: IntVector[D]): A = discreteField(discreteField.domain.pointSet.pointId(idx))

    def isDefinedAt(idx: IntVector[D]): Boolean = {
      discreteField.domain.pointSet.isDefinedAt(idx)
    }

  }

}

trait DiscreteFieldWarp[D, DDomain[D] <: DiscreteDomain[D], Value] {

  def transformWithField(discreteField: DiscreteField[D, DDomain, Value],
                         warpField: DiscreteField[D, DDomain, EuclideanVector[D]])(
    implicit domainWarp: DomainWarp[D, DDomain]
  ): DDomain[D]

  def transform(discreteField: DiscreteField[D, DDomain, Value], transformation: Transformation[D])(
    implicit domainWarp: DomainWarp[D, DDomain]
  ): DiscreteField[D, DDomain, Value]
}

object ScalarMeshField {
  def apply[S: Scalar: ClassTag](mesh: TriangleMesh[_3D], data: Traversable[S]): ScalarMeshField[S] = {
    DiscreteField(mesh, ScalarArray(data.toArray))
  }
}
