package org.statismo.stk.core.common

import org.statismo.stk.core.geometry.{NDSpace, Dim, Point, Vector}
import scala.reflect.ClassTag
import spire.math.Numeric

/**
 * Defines a discrete set of values, where each associated to a point of the domain.
 */
trait PointData[D <: Dim, A] extends PartialFunction[Int, A] { self =>

  def domain: FiniteDiscreteDomain[D]

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
class ScalarPointData[D <: Dim, A : Numeric : ClassTag](val domain : FiniteDiscreteDomain[D], val data : Array[A]) extends PointData[D, A] {

  /** map the function f over the values, but ensures that the result is scalar valued as well */
  def map[B: Numeric : ClassTag](f: A => B): ScalarPointData[D, B] = {
    new ScalarPointData(domain, data.map(f))
  }


  override def values = data.iterator
  override def apply(ptId : Int) = data(ptId)
  override def isDefinedAt(ptId : Int) = data.isDefinedAt(ptId)


  override def equals(other: Any): Boolean =
    other match {

      case that: ScalarPointData[D, A]  =>
        (that canEqual this) &&
          data.deep == that.data.deep &&
          domain == that.domain

      case _ => false
    }

  def canEqual(other: Any): Boolean =
    other.isInstanceOf[PointData[D, A]]

  override lazy val hashCode: Int = data.hashCode() + domain.hashCode()


}

/**
 *
 */
class VectorPointData[D <: Dim, DO <: Dim]private (val domain : FiniteDiscreteDomain[D], val data : IndexedSeq[Vector[DO]]) extends PointData[D, Vector[DO]] {

  override def values = data.iterator
  override def apply(ptId : Int) = data(ptId)
  override def isDefinedAt(ptId : Int) = data.isDefinedAt(ptId)


  /** map the function f over the values, but ensures that the result is scalar valued as well */
  def map(f: Vector[DO] => Vector[DO]): VectorPointData[D, DO] = new VectorPointData(domain, data.map(f))

}


object VectorPointData {

  def apply[D <: Dim, DO <: Dim](domain : FiniteDiscreteDomain[D], data : IndexedSeq[Vector[DO]]) = {
    new VectorPointData(domain, data)
  }
}