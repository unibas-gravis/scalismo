package org.statismo.stk.core.common

import org.statismo.stk.core.geometry.{Dim, Point}
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

  def foreach(f: A => A): Unit = values.foreach(f)

  def map[B](f: A => B): PointData[D, B] = new PointData[D, B] {
    override def domain = self.domain
    override def values = self.values.map(f)
    override def apply(i : Int) = f(self(i))
    override def isDefinedAt(i : Int) = self.isDefinedAt(i : Int)
  }

}

/**
 *
 */
trait ScalarPointData[D <: Dim, A] extends PointData[D, A] {

  protected[this] def numeric : Numeric[A]

  /** map the function f over the values, but ensures that the result is scalar valued as well */
  def mapScalar[B: Numeric : ClassTag](f: A => B): ScalarPointData[D, B]

}

/**
 * This trait is used to conveniently define point data that is defined as an array
  */
trait PointDataAsArray[D <: Dim, A] { self : PointData[D, A] =>
  def data : Array[A]

  override def values = data.iterator
  override def apply(ptId : Int) = data(ptId)
  override def isDefinedAt(ptId : Int) = data.isDefinedAt(ptId)


  override def equals(other: Any): Boolean =
    other match {

      case that: PointData[D, A] with PointDataAsArray[D, A] =>
        (that canEqual this) &&
          data.deep == that.data.deep &&
          domain == that.domain

      case _ => false
    }

  def canEqual(other: Any): Boolean =
    other.isInstanceOf[PointData[D, A]]

  override lazy val hashCode: Int = data.hashCode() + domain.hashCode()

}

