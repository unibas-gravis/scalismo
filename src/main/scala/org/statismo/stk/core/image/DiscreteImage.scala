package org.statismo.stk.core
package image

import scala.language.implicitConversions
import scala.{ specialized => spec }
import reflect.runtime.universe.{ TypeTag, typeOf }
import scala.reflect.ClassTag
import scala.reflect.ClassTag
import org.statismo.stk.core.geometry._
import org.statismo.stk.core.common.PointData
import org.statismo.stk.core.common.ScalarPointData
import spire.math.Numeric

trait DiscreteImage[D <: Dim, @specialized(Float, Short) Pixel] extends PointData[D, Pixel] {
  def ndSpace : NDSpace[D]
  def domain: DiscreteImageDomain[D]
  val dimensionality = ndSpace.dimensionality

  def apply(idx: Index[D]): Pixel = values(domain.indexToLinearIndex(idx))
  def isDefinedAt(idx: Index[D]): Boolean = {
    (0 until dimensionality).foldLeft(true)((res, d) => res && idx(d) >= 0 && idx(d) < domain.size(d))
  }
  override lazy val hashCode = super.hashCode

}

class DiscreteScalarImage[D <: Dim: NDSpace, A : Numeric : ClassTag] private (val domain: DiscreteImageDomain[D], val values: Array[A]) extends DiscreteImage[D, A] with ScalarPointData[D, A] {
  require(domain.numberOfPoints == values.size)

  override def numeric = implicitly[Numeric[A]]
  override def ndSpace = implicitly[NDSpace[D]]


  def map[B: Numeric : ClassTag](f: A => B) : DiscreteScalarImage[D, B] = {
    new DiscreteScalarImage(domain, values.map(f))
  }
}


object DiscreteScalarImage {


  trait Create[D <: Dim] {
    def createDiscreteScalarImage[A : Numeric : ClassTag](domain : DiscreteImageDomain[D], values : Array[A]) : DiscreteScalarImage[D, A]
  }

  implicit object createDiscreteScalarImage1D extends Create[_1D] {
    override def createDiscreteScalarImage[A: Numeric : ClassTag](domain: DiscreteImageDomain[_1D], values: Array[A]): DiscreteScalarImage[_1D, A] = {
      new DiscreteScalarImage[_1D, A](domain, values)
    }
  }

  implicit object createDiscreteScalarImage2D extends Create[_2D] {
    override def createDiscreteScalarImage[A: Numeric : ClassTag](domain: DiscreteImageDomain[_2D], values: Array[A]): DiscreteScalarImage[_2D, A] = {
      new DiscreteScalarImage[_2D, A](domain, values)
    }
  }

  implicit object createDiscreteScalarImage3D extends Create[_3D] {
    override def createDiscreteScalarImage[A: Numeric : ClassTag](domain: DiscreteImageDomain[_3D], values: Array[A]): DiscreteScalarImage[_3D, A] = {
      new DiscreteScalarImage[_3D, A](domain, values)
    }
  }


  def apply[D <: Dim : NDSpace, A : Numeric : ClassTag](domain : DiscreteImageDomain[D], values : Array[A])(implicit evCreateImage : Create[D]) = {
    evCreateImage.createDiscreteScalarImage(domain, values)
  }

  def apply[D <: Dim : NDSpace, A : Numeric : ClassTag](domain : DiscreteImageDomain[D], f : Point[D] => A)(implicit evCreateImage : Create[D]) = {
    evCreateImage.createDiscreteScalarImage(domain, domain.points.map(f).toArray)
  }





}

