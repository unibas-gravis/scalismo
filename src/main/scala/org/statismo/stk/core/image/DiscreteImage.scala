package org.statismo.stk.core
package image

import scala.language.implicitConversions
import scala.{ specialized => spec }
import reflect.runtime.universe.{ TypeTag, typeOf }
import scala.reflect.ClassTag

import breeze.linalg.DenseVector

import breeze.linalg.DenseMatrix
import scala.reflect.ClassTag
import scala.util.Random
import org.statismo.stk.core.geometry._

abstract class DiscreteImage[D <: Dim, @specialized(Float, Short) Pixel] extends PartialFunction[Int, Pixel] {
  def domain: DiscreteImageDomain[D]
  def pixelDimensionality: Int
  def pixelValues: Array[Pixel]
  def apply(idx: Int): Pixel = pixelValues(idx)
  def apply(idx: Index[D]): Pixel = pixelValues(domain.indexToLinearIndex(idx))
  def isDefinedAt(idx: Int) = idx >= 0 && idx <= pixelValues.size
  def isDefinedAt(idx: Index[D]): Boolean = {
    (0 until domain.dimensionality).foldLeft(true)((res, d) => res && idx(d) >= 0 && idx(d) <= domain.size(d))
  }

  def map[Pixel2: ScalarPixel : ClassTag](f: Pixel => Pixel2): DiscreteScalarImage[D, Pixel2]
  def foreach[A](f: Pixel => A): Unit = pixelValues.foreach(f)
  
  override def hashCode = pixelValues.deep.hashCode
  override def equals(other: Any): Boolean = other match {
    case that: DiscreteImage[D, Pixel] => {
      that.canEqual(this) && this.pixelValues.deep == that.pixelValues.deep && this.domain == that.domain
    }
    case _ => false
  }
  
  def canEqual(other: Any): Boolean = other.isInstanceOf[DiscreteImage[D, Pixel]]  
}

abstract class DiscreteScalarImage[D <: Dim, Pixel] extends DiscreteImage[D, Pixel] {
  def pixelDimensionality = 1

}

case class DiscreteScalarImage1D[@specialized(Short, Float) Pixel: ScalarPixel](val domain: DiscreteImageDomain1D, val pixelValues: Array[Pixel]) extends DiscreteScalarImage[OneD, Pixel] {
  require(domain.numberOfPoints == pixelValues.size)
  def map[@specialized(Short, Float) A: ScalarPixel  : ClassTag](f: Pixel => A) = DiscreteScalarImage1D(this.domain, this.pixelValues.map(f))
  
}

case class DiscreteScalarImage2D[@specialized(Short, Float) Pixel: ScalarPixel](val domain: DiscreteImageDomain2D, val pixelValues: Array[Pixel]) extends DiscreteScalarImage[TwoD, Pixel] {
  require(domain.numberOfPoints == pixelValues.size)
  def map[@specialized(Short, Float) A: ScalarPixel  : ClassTag](f: Pixel => A) = DiscreteScalarImage2D(this.domain, this.pixelValues.map(f))
}

case class DiscreteScalarImage3D[@specialized(Short, Float) Pixel: ScalarPixel](val domain: DiscreteImageDomain3D, val pixelValues: Array[Pixel]) extends DiscreteScalarImage[ThreeD, Pixel] {
  require(domain.numberOfPoints == pixelValues.size)
  def map[@specialized(Short, Float) A: ScalarPixel : ClassTag](f: Pixel => A) = DiscreteScalarImage3D(this.domain, this.pixelValues.map(f))
}


object DiscreteImage1D {

  def random[Pixel: ScalarPixel : ClassTag](domain: DiscreteImageDomain1D): DiscreteScalarImage1D[Pixel] = {
    val scalarPixel = implicitly[ScalarPixel[Pixel]]
    val N = domain.numberOfPoints
    val values = for (i <- 0 to N) yield Random.nextFloat
    DiscreteScalarImage1D(domain, values.map(scalarPixel.fromFloat(_)).toArray)
  }

}

object DiscreteImage2D {

  def random[Pixel: ScalarPixel : ClassTag](domain: DiscreteImageDomain2D): DiscreteScalarImage2D[Pixel] = {
    val scalarPixel = implicitly[ScalarPixel[Pixel]]
    val N = domain.numberOfPoints
    val values = for (i <- 0 to N) yield Random.nextFloat
    DiscreteScalarImage2D(domain, values.map(scalarPixel.fromFloat(_)).toArray)
  }

}

object DiscreteImage3D {

  def random[Pixel: ScalarPixel : ClassTag](domain: DiscreteImageDomain3D): DiscreteScalarImage3D[Pixel] = {
    val scalarPixel = implicitly[ScalarPixel[Pixel]]
    val N = domain.numberOfPoints
    val values = for (i <- 0 to N) yield Random.nextFloat
    DiscreteScalarImage3D(domain, values.map(scalarPixel.fromFloat(_)).toArray)
  }

}
