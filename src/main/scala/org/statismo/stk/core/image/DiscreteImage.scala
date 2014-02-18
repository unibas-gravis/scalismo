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
import org.statismo.stk.core.common.PointData
import org.statismo.stk.core.common.ScalarValue

abstract class DiscreteImage[D <: Dim, @specialized(Float, Short) Pixel] extends PointData[D, Pixel] {
  def domain: DiscreteImageDomain[D]


  def apply(idx: Index[D]): Pixel = values(domain.indexToLinearIndex(idx))
  def isDefinedAt(idx: Index[D]): Boolean = {
    (0 until domain.dimensionality).foldLeft(true)((res, d) => res && idx(d) >= 0 && idx(d) < domain.size(d))
  }

}

abstract class DiscreteScalarImage[D <: Dim, Pixel] extends DiscreteImage[D, Pixel] {
  def valueDimensionality = 1
  def map[Pixel2: ScalarValue : ClassTag](f: Pixel => Pixel2): DiscreteScalarImage[D, Pixel2]
}

case class DiscreteScalarImage1D[@specialized(Short, Float) Pixel: ScalarValue](val domain: DiscreteImageDomain1D, val values: Array[Pixel]) extends DiscreteScalarImage[OneD, Pixel] {
  require(domain.numberOfPoints == values.size)
  def map[@specialized(Short, Float) A: ScalarValue : ClassTag](f: Pixel => A) = DiscreteScalarImage1D(this.domain, this.values.map(f))

}

case class DiscreteScalarImage2D[@specialized(Short, Float) Pixel: ScalarValue](val domain: DiscreteImageDomain2D, val values: Array[Pixel]) extends DiscreteScalarImage[TwoD, Pixel] {
  require(domain.numberOfPoints == values.size)
  def map[@specialized(Short, Float) A: ScalarValue: ClassTag](f: Pixel => A) = DiscreteScalarImage2D(this.domain, this.values.map(f))
}

case class DiscreteScalarImage3D[@specialized(Short, Float) Pixel: ScalarValue](val domain: DiscreteImageDomain3D, val values: Array[Pixel]) extends DiscreteScalarImage[ThreeD, Pixel] {
  require(domain.numberOfPoints == values.size)
  def map[@specialized(Short, Float) A: ScalarValue: ClassTag](f: Pixel => A) = DiscreteScalarImage3D(this.domain, this.values.map(f))
}

object DiscreteImage1D {

  def random[Pixel: ScalarValue: ClassTag](domain: DiscreteImageDomain1D): DiscreteScalarImage1D[Pixel] = {
    val ScalarValue = implicitly[ScalarValue[Pixel]]
    val N = domain.numberOfPoints
    val values = for (i <- 0 to N) yield Random.nextFloat
    DiscreteScalarImage1D(domain, values.map(ScalarValue.fromFloat(_)).toArray)
  }

}

object DiscreteImage2D {

  def random[Pixel: ScalarValue: ClassTag](domain: DiscreteImageDomain2D): DiscreteScalarImage2D[Pixel] = {
    val ScalarValue = implicitly[ScalarValue[Pixel]]
    val N = domain.numberOfPoints
    val values = for (i <- 0 to N) yield Random.nextFloat
    DiscreteScalarImage2D(domain, values.map(ScalarValue.fromFloat(_)).toArray)
  }

}

object DiscreteImage3D {

  def random[Pixel: ScalarValue: ClassTag](domain: DiscreteImageDomain3D): DiscreteScalarImage3D[Pixel] = {
    val ScalarValue = implicitly[ScalarValue[Pixel]]
    val N = domain.numberOfPoints
    val values = for (i <- 0 to N) yield Random.nextFloat
    DiscreteScalarImage3D(domain, values.map(ScalarValue.fromFloat(_)).toArray)
  }

}
