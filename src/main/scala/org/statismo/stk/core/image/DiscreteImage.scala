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
import org.statismo.stk.core.common.ScalarPointData

abstract class DiscreteImage[D <: Dim: DimOps, @specialized(Float, Short) Pixel] extends ScalarPointData[D, Pixel] {
  def domain: DiscreteImageDomain[D]
  val dimensionality = implicitly[DimOps[D]].toInt

  def apply(idx: Index[D]): Pixel = values(domain.indexToLinearIndex(idx))
  def isDefinedAt(idx: Index[D]): Boolean = {
    (0 until dimensionality).foldLeft(true)((res, d) => res && idx(d) >= 0 && idx(d) < domain.size(d))
  }
  override lazy val hashCode = super.hashCode

}

abstract class DiscreteScalarImage[D <: Dim: DimOps, Pixel] extends DiscreteImage[D, Pixel] {
  def valueDimensionality = 1
  def map[Pixel2: ScalarValue: ClassTag](f: Pixel => Pixel2): DiscreteScalarImage[D, Pixel2]
}

case class DiscreteScalarImage1D[@specialized(Short, Float) Pixel: ScalarValue](val domain: DiscreteImageDomain[_1D], val values: Array[Pixel]) extends DiscreteScalarImage[_1D, Pixel] {
  require(domain.numberOfPoints == values.size)
  def map[@specialized(Short, Float) A: ScalarValue: ClassTag](f: Pixel => A) = DiscreteScalarImage1D(this.domain, this.values.map(f))
}

case class DiscreteScalarImage2D[@specialized(Short, Float) Pixel: ScalarValue](val domain: DiscreteImageDomain[_2D], val values: Array[Pixel]) extends DiscreteScalarImage[_2D, Pixel] {
  require(domain.numberOfPoints == values.size)
  def map[@specialized(Short, Float) A: ScalarValue: ClassTag](f: Pixel => A) = DiscreteScalarImage2D(this.domain, this.values.map(f))
}

case class DiscreteScalarImage3D[@specialized(Short, Float) Pixel: ScalarValue](val domain: DiscreteImageDomain[_3D], val values: Array[Pixel]) extends DiscreteScalarImage[_3D, Pixel] {
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
