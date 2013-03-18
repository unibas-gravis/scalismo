package smptk
package image

import scala.language.higherKinds
import scala.language.implicitConversions
import scala.{ specialized => spec }
import reflect.runtime.universe.{ TypeTag, typeOf }
import scala.reflect.ClassTag
import numerics.Integration._
import breeze.linalg.DenseVector
import numerics.Integration
import breeze.linalg.DenseMatrix
import image.Geometry._
import scala.reflect.ClassTag
import scala.util.Random

trait DiscreteImage[CV[A] <: CoordVector[A], @specialized(Float, Short) Pixel] extends PartialFunction[Int, Pixel] {
  def domain: DiscreteImageDomain[CV]
  def pixelDimensionality: Int
  def pixelValues: IndexedSeq[Pixel]
  def apply(idx: Int): Pixel = pixelValues(idx)
  def apply(idx: CV[Int]): Pixel = pixelValues(domain.indexToLinearIndex(idx))
  def isDefinedAt(idx: Int) = idx >= 0 && idx <= pixelValues.size
  def isDefinedAt(idx: CV[Int]): Boolean = {
    (0 until domain.dimensionality).foldLeft(true)((res, d) => res && idx(d) >= 0 && idx(d) <= domain.size(d))
  }

  def map[Pixel2: ScalarPixel : ClassTag](f: Pixel => Pixel2): DiscreteScalarImage[CV, Pixel2]
  def foreach[A](f: Pixel => A): Unit = pixelValues.foreach(f)
}

trait DiscreteScalarImage[CV[A] <: CoordVector[A], Pixel] extends DiscreteImage[CV, Pixel] {
  def pixelDimensionality = 1

}

case class DiscreteScalarImage1D[@specialized(Short, Float) Pixel: ScalarPixel](val domain: DiscreteImageDomain1D, val pixelValues: IndexedSeq[Pixel]) extends DiscreteScalarImage[CoordVector1D, Pixel] {
  require(domain.numberOfPoints == pixelValues.size)
  def map[@specialized(Short, Float) A: ScalarPixel  : ClassTag](f: Pixel => A) = DiscreteScalarImage1D(this.domain, this.pixelValues.map(f))
}

case class DiscreteScalarImage2D[@specialized(Short, Float) Pixel: ScalarPixel](val domain: DiscreteImageDomain2D, val pixelValues: IndexedSeq[Pixel]) extends DiscreteScalarImage[CoordVector2D, Pixel] {
  require(domain.numberOfPoints == pixelValues.size)
  def map[@specialized(Short, Float) A: ScalarPixel  : ClassTag](f: Pixel => A) = DiscreteScalarImage2D(this.domain, this.pixelValues.map(f))
}

case class DiscreteScalarImage3D[@specialized(Short, Float) Pixel: ScalarPixel](val domain: DiscreteImageDomain3D, val pixelValues: IndexedSeq[Pixel]) extends DiscreteScalarImage[CoordVector3D, Pixel] {
  require(domain.numberOfPoints == pixelValues.size)
  def map[@specialized(Short, Float) A: ScalarPixel : ClassTag](f: Pixel => A) = DiscreteScalarImage3D(this.domain, this.pixelValues.map(f))
}


object DiscreteImage1D {

  def random[Pixel: ScalarPixel : ClassTag](domain: DiscreteImageDomain1D): DiscreteScalarImage1D[Pixel] = {
    val scalarPixel = implicitly[ScalarPixel[Pixel]]
    val N = domain.numberOfPoints
    val values = for (i <- 0 to N) yield Random.nextFloat
    DiscreteScalarImage1D(domain, values.map(scalarPixel.fromFloat(_)))
  }

}

object DiscreteImage2D {

  def random[Pixel: ScalarPixel : ClassTag](domain: DiscreteImageDomain2D): DiscreteScalarImage2D[Pixel] = {
    val scalarPixel = implicitly[ScalarPixel[Pixel]]
    val N = domain.numberOfPoints
    val values = for (i <- 0 to N) yield Random.nextFloat
    DiscreteScalarImage2D(domain, values.map(scalarPixel.fromFloat(_)))
  }

}

object DiscreteImage3D {

  def random[Pixel: ScalarPixel : ClassTag](domain: DiscreteImageDomain3D): DiscreteScalarImage3D[Pixel] = {
    val scalarPixel = implicitly[ScalarPixel[Pixel]]
    val N = domain.numberOfPoints
    val values = for (i <- 0 to N) yield Random.nextFloat
    DiscreteScalarImage3D(domain, values.map(scalarPixel.fromFloat(_)))
  }

}
