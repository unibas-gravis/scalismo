package org.statismo.stk.core
package image

import scala.language.implicitConversions
import scala.{ specialized => spec }
import reflect.runtime.universe.{ TypeTag, typeOf }
import scala.reflect.ClassTag
import breeze.linalg.DenseVector
import numerics.Integrator
import breeze.linalg.DenseMatrix
import scala.reflect.ClassTag
import scala.util.Random
import org.statismo.stk.core.registration.{ CanDifferentiate, Transformation }
import org.statismo.stk.core.numerics.Integrator
import org.statismo.stk.core.geometry._
import org.statismo.stk.core.common.Domain
import org.statismo.stk.core.common.ImplicitDomain
import org.statismo.stk.core.registration.CanDifferentiate
import org.statismo.stk.core.registration.CanDifferentiate
import org.statismo.stk.core.numerics.IntegratorConfiguration
import org.statismo.stk.core.numerics.UniformSampler2D
import org.statismo.stk.core.common.BoxedDomain2D
import org.statismo.stk.core.numerics.UniformSampler1D
import org.statismo.stk.core.common.BoxedDomain1D
import org.statismo.stk.core.numerics.UniformSampler3D
import org.statismo.stk.core.common.BoxedDomain3D

/**
 * The generic interface for continuous images
 */
abstract class ContinuousImage[D <: Dim, @specialized(Short, Float) Pixel] extends Function1[Point[D], Pixel] { self =>

  /** the function that defines the image values */
  val f: Point[D] => Pixel

  def apply(x: Point[D]): Pixel = {
    if (!isDefinedAt(x)) throw new Exception(s"Point $x is outside the domain")
    f(x)
  }

  def liftPixelValue: (Point[D] => Option[Pixel]) = { x =>
    if (isDefinedAt(x)) Some(f(x))
    else None
  }

  def domain: Domain[D]
  def isDefinedAt(pt: Point[D]): Boolean = domain.isDefinedAt(pt)

  //def differentiate[Value2](): ContinuousImage[Point, Value2]

  def pixelDimensionality: Int

  def lift(fl: Pixel => Pixel): ContinuousImage[D, Pixel] => ContinuousImage[D, Pixel] = {
    img: ContinuousImage[D, Pixel] =>
      new ContinuousImage[D, Pixel] {
        override def apply(x: Point[D]) = fl(img.apply(x))
        def pixelDimensionality = img.pixelDimensionality
        val f = img.f
        def domain = img.domain
      }
  }

  //TODO add derivative here (use coordinatesVector for return type)
}

abstract class ContinuousVectorImage[D <: Dim] extends ContinuousImage[D, Vector[D]] {}

abstract class ContinuousScalarImage[D <: Dim] extends ContinuousImage[D, Float] { self: ContinuousImage[D, Float] =>
  type CI = ContinuousScalarImage[D]

  val pixelDimensionality = 1

  /** the function that defines the derivative, if it exists */
  val df: Option[Point[D] => Vector[D]]

  def differentiate: Option[ContinuousVectorImage[D]] = {
    self.df.map(df =>
      new ContinuousVectorImage[D] {
        val f = (x: Point[D]) => df(x)
        def domain = self.domain
        def pixelDimensionality = self.pixelDimensionality
      })
  }

  def +(that: CI): CI

  def -(that: CI): CI

  def :*(that: CI): CI

  def *(s: Double): CI

  def square: CI

  def compose(t: Transformation[D]): CI

}

/**
 * Implementation trait which can be used to specialize the functionality of ContinuousScalarImages for different subtypes
 */
trait ContinuousScalarImageLike[D <: Dim, Repr <: ContinuousScalarImage[D]] { self: ContinuousScalarImage[D] =>

  def newConcreteImageRepr(domain: Domain[D], f: Point[D] => Float, df: Option[Point[D] => Vector[D]]): Repr

  def +(that: CI): Repr = {
    def f(x: Point[D]): Float = self.f(x) + that.f(x)
    def df = for (selfdf <- self.df; thatdf <- that.df) yield ((x: Point[D]) => selfdf(x) + thatdf(x))
    newConcreteImageRepr(self.domain.intersection(that.domain), f, df)
  }

  def -(that: CI): Repr = {
    def f(x: Point[D]): Float = self.f(x) - that.f(x)
    def df = for (seldf <- self.df; thatdf <- that.df) yield (x: Point[D]) => seldf(x) - thatdf(x)
    val newDomain = self.domain.intersection(that.domain)
    newConcreteImageRepr(newDomain, f, df)
  }

  def :*(that: CI): Repr = {
    def f(x: Point[D]): Float = self.f(x) * that.f(x)
    def df = for (selfdf <- self.df; thatdf <- that.df) yield ((x: Point[D]) => selfdf(x) * that(x) + thatdf(x) * self.f(x))
    val newDomain = self.domain.intersection(that.domain)
    newConcreteImageRepr(newDomain, f, df)
  }

  def *(s: Double): Repr = {
    def f(x: Point[D]): Float = self.f(x) * s.toFloat
    val df = for (selfdf <- self.df) yield (x: Point[D]) => selfdf(x) * s.toFloat
    val newDomain = self.domain
    newConcreteImageRepr(newDomain, f, df)
  }

  def square: Repr = {
    def f(x: Point[D]): Float = self.f(x) * self.f(x)
    val df = for (selfdf <- self.df) yield (x: Point[D]) => selfdf(x) * self.f(x) * 2f
    val newDomain = self.domain
    newConcreteImageRepr(newDomain, f, df)
  }

  def compose(t: Transformation[D]): Repr = {
    def f(x: Point[D]) = self.f(t(x))
    val df = t match {
      case t1: CanDifferentiate[D] => self.df.map { selfdf =>
        (x: Point[D]) => t1.takeDerivative(x) * selfdf(t(x))
      }
      case _ => None
    }

    val newDomain = new ImplicitDomain[D] {
      override val chi = (pt: Point[D]) => self.isDefinedAt(t(pt))
    }
    newConcreteImageRepr(newDomain, f, df)
  }
}

case class ContinuousScalarImage1D(val domain: Domain[OneD], val f: Point[OneD] => Float, val df: Option[Point[OneD] => Vector[OneD]] = None) extends ContinuousScalarImage[OneD] with ContinuousScalarImageLike[OneD, ContinuousScalarImage1D] {

  override val pixelDimensionality = 1
  def newConcreteImageRepr(domain: Domain[OneD], f: Point[OneD] => Float, df: Option[Point[OneD] => Vector[OneD]]): ContinuousScalarImage1D = ContinuousScalarImage1D(domain, f, df)

  def convolve(filter: Filter[OneD],  numberOfPoints:Int): ContinuousScalarImage1D = {
    def convolvedImgFun(x: Point[OneD]) = {

      def intermediateF(t: Point[OneD]): Float = {
        val p = Point1D(x(0) - t(0))
    
        this.liftPixelValue(p).getOrElse(0f) * filter(t)
      }

      val support = filter.support
      val integrator = Integrator[OneD](IntegratorConfiguration(UniformSampler1D(support.asInstanceOf[BoxedDomain1D], numberOfPoints)))

      val intermediateContinuousImage = ContinuousScalarImage1D(this.domain, intermediateF)
      integrator.integrateScalar(intermediateContinuousImage)

    }

    def convolvedImgDerivative: Option[Point[OneD] => Vector[OneD]] = {
      if (this.df.isDefined)
        Some((x: Point[OneD]) => {
          val thisDF = this.df.get
          def intermediateDF(t: Point[OneD]): Vector[OneD] = {
            val p = Point1D(x(0) - t(0))
            if (this.isDefinedAt(p))
              thisDF(p) * filter(t)
            else Vector1D(0)

          }

          val support = filter.support
          val integrator = Integrator[OneD](IntegratorConfiguration(UniformSampler1D(support.asInstanceOf[BoxedDomain1D], 9)))

          val intermediateContinuousImage = ContinuousVectorImage1D(this.domain, pixelDimensionality, intermediateDF, None)
          integrator.integrateVector(intermediateContinuousImage)
        })

      else None
    }

    ContinuousScalarImage1D(this.domain, convolvedImgFun, convolvedImgDerivative)
  }

}

case class ContinuousScalarImage2D(domain: Domain[TwoD], val f: Point[TwoD] => Float, val df: Option[Point[TwoD] => Vector[TwoD]] = None) extends ContinuousScalarImage[TwoD] with ContinuousScalarImageLike[TwoD, ContinuousScalarImage2D] {

  def newConcreteImageRepr(domain: Domain[TwoD], f: Point[TwoD] => Float, df: Option[Point[TwoD] => Vector[TwoD]]): ContinuousScalarImage2D = ContinuousScalarImage2D(domain, f, df)

  override val pixelDimensionality = 1

  def convolve(filter: Filter[TwoD], numberOfPoints:Int): ContinuousScalarImage2D = {

    def f(x: Point[TwoD]) = {

      def intermediateF(t: Point[TwoD]) = {
        val p = Point2D(x(0) - t(0), x(1) - t(1))
        this.liftPixelValue(p).getOrElse(0f) * filter(t)
      }

      val support = filter.support
      val integrator = Integrator[TwoD](IntegratorConfiguration(UniformSampler2D(support, numberOfPoints)))

      val intermediateContinuousImage = ContinuousScalarImage2D(this.domain, intermediateF)
      integrator.integrateScalar(intermediateContinuousImage)

    }

    def convolvedImgDerivative: Option[Point[TwoD] => Vector[TwoD]] = {
      if (this.df.isDefined)
        Some((x: Point[TwoD]) => {
          val thisDF = this.df.get
          def intermediateDF(t: Point[TwoD]) = {
            val p = Point2D(x(0) - t(0), x(1) - t(1))

            if (this.isDefinedAt(p))
              thisDF(p) * filter(t)
            else Vector2D(0, 0)

          }

          val support = filter.support
          val integrator = Integrator[TwoD](IntegratorConfiguration(UniformSampler2D(support, numberOfPoints)))

          val intermediateContinuousImage = ContinuousVectorImage2D(this.domain, 2, intermediateDF, None)
          integrator.integrateVector(intermediateContinuousImage)
        })

      else None

    }

    ContinuousScalarImage2D(this.domain, f, convolvedImgDerivative)
  }

}

case class ContinuousScalarImage3D(domain: Domain[ThreeD], val f: Point[ThreeD] => Float, val df: Option[Point[ThreeD] => Vector[ThreeD]] = None) extends ContinuousScalarImage[ThreeD] with ContinuousScalarImageLike[ThreeD, ContinuousScalarImage3D] {

  def newConcreteImageRepr(domain: Domain[ThreeD], f: Point[ThreeD] => Float, df: Option[Point[ThreeD] => Vector[ThreeD]]): ContinuousScalarImage3D = ContinuousScalarImage3D(domain, f, df)

  override val pixelDimensionality = 1

  def convolve(filter: Filter[ThreeD], numberOfPoints : Int): ContinuousScalarImage3D = {

    def f(x: Point[ThreeD]) = {

      def intermediateF(t: Point[ThreeD]) = {
        val p = Point3D(x(0) - t(0), x(1) - t(1), x(2) - t(2))
        this.liftPixelValue(p).getOrElse(0f) * filter(t)
      }

      val support = filter.support

      val integrator = Integrator[ThreeD](IntegratorConfiguration(UniformSampler3D(support.asInstanceOf[BoxedDomain3D], numberOfPoints)))

      val intermediateContinuousImage = ContinuousScalarImage3D(this.domain, intermediateF)
      integrator.integrateScalar(intermediateContinuousImage)

    }

    def convolvedImgDerivative: Option[Point[ThreeD] => Vector[ThreeD]] = {
      if (this.df.isDefined)
        Some((x: Point[ThreeD]) => {
          val thisDF = this.df.get
          def intermediateDF(t: Point[ThreeD]) = {
            val p = Point3D(x(0) - t(0), x(1) - t(1), x(2) - t(2))

            if (this.isDefinedAt(p))
              thisDF(p) * filter(t)
            else Vector3D(0, 0, 0)

          }
          val support = filter.support
          val integrator = Integrator[ThreeD](IntegratorConfiguration(UniformSampler3D(support.asInstanceOf[BoxedDomain3D], numberOfPoints)))

          val intermediateContinuousImage = ContinuousVectorImage3D(this.domain, 3, intermediateDF, None)
          integrator.integrateVector(intermediateContinuousImage)
        })

      else None

    }

    ContinuousScalarImage3D(this.domain, f, convolvedImgDerivative)
  }
}

/////////////////////////////////////////////
// Vector Images
/////////////////////////////////////////////

case class ContinuousVectorImage1D(val domain: Domain[OneD], val pixelDimensionality: Int, val f: Point[OneD] => Vector[OneD], val df: Option[Point[OneD] => Matrix1x1]) extends ContinuousVectorImage[OneD] {}

case class ContinuousVectorImage2D(val domain: Domain[TwoD], val pixelDimensionality: Int, val f: Point[TwoD] => Vector[TwoD], val df: Option[Point[TwoD] => Matrix2x2]) extends ContinuousVectorImage[TwoD] {}

case class ContinuousVectorImage3D(val domain: Domain[ThreeD], val pixelDimensionality: Int, val f: Point[ThreeD] => Vector[ThreeD], val df: Option[Point[ThreeD] => Matrix3x3]) extends ContinuousVectorImage[ThreeD] {}

