package smptk
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
import registration.Transformation
import smptk.numerics.Integrator
import smptk.geometry._
import smptk.common.Domain
import smptk.common.ImplicitDomain

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


  def domain : Domain[D]
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

abstract class ContinuousVectorImage[D <: Dim] extends ContinuousImage[D, DenseVector[Double]] {}

abstract class ContinuousScalarImage[D <: Dim] extends ContinuousImage[D, Double] { self: ContinuousImage[D, Double] =>
  type CI = ContinuousScalarImage[D]

  val pixelDimensionality = 1

  /** the function that defines the derivative, if it exists */
  val df: Option[Point[D] => DenseVector[Double]]

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

  def newConcreteImageRepr(domain: Domain[D], f: Point[D] => Double, df: Option[Point[D] => DenseVector[Double]]): Repr

  def +(that: CI): Repr = {
    def f(x: Point[D]): Double = self.f(x) + that.f(x)
    def df = for (selfdf <- self.df; thatdf <- that.df) yield ((x: Point[D]) => selfdf(x) + thatdf(x))
    newConcreteImageRepr(self.domain.intersection(that.domain), f, df)
  }

  def -(that: CI): Repr = {
    def f(x: Point[D]): Double = self.f(x) - that.f(x)
    def df = for (seldf <- self.df; thatdf <- that.df) yield (x: Point[D]) => seldf(x) - thatdf(x)
    val newDomain = self.domain.intersection(that.domain)
    newConcreteImageRepr(newDomain, f, df)
  }

  def :*(that: CI): Repr = {
    def f(x: Point[D]): Double = self.f(x) * that.f(x)
    def df = for (selfdf <- self.df; thatdf <- that.df) yield ((x: Point[D]) => selfdf(x) * that(x) + thatdf(x) * self.f(x))
    val newDomain = self.domain.intersection(that.domain)
    newConcreteImageRepr(newDomain, f, df)
  }

  def *(s: Double): Repr = {
    def f(x: Point[D]): Double = self.f(x) * s
    val df = for (selfdf <- self.df) yield (x: Point[D]) => selfdf(x) * s
    val newDomain = self.domain
    newConcreteImageRepr(newDomain, f, df)
  }

  def square: Repr = {
    def f(x: Point[D]): Double = self.f(x) * self.f(x)
    val df = for (selfdf <- self.df) yield (x: Point[D]) => selfdf(x) * self.f(x) * 2.0
    val newDomain = self.domain
    newConcreteImageRepr(newDomain, f, df)
  }

  def compose(t: Transformation[D]): Repr = {
    def f(x: Point[D]) = self.f(t(x))
    val df = for (selfdf <- self.df) yield ((x: Point[D]) => t.takeDerivative(x) * selfdf(t(x)))
    val newDomain = new ImplicitDomain[D] {
      override val chi = (pt: Point[D]) => self.isDefinedAt(t(pt))
    }
    newConcreteImageRepr(newDomain, f, df)
  }
}

case class ContinuousScalarImage1D(val domain: Domain[OneD], val f: Point[OneD] => Double, val df: Option[Point[OneD] => DenseVector[Double]] = None) extends ContinuousScalarImage[OneD] with ContinuousScalarImageLike[OneD, ContinuousScalarImage1D] {

  override val pixelDimensionality = 1
  def newConcreteImageRepr(domain: Domain[OneD], f: Point[OneD] => Double, df: Option[Point[OneD] => DenseVector[Double]]): ContinuousScalarImage1D = ContinuousScalarImage1D(domain, f, df)

  def convolve(filter: Filter[OneD], integrator: Integrator[OneD]): ContinuousScalarImage1D = {
    def convolvedImgFun(x: Point[OneD]) = {

      // f(x) = Int (Img(x-t)*G(t) dt
      def intermediateF(t: Point[OneD]) = {
        val p = Point1D(x(0) - t(0))
        this.liftPixelValue(p).getOrElse(0.0) * filter(t)
      }

      val intermediateContinuousImage = ContinuousScalarImage1D(this.domain, intermediateF)
      integrator.integrateScalar(intermediateContinuousImage, filter.support)

    }

    def convolvedImgDerivative: Option[Point[OneD] => DenseVector[Double]] = {
      if (this.df.isDefined)
        Some((x: Point[OneD]) => {
          val thisDF = this.df.get
          def intermediateDF(t: Point[OneD]) = {
            val p = Point1D(x(0) - t(0))
            if (this.isDefinedAt(p))
              thisDF(p) * filter(t)
            else DenseVector.zeros[Double](pixelDimensionality)

          }
          val intermediateContinuousImage = ContinuousVectorImage1D(this.domain, pixelDimensionality, intermediateDF, None)
          integrator.integrateVector(intermediateContinuousImage, filter.support)
        })

      else None
    }

    ContinuousScalarImage1D(this.domain, convolvedImgFun, convolvedImgDerivative)
  }

}


case class ContinuousScalarImage2D(domain : Domain[TwoD], val f: Point[TwoD] => Double, val df: Option[Point[TwoD] => DenseVector[Double]] = None) extends ContinuousScalarImage[TwoD] with ContinuousScalarImageLike[TwoD, ContinuousScalarImage2D] {

  def newConcreteImageRepr(domain : Domain[TwoD], f: Point[TwoD] => Double, df: Option[Point[TwoD] => DenseVector[Double]]): ContinuousScalarImage2D = ContinuousScalarImage2D(domain, f, df)


  override val pixelDimensionality = 1

  def convolve(filter: Filter[TwoD], integrator: Integrator[TwoD]): ContinuousScalarImage2D = {

    def f(x: Point[TwoD]) = {

      def intermediateF(t: Point[TwoD]) = {
        val p = Point2D(x(0) - t(0), x(1) - t(1))
        this.liftPixelValue(p).getOrElse(0.0) * filter(t)
      }

      val intermediateContinuousImage = ContinuousScalarImage2D(this.domain, intermediateF)
      integrator.integrateScalar(intermediateContinuousImage, filter.support)

    }

    def convolvedImgDerivative: Option[Point[TwoD] => DenseVector[Double]] = {
      if (this.df.isDefined)
        Some((x: Point[TwoD]) => {
          val thisDF = this.df.get
          def intermediateDF(t: Point[TwoD]) = {
            val p = Point2D(x(0) - t(0), x(1) - t(1))

            if (this.isDefinedAt(p))
              thisDF(p) * filter(t)
            else DenseVector.zeros[Double](2)

          }
          val intermediateContinuousImage = ContinuousVectorImage2D(this.domain, 2, intermediateDF, None)
          integrator.integrateVector(intermediateContinuousImage, filter.support)
        })

      else None

    }

    ContinuousScalarImage2D(this.domain, f, convolvedImgDerivative)
  }

}

case class ContinuousScalarImage3D(domain: Domain[ThreeD], val f: Point[ThreeD] => Double, val df: Option[Point[ThreeD] => DenseVector[Double]] = None) extends ContinuousScalarImage[ThreeD] with ContinuousScalarImageLike[ThreeD, ContinuousScalarImage3D] {

  def newConcreteImageRepr(domain: Domain[ThreeD], f: Point[ThreeD] => Double, df: Option[Point[ThreeD] => DenseVector[Double]]): ContinuousScalarImage3D = ContinuousScalarImage3D(domain, f, df)

  override val pixelDimensionality = 1

  def convolve(filter: Filter[ThreeD], integrator: Integrator[ThreeD]): ContinuousScalarImage3D = {

    def f(x: Point[ThreeD]) = {

      def intermediateF(t: Point[ThreeD]) = {
        val p = Point3D(x(0) - t(0), x(1) - t(1), x(2) - t(2))
        this.liftPixelValue(p).getOrElse(0.0) * filter(t)
      }

      val intermediateContinuousImage = ContinuousScalarImage3D(this.domain, intermediateF)
      integrator.integrateScalar(intermediateContinuousImage, filter.support)

    }

    def convolvedImgDerivative: Option[Point[ThreeD] => DenseVector[Double]] = {
      if (this.df.isDefined)
        Some((x: Point[ThreeD]) => {
          val thisDF = this.df.get
          def intermediateDF(t: Point[ThreeD]) = {
            val p = Point3D(x(0) - t(0), x(1) - t(1), x(2) - t(2))

            if (this.isDefinedAt(p))
              thisDF(p) * filter(t)
            else DenseVector.zeros[Double](3)

          }
          val intermediateContinuousImage = ContinuousVectorImage3D(this.domain, 3, intermediateDF, None)
          integrator.integrateVector(intermediateContinuousImage, filter.support)
        })

      else None

    }

    ContinuousScalarImage3D(this.domain, f, convolvedImgDerivative)
  }
}

/////////////////////////////////////////////
// Vector Images
/////////////////////////////////////////////

case class ContinuousVectorImage1D(val domain : Domain[OneD], val pixelDimensionality: Int, val f: Point[OneD] => DenseVector[Double], val df: Option[Point[OneD] => DenseMatrix[Double]]) extends ContinuousVectorImage[OneD] {}

case class ContinuousVectorImage2D(val domain : Domain[TwoD], val pixelDimensionality: Int, val f: Point[TwoD] => DenseVector[Double], val df: Option[Point[TwoD] => DenseMatrix[Double]]) extends ContinuousVectorImage[TwoD] {}

case class ContinuousVectorImage3D(val domain : Domain[ThreeD], val pixelDimensionality: Int, val f: Point[ThreeD] => DenseVector[Double], val df: Option[Point[ThreeD] => DenseMatrix[Double]]) extends ContinuousVectorImage[ThreeD] {}

