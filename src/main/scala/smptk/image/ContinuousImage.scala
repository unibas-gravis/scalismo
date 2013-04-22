package smptk
package image

import scala.language.higherKinds
import scala.language.implicitConversions
import scala.{ specialized => spec }
import reflect.runtime.universe.{ TypeTag, typeOf }
import scala.reflect.ClassTag
import breeze.linalg.DenseVector
import numerics.Integrator
import breeze.linalg.DenseMatrix
import Geometry._
import scala.reflect.ClassTag
import scala.util.Random
import registration.Transformation
import smptk.common.BoxedRegion
import smptk.numerics.Integrator

/**
 * The generic interface for continuous images
 */
trait ContinuousImage[CV[A] <: CoordVector[A], @specialized(Short, Float) Pixel] {

  /** the function that defines the image values */
  val f: CV[Double] => Pixel

  def apply(x: CV[Double]): Pixel = {
    if (!isDefinedAt(x)) throw new Exception(s"Point $x is outside the domain")
    f(x)
  }

  def liftPixelValue: (CV[Double] => Option[Pixel]) = { x =>
    if (isDefinedAt(x)) Some(f(x))
    else None
  }

  def isDefinedAt(pt: CV[Double]): Boolean
  //def differentiate[Value2](): ContinuousImage[Point, Value2]

  def pixelDimensionality: Int

  //TODO add derivative here (use coordinatesVector for return type)
}

trait ContinuousVectorImage[CV[A] <: CoordVector[A]] extends ContinuousImage[CV, DenseVector[Double]] { self =>

  type Pixel = DenseVector[Double]

  def apply(point: CV[Double]): DenseVector[Double]

  def pixelDimensionality: Int

}

trait ContinuousScalarImage[CV[A] <: CoordVector[A]] extends ContinuousImage[CV, Double] { self: ContinuousImage[CV, Double] =>
  type CI = ContinuousScalarImage[CV]

  val pixelDimensionality = 1

  /** the function that defines the derivative, if it exists */
  val df: Option[CV[Double] => DenseVector[Double]]

  def differentiate: Option[ContinuousVectorImage[CV]] = {
    self.df.map(df =>
      new ContinuousVectorImage[CV] {
        val f = (x: CV[Double]) => df(x)
        def isDefinedAt(pt: CV[Double]) = self.isDefinedAt(pt)
        def pixelDimensionality = self.pixelDimensionality
      })
  }

  def +(that: CI): CI

  def -(that: CI): CI

  def :*(that: CI): CI

  def *(s: Double): CI

  def square: CI

  def compose(t: Transformation[CV]): CI
  def warp(t: Transformation[CV], imageDomainIndFunc: CV[Double] => Boolean): CI
  // def convolve(filter : CV => Double, imageDomainIndFunc: CV[Double] => Boolean /* later the integration method here */) : CI
}

/**
 * Implementation trait which can be used to specialize the functionality of ContinuousScalarImages for different subtypes
 */
trait ContinuousScalarImageLike[CV[A] <: CoordVector[A], Repr <: ContinuousScalarImage[CV]] { self: ContinuousScalarImage[CV] =>

  def newConcreteImageRepr(isDefinedAt: CV[Double] => Boolean, f: CV[Double] => Double, df: Option[CV[Double] => DenseVector[Double]]): Repr

  def +(that: CI): Repr = {
    def f(x: CV[Double]): Double = self.f(x) + that.f(x)
    def df = for (selfdf <- self.df; thatdf <- that.df) yield ((x: CV[Double]) => selfdf(x) + thatdf(x))
    val newDomain = (pt: CV[Double]) => self.isDefinedAt(pt) && that.isDefinedAt(pt)
    newConcreteImageRepr(newDomain, f, df)
  }

  def -(that: CI): Repr = {
    def f(x: CV[Double]): Double = self.f(x) - that.f(x)
    def df = for (seldf <- self.df; thatdf <- that.df) yield (x: CV[Double]) => seldf(x) - thatdf(x)
    val newDomain = (pt: CV[Double]) => self.isDefinedAt(pt) && that.isDefinedAt(pt)
    newConcreteImageRepr(newDomain, f, df)
  }

  def :*(that: CI): Repr = {
    def f(x: CV[Double]): Double = self.f(x) * that.f(x)
    def df = for (selfdf <- self.df; thatdf <- that.df) yield ((x: CV[Double]) => selfdf(x) * that(x) + thatdf(x) * self.f(x))
    val newDomain = (pt: CV[Double]) => self.isDefinedAt(pt) && that.isDefinedAt(pt)
    newConcreteImageRepr(newDomain, f, df)
  }

  def *(s: Double): Repr = {
    def f(x: CV[Double]): Double = self.f(x) * s
    val df = for (selfdf <- self.df) yield (x: CV[Double]) => selfdf(x) * s
    val newDomain = (pt: CV[Double]) => self.isDefinedAt(pt)
    newConcreteImageRepr(newDomain, f, df)
  }

  def square: Repr = {
    def f(x: CV[Double]): Double = self.f(x) * self.f(x)
    val df = for (selfdf <- self.df) yield (x: CV[Double]) => selfdf(x) * self.f(x) * 2.
    val newDomain = (pt: CV[Double]) => self.isDefinedAt(pt)
    newConcreteImageRepr(newDomain, f, df)
  }

  def compose(t: Transformation[CV]): Repr = {
    def f(x: CV[Double]) = self.f(t(x))
    val df = for (selfdf <- self.df) yield ((x: CV[Double]) => t.takeDerivative(x) * selfdf(t(x)))
    val newDomain = (pt: CV[Double]) => self.isDefinedAt(pt) && self.isDefinedAt(t(pt))

    newConcreteImageRepr(newDomain, f, df)
  }

  def warp(t: Transformation[CV], imageDomainIndFunc: CV[Double] => Boolean): Repr = {
    def f(x: CV[Double]) = self.f(t(x))
    def df(x: CV[Double]) = for (selfdf <- self.df) yield t.takeDerivative(x) * selfdf(t(x))
    val newDomain = (pt: CV[Double]) => imageDomainIndFunc(pt) && self.isDefinedAt(t(pt))

    newConcreteImageRepr(newDomain, f, self.df)
  }

}

case class ContinuousScalarImage1D(_isDefinedAt: Point1D => Boolean, val f: Point1D => Double, val df: Option[Point1D => DenseVector[Double]] = None) extends ContinuousScalarImage[CoordVector1D] with ContinuousScalarImageLike[CoordVector1D, ContinuousScalarImage1D] {

  override val pixelDimensionality = 1
  def newConcreteImageRepr(isDefinedAt: CoordVector1D[Double] => Boolean, f: CoordVector1D[Double] => Double, df: Option[CoordVector1D[Double] => DenseVector[Double]]): ContinuousScalarImage1D = ContinuousScalarImage1D(isDefinedAt, f, df)

  def isDefinedAt(pt: Point1D) = _isDefinedAt(pt)

  def convolve(filter: Point1D => Double, filterSupportRegion : BoxedRegion[CoordVector1D], integrator : Integrator[CoordVector1D]): ContinuousScalarImage1D = {

    def f(x: Point1D) = {

      def intermediateF(t: Point1D) = {
        val p = CoordVector1D(x(0) - t(0))
        this.f(p) * filter(t)
      }

      val intermediateContinuousImage = ContinuousScalarImage1D((t: Point1D) => true, intermediateF)
      integrator.integrateScalar(intermediateContinuousImage, filterSupportRegion)

    }
    def df(x: Point1D) = new Exception("Not implemented yet !")

    ContinuousScalarImage1D(this.isDefinedAt, f)
  }

}

case class ContinuousScalarImage2D(_isDefinedAt: CoordVector2D[Double] => Boolean, val f: Point2D => Double, val df: Option[Point2D => DenseVector[Double]] = None) extends ContinuousScalarImage[CoordVector2D] with ContinuousScalarImageLike[CoordVector2D, ContinuousScalarImage2D] {

  def newConcreteImageRepr(isDefinedAt: Point2D => Boolean, f: Point2D => Double, df: Option[Point2D => DenseVector[Double]]): ContinuousScalarImage2D = ContinuousScalarImage2D(isDefinedAt, f, df)

  override val pixelDimensionality = 1
  def isDefinedAt(pt: Point2D) = _isDefinedAt(pt)

  def convolve(filter: Point2D => Double, filterSupportRegion: BoxedRegion[CoordVector2D], integrator: Integrator[CoordVector2D] ): ContinuousScalarImage2D = {

    def f(x: Point2D) = {

      def intermediateF(t: Point2D) = {
        val p = CoordVector2D( x(0) - t(0) , x(1) - t(1))
        this.f(p) * filter(t)
      }

      val intermediateContinuousImage = ContinuousScalarImage2D((t: Point2D) => true, intermediateF)
      integrator.integrateScalar(intermediateContinuousImage, filterSupportRegion)

    }
    def df(x: Point1D) = new Exception("Not implemented yet !")

    ContinuousScalarImage2D(this.isDefinedAt, f)
  }

}

case class ContinuousScalarImage3D(_isDefinedAt: Point3D => Boolean, val f: Point3D => Double, val df: Option[Point3D => DenseVector[Double]] = None) extends ContinuousScalarImage[CoordVector3D] with ContinuousScalarImageLike[CoordVector3D, ContinuousScalarImage3D] {

  def newConcreteImageRepr(isDefinedAt: Point3D => Boolean, f: Point3D => Double, df: Option[Point3D => DenseVector[Double]]): ContinuousScalarImage3D = ContinuousScalarImage3D(isDefinedAt, f, df)

  override val pixelDimensionality = 1
  def isDefinedAt(pt: Point3D) = _isDefinedAt(pt)
}

/////////////////////////////////////////////
// Vector Images
/////////////////////////////////////////////

case class ContinousVectorImage1D(val _isDefinedAt: Point1D => Boolean, val pixelDimensionality: Int, val f: Point1D => DenseVector[Double], val df: Option[Point1D => DenseMatrix[Double]]) extends ContinuousVectorImage[CoordVector1D] {

  type CV[A] = CoordVector1D[A]
  def isDefinedAt(x: Point1D) = _isDefinedAt(x)
}
