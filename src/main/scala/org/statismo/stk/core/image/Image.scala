package org.statismo.stk.core
package image

//import org.statismo.stk.core.image.DiscreteScalarImage.{CanInterpolate}
import org.statismo.stk.core.image.filter.Filter
import spire.math.Numeric

import scala.language.implicitConversions
import org.statismo.stk.core.common.{RealSpace, Domain}
import org.statismo.stk.core.geometry._
import org.statismo.stk.core.numerics.{UniformSampler, IntegratorConfiguration, Integrator}
import org.statismo.stk.core.registration.CanDifferentiate
import org.statismo.stk.core.registration.Transformation
import scala.reflect.ClassTag

/**
 * An image is simply a function from points to values, together with a domain on which the
 * function is defined.
 */
trait Image[D <: Dim, A] extends Function1[Point[D], A] { self =>

  /** a function that defines the image values. It must be defined on the full domain */
  protected[Image] val f: Point[D] => A

  /** The domain on which the image is defined */
  def domain: Domain[D]

  /** True if the image is defined at the given point */
  def isDefinedAt(pt: Point[D]): Boolean = domain.isDefinedAt(pt)

  /** The value of the image at a given point.
   * if an image is accessed outside of its definition, an exception is thrown */
  override def apply(x: Point[D]): A = {
    if (!isDefinedAt(x)) throw new Exception(s"Point $x is outside the domain")
    f(x)
  }

  /**
   * Lifts the definition of the value function such that it is defined everywhere,
   * but yields none if the value is outside of the domain
   */
  def liftValues: (Point[D] => Option[A]) = new Image[D, Option[A]] {
    override val f = { (x : Point[D]) =>
      if (self.isDefinedAt(x)) Some(self.f(x))
      else None
    }
    override def domain = RealSpace[D]
  }

}

/**
 * Utility functions to create and manipulate images
 */
object Image {

  /**
   * Lifts a function between pixel values such that it acts on image intensities.
   * This is useful to write functions that manipulate the image intensities.
   */
  def lift[D <: Dim, A](fl: A => A): Image[D, A] => Image[D, A] = {
    img: Image[D, A] =>
      new Image[D, A] {
        override def apply(x: Point[D]) = fl(img.apply(x))
        override val f = img.f
        def domain = img.domain
      }
  }

}


/**
  * An image whose values are scalar.
  */
class ScalarImage[D <: Dim : NDSpace] protected (val domain: Domain[D], val f: Point[D] => Float) extends Image[D, Float] {

  /** adds two images. The domain of the new image is the intersection of both */
  def +(that: ScalarImage[D]): ScalarImage[D] = {
    def f(x: Point[D]): Float = this.f(x) + that.f(x)
    new ScalarImage(Domain.intersection[D](domain,that.domain), f)
  }

  /** subtract two images. The domain of the new image is the intersection of the domains of the individual images*/
  def -(that: ScalarImage[D]): ScalarImage[D] = {
    def f(x: Point[D]): Float = this.f(x) - that.f(x)
    val newDomain = Domain.intersection[D](domain, that.domain)
    new ScalarImage(newDomain, f)
  }


  /** element wise multiplcation. The domain of the new image is the intersection of the domains of the individual images*/
  def :*(that: ScalarImage[D]): ScalarImage[D] = {
    def f(x: Point[D]): Float = this.f(x) * that.f(x)
    val newDomain = Domain.intersection[D](domain, that.domain)
    new ScalarImage(newDomain, f)
  }

  /** scalar multiplication of an image */
  def *(s: Double): ScalarImage[D] = {
    def f(x: Point[D]): Float = this.f(x) * s.toFloat
    val newDomain = domain
    new ScalarImage(newDomain, f)
  }

  /** composes (i.e. warp) an image with a transformation. */
  def compose(t: Transformation[D]): ScalarImage[D] = {
    def f(x: Point[D]) = this.f(t(x))

    val newDomain = Domain.fromPredicate[D]((pt: Point[D]) => isDefinedAt(t(pt)))
    new ScalarImage(newDomain, f)
  }

  /** applies the given function to the image values */
  def andThen(g: Float => Float): ScalarImage[D] = {
    new ScalarImage(domain, f andThen g)
  }

  /** convolution of an image with a given filter. The convolution is carried out by
    * numerical integration, using the given number of poitns as an approximation.
    */
  def convolve(filter: Filter[D], numberOfPoints : Int): ScalarImage[D] = {

    def f(x: Point[D]) = {

      def intermediateF(t: Point[D]) = {
        val p = (x - t).toPoint
        liftValues(p).getOrElse(0f) * filter(t)
      }

      val support = filter.support

      val integrator = Integrator[D](IntegratorConfiguration(UniformSampler(support, numberOfPoints)))

      val intermediateContinuousImage = ScalarImage(filter.support, intermediateF)
      integrator.integrateScalar(intermediateContinuousImage)

    }

    ScalarImage(domain, f)
  }


  /**
   * Returns a discrete scalar image with the given domain, whose values are obtained by sampling the scalarImge at the domain points.
   * If the image is not defined at a domain point, the outside value is used.
   */
  def sample[Pixel: Numeric: ClassTag](domain: DiscreteImageDomain[D], outsideValue: Double): DiscreteScalarImage[D, Pixel] = {
    val numeric = implicitly[Numeric[Pixel]]

    val sampledValues = domain.points.toIndexedSeq.par.map((pt: Point[D]) => {
      if (isDefinedAt(pt)) numeric.fromDouble(this(pt))
      else numeric.fromDouble(outsideValue)
    })

    DiscreteScalarImage(domain, sampledValues.toArray)
  }

}

/**
 * Factory methods for createing scalar images
 */
object ScalarImage {

  /**
   *  Creates a new scalar image with given domain and values
   *
   * @param domain The domain over which the image is defined
   * @param f A function which yields for each point of the domain its value
   */
  def apply[D <: Dim : NDSpace](domain: Domain[D], f: Point[D] => Float) = new ScalarImage[D](domain, f)

}


/**
 * A scalar image that is once differentiable
 */
class DifferentiableScalarImage[D <: Dim : NDSpace] (_domain: Domain[D], _f: Point[D] => Float, val df : Point[D] => Vector[D]) extends ScalarImage[D](_domain, _f) {

  def differentiate : VectorImage[D] = VectorImage(domain, df)

  def +(that: DifferentiableScalarImage[D]): DifferentiableScalarImage[D] = {
    def f(x: Point[D]): Float = this.f(x) + that.f(x)
    def df = (x: Point[D]) => this.df(x) + that.df(x)
    new DifferentiableScalarImage(Domain.intersection[D](domain,that.domain), f, df)
  }

  def -(that: DifferentiableScalarImage[D]): DifferentiableScalarImage[D] = {
    def f(x: Point[D]): Float = this.f(x) - that.f(x)
    def df = (x: Point[D]) => this.df(x) - that.df(x)
    val newDomain = Domain.intersection[D](domain, that.domain)
    new DifferentiableScalarImage(newDomain, f, df)
  }

  def :*(that: DifferentiableScalarImage[D]): DifferentiableScalarImage[D] = {
    def f(x: Point[D]): Float = this.f(x) * that.f(x)
    def df = (x: Point[D]) => this.df(x) * that(x) + that.df(x) * this.f(x)
    val newDomain = Domain.intersection[D](this.domain, that.domain)
    new DifferentiableScalarImage(newDomain, f, df)
  }

  override def *(s: Double): DifferentiableScalarImage[D] = {
    def f(x: Point[D]): Float = this.f(x) * s.toFloat
    val df = (x: Point[D]) => this.df(x) * s.toFloat
    val newDomain = domain
    new DifferentiableScalarImage(newDomain, f, df)
  }


  def compose(t: Transformation[D] with CanDifferentiate[D]): DifferentiableScalarImage[D] = {
    def f(x: Point[D]) = this.f(t(x))
    val newDomain = Domain.fromPredicate[D]((pt: Point[D]) => this.isDefinedAt(t(pt)))
    val df = (x: Point[D]) => t.takeDerivative(x) * this.df(t(x))

    new DifferentiableScalarImage(newDomain, f, df)
  }

  override def convolve(filter: Filter[D], numberOfPoints : Int): DifferentiableScalarImage[D] = {

    val convolvedImage = super.convolve(filter, numberOfPoints)

    def convolvedImgDerivative: Point[D] => Vector[D] = {
      (x: Point[D]) => {
        val df = this.df
        def intermediateDF(t: Point[D]): Vector[D] = {
          val p = (x - t).toPoint

          if (this.isDefinedAt(p))
            df(p) * filter(t)
          else Vector.zeros[D]

        }
        val support = filter.support
        val integrator = Integrator[D](IntegratorConfiguration(UniformSampler(support, numberOfPoints)))

        val intermediateContinuousImage = VectorImage(filter.support, intermediateDF)
        integrator.integrateVector(intermediateContinuousImage)
      }
    }

    new DifferentiableScalarImage(domain, convolvedImage.f, convolvedImgDerivative)
  }


}

/**
 * Factory methods to create a differentiableScalarImage
 */
object DifferentiableScalarImage {

  /** creates a new differentiable image.
    *
    * @param domain the domain of the image
    * @param f a function that yiels for each point of the domain its intensities
    * @param df the derivative of the function f
    */
  def apply[D <: Dim : NDSpace](domain: Domain[D], f: Point[D] => Float, df: Point[D] => Vector[D]) = new DifferentiableScalarImage[D](domain, f, df)

}


/**
 * An vector valued image.
 */
case class VectorImage[D <: Dim](domain: Domain[D], f: Point[D] => Vector[D]) extends Image[D, Vector[D]]


