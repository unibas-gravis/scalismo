package org.statismo.stk.core
package image

import org.statismo.stk.core.image.filter.Filter
import spire.math.Numeric

import scala.language.implicitConversions
import org.statismo.stk.core.common.Domain
import org.statismo.stk.core.geometry._
import org.statismo.stk.core.numerics.{UniformSampler, IntegratorConfiguration, Integrator}
import org.statismo.stk.core.registration.CanDifferentiate
import org.statismo.stk.core.registration.Transformation
import scala.reflect.ClassTag

/**
 * The generic interface for continuous images
 */
trait Image[D <: Dim, A] extends Function1[Point[D], A] { self =>

  /** the function that defines the image values */
  val f: Point[D] => A
  def domain: Domain[D]
  def isDefinedAt(pt: Point[D]): Boolean = domain.isDefinedAt(pt)


  def apply(x: Point[D]): A = {
    if (!isDefinedAt(x)) throw new Exception(s"Point $x is outside the domain")
    f(x)
  }

  def liftValues: (Point[D] => Option[A]) = { x =>
    if (isDefinedAt(x)) Some(f(x))
    else None
  }

}

object Image {

  def lift[D <: Dim, A](fl: A => A): Image[D, A] => Image[D, A] = {
    img: Image[D, A] =>
      new Image[D, A] {
        override def apply(x: Point[D]) = fl(img.apply(x))
        val f = img.f
        def domain = img.domain
      }
  }

}


class ScalarImage[D <: Dim] protected (val domain: Domain[D], val f: Point[D] => Float) extends Image[D, Float] {


  def +(that: ScalarImage[D]): ScalarImage[D] = {
    def f(x: Point[D]): Float = this.f(x) + that.f(x)
    new ScalarImage(Domain.intersection[D](domain,that.domain), f)
  }

  def -(that: ScalarImage[D]): ScalarImage[D] = {
    def f(x: Point[D]): Float = this.f(x) - that.f(x)
    val newDomain = Domain.intersection[D](domain, that.domain)
    new ScalarImage(newDomain, f)
  }

  def :*(that: ScalarImage[D]): ScalarImage[D] = {
    def f(x: Point[D]): Float = this.f(x) * that.f(x)
    val newDomain = Domain.intersection[D](domain, that.domain)
    new ScalarImage(newDomain, f)
  }

  def *(s: Double): ScalarImage[D] = {
    def f(x: Point[D]): Float = this.f(x) * s.toFloat
    val newDomain = domain
    new ScalarImage(newDomain, f)
  }

  def square: ScalarImage[D] = {
    def f(x: Point[D]): Float = this.f(x) * this.f(x)
    val newDomain = domain
    ScalarImage(newDomain, f)
  }

  def compose(t: Transformation[D]): ScalarImage[D] = {
    def f(x: Point[D]) = this.f(t(x))

    val newDomain = Domain.fromPredicate[D]((pt: Point[D]) => isDefinedAt(t(pt)))
    new ScalarImage(newDomain, f)
  }

  def andThen(g: Float => Float): ScalarImage[D] = {
    new ScalarImage(domain, f andThen g)
  }
}


class DifferentiableScalarImage[D <: Dim] (_domain: Domain[D], _f: Point[D] => Float, val df : Point[D] => Vector[D]) extends ScalarImage[D](_domain, _f) {

  def differentiate : VectorImage[D] = VectorImage(domain, df)

  def +(that: DifferentiableScalarImage[D]): DifferentiableScalarImage[D] = {
    def f(x: Point[D]): Float = this.f(x) + that.f(x)
    def df = (x: Point[D]) => this.df(x) + that.df(x)
    new DifferentiableScalarImage(Domain.intersection[D](domain,that.domain), f, df)
  }

  def -(that: DifferentiableScalarImage[D]): ScalarImage[D] = {
    def f(x: Point[D]): Float = this.f(x) - that.f(x)
    def df = (x: Point[D]) => this.df(x) - that.df(x)
    val newDomain = Domain.intersection[D](domain, that.domain)
    new DifferentiableScalarImage(newDomain, f, df)
  }

  def :*(that: DifferentiableScalarImage[D]): ScalarImage[D] = {
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

  override def square: DifferentiableScalarImage[D] = {
    def f(x: Point[D]): Float = this.f(x) * this.f(x)
    val df = (x: Point[D]) => this.df(x) * this.f(x) * 2f
    val newDomain = domain
    new DifferentiableScalarImage(newDomain, f, df)
  }

  def compose(t: Transformation[D] with CanDifferentiate[D]): DifferentiableScalarImage[D] = {
    def f(x: Point[D]) = this.f(t(x))
    val newDomain = Domain.fromPredicate[D]((pt: Point[D]) => this.isDefinedAt(t(pt)))
    val df = (x: Point[D]) => t.takeDerivative(x) * this.df(t(x))

    new DifferentiableScalarImage(newDomain, f, df)
  }
}


object ScalarImage {

  def apply[D <: Dim](domain: Domain[D], f: Point[D] => Float) = new ScalarImage[D](domain, f)

  def sample[D <: Dim : NDSpace , Pixel: Numeric: ClassTag](img: ScalarImage[D], domain: DiscreteImageDomain[D], outsideValue: Double): DiscreteScalarImage[D, Pixel] = {
    val numeric = implicitly[Numeric[Pixel]]

    val sampledValues = domain.points.toIndexedSeq.par.map((pt: Point[D]) => {
      if (img.isDefinedAt(pt)) numeric.fromDouble(img(pt))
      else numeric.fromDouble(outsideValue)
    })

    DiscreteScalarImage(domain, sampledValues.toArray)
  }


  def convolve[D <: Dim : NDSpace](img : ScalarImage[D], filter: Filter[D], numberOfPoints : Int): ScalarImage[D] = {

      def f(x: Point[D]) = {

        def intermediateF(t: Point[D]) = {
          val p = (x - t).toPoint
          img.liftValues(p).getOrElse(0f) * filter(t)
        }

        val support = filter.support

        val integrator = Integrator[D](IntegratorConfiguration(UniformSampler(support, numberOfPoints)))

        val intermediateContinuousImage = ScalarImage(filter.support, intermediateF)
        integrator.integrateScalar(intermediateContinuousImage)

      }

      ScalarImage(img.domain, f)
  }

}


object DifferentiableScalarImage {

  def apply[D <: Dim](domain: Domain[D], f: Point[D] => Float, df: Point[D] => Vector[D]) = new DifferentiableScalarImage[D](domain, f, df)

  def convolve[D <: Dim : NDSpace](img : DifferentiableScalarImage[D], filter: Filter[D], numberOfPoints : Int): ScalarImage[D] = {

      val convolvedImage = ScalarImage.convolve(img, filter, numberOfPoints)

      def convolvedImgDerivative: Point[D] => Vector[D] = {
        (x: Point[D]) => {
          val df = img.df
          def intermediateDF(t: Point[D]): Vector[D] = {
            val p = (x - t).toPoint

            if (img.isDefinedAt(p))
              df(p) * filter(t)
            else Vector.zeros[D]

          }
          val support = filter.support
          val integrator = Integrator[D](IntegratorConfiguration(UniformSampler(support, numberOfPoints)))

          val intermediateContinuousImage = VectorImage(filter.support, intermediateDF)
          integrator.integrateVector(intermediateContinuousImage)
        }
      }

      new DifferentiableScalarImage(img.domain, convolvedImage.f, convolvedImgDerivative)
    }

}



case class VectorImage[D <: Dim](domain: Domain[D], f: Point[D] => Vector[D]) extends Image[D, Vector[D]]


