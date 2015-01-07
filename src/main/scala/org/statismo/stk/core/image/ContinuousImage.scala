package org.statismo.stk.core
package image

import spire.math.Numeric

import scala.language.implicitConversions
import org.statismo.stk.core.common.Domain
import org.statismo.stk.core.geometry._
import org.statismo.stk.core.numerics.{UniformSampler, IntegratorConfiguration, Integrator}
import org.statismo.stk.core.registration.CanDifferentiate
import org.statismo.stk.core.registration.Transformation

import org.statismo.stk.core.common.BoxedDomain

import scala.reflect.ClassTag

/**
 * The generic interface for continuous images
 */
trait ContinuousImage[D <: Dim, @specialized(Short, Float) Pixel] extends Function1[Point[D], Pixel] { self =>

  /** the function that defines the image values */
  val f: Point[D] => Pixel
  def domain: Domain[D]
  def isDefinedAt(pt: Point[D]): Boolean = domain.isDefinedAt(pt)


  def apply(x: Point[D]): Pixel = {
    if (!isDefinedAt(x)) throw new Exception(s"Point $x is outside the domain")
    f(x)
  }

  def liftPixelValue: (Point[D] => Option[Pixel]) = { x =>
    if (isDefinedAt(x)) Some(f(x))
    else None
  }

}

object ContinuousImage {

  def lift[D <: Dim, Pixel](fl: Pixel => Pixel): ContinuousImage[D, Pixel] => ContinuousImage[D, Pixel] = {
    img: ContinuousImage[D, Pixel] =>
      new ContinuousImage[D, Pixel] {
        override def apply(x: Point[D]) = fl(img.apply(x))
        val f = img.f
        def domain = img.domain
      }
  }

}


class ContinuousScalarImage[D <: Dim] private (val domain: Domain[D], val f: Point[D] => Float, val df: Option[Point[D] => Vector[D]] = None) extends ContinuousImage[D, Float] { self: ContinuousImage[D, Float] =>

  def differentiate: Option[ContinuousVectorImage[D]] = {
    self.df.map(df =>
      ContinuousVectorImage[D](self.domain, (x: Point[D]) => df(x), None)
      )
  }


  def +(that: ContinuousScalarImage[D]): ContinuousScalarImage[D] = {
    def f(x: Point[D]): Float = self.f(x) + that.f(x)
    def df = for (selfdf <- self.df; thatdf <- that.df) yield ((x: Point[D]) => selfdf(x) + thatdf(x))
    new ContinuousScalarImage(Domain.intersection[D](self.domain,that.domain), f, df)
  }

  def -(that: ContinuousScalarImage[D]): ContinuousScalarImage[D] = {
    def f(x: Point[D]): Float = self.f(x) - that.f(x)
    def df = for (seldf <- self.df; thatdf <- that.df) yield (x: Point[D]) => seldf(x) - thatdf(x)
    val newDomain = Domain.intersection[D](self.domain, that.domain)
    new ContinuousScalarImage(newDomain, f, df)
  }

  def :*(that: ContinuousScalarImage[D]): ContinuousScalarImage[D] = {
    def f(x: Point[D]): Float = self.f(x) * that.f(x)
    def df = for (selfdf <- self.df; thatdf <- that.df) yield ((x: Point[D]) => selfdf(x) * that(x) + thatdf(x) * self.f(x))
    val newDomain = Domain.intersection[D](self.domain, that.domain)
    new ContinuousScalarImage(newDomain, f, df)
  }

  def *(s: Double): ContinuousScalarImage[D] = {
    def f(x: Point[D]): Float = self.f(x) * s.toFloat
    val df = for (selfdf <- self.df) yield (x: Point[D]) => selfdf(x) * s.toFloat
    val newDomain = self.domain
    new ContinuousScalarImage(newDomain, f, df)
  }

  def square: ContinuousScalarImage[D] = {
    def f(x: Point[D]): Float = self.f(x) * self.f(x)
    val df = for (selfdf <- self.df) yield (x: Point[D]) => selfdf(x) * self.f(x) * 2f
    val newDomain = self.domain
    ContinuousScalarImage(newDomain, f, df)
  }

  def compose(t: Transformation[D]): ContinuousScalarImage[D] = {
    def f(x: Point[D]) = self.f(t(x))
    val df = t match {
      case t1: CanDifferentiate[D] => self.df.map { selfdf =>
        (x: Point[D]) => t1.takeDerivative(x) * selfdf(t(x))
      }
      case _ => None
    }

    val newDomain = Domain.fromPredicate[D]((pt: Point[D]) => self.isDefinedAt(t(pt)))

    new ContinuousScalarImage(newDomain, f, df)
  }
}


object ContinuousScalarImage {

  def apply[D <: Dim](domain: Domain[D], f: Point[D] => Float, df: Option[Point[D] => Vector[D]] = None) = new ContinuousScalarImage[D](domain, f, df)


  def sample[D <: Dim : NDSpace , @specialized(Short, Float, Double) Pixel: Numeric: ClassTag](img: ContinuousScalarImage[D], domain: DiscreteImageDomain[D], outsideValue: Double): DiscreteScalarImage[D, Pixel] = {
    val numeric = implicitly[Numeric[Pixel]]
    val sampledValues = domain.points.toIndexedSeq.par.map((pt: Point[D]) => {
      if (img.isDefinedAt(pt)) numeric.fromDouble(img(pt))
      else numeric.fromDouble(outsideValue)
    })


    DiscreteScalarImage(domain, sampledValues.toArray)
  }



  def convolve[D <: Dim : NDSpace](img : ContinuousScalarImage[D], filter: Filter[D], numberOfPoints : Int): ContinuousScalarImage[D] = {

      def f(x: Point[D]) = {

        def intermediateF(t: Point[D]) = {
          val p = (x - t).toPoint
          img.liftPixelValue(p).getOrElse(0f) * filter(t)
        }

        val support = filter.support

        val integrator = Integrator[D](IntegratorConfiguration(UniformSampler(support, numberOfPoints)))

        val intermediateContinuousImage = ContinuousScalarImage(filter.support, intermediateF)
        integrator.integrateScalar(intermediateContinuousImage)

      }

      def convolvedImgDerivative: Option[Point[D] => Vector[D]] = {
        if (img.df.isDefined)
          Some((x: Point[D]) => {
            val df = img.df.get
            def intermediateDF(t: Point[D]) : Vector[D] = {
              val p = (x - t).toPoint

              if (img.isDefinedAt(p))
                df(p) * filter(t)
              else Vector.zeros[D]

            }
            val support = filter.support
            val integrator = Integrator[D](IntegratorConfiguration(UniformSampler(support, numberOfPoints)))

            val intermediateContinuousImage = ContinuousVectorImage(filter.support, intermediateDF, None)
            integrator.integrateVector(intermediateContinuousImage)
          })

        else None

      }

      ContinuousScalarImage(img.domain, f, convolvedImgDerivative)
  }

}


case class ContinuousVectorImage[D <: Dim](domain: Domain[D], f: Point[D] => Vector[D], df: Option[Point[D] => SquareMatrix[D]]) extends ContinuousImage[D, Vector[D]]



