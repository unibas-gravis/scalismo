/*
 * Copyright 2015 University of Basel, Graphics and Vision Research Group
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package scalismo.image

import scalismo.common._
import scalismo.geometry._
import scalismo.image.filter.Filter
import scalismo.numerics.{GridSampler, Integrator}
import scalismo.registration.{CanDifferentiate, Transformation}

import scala.reflect.ClassTag

/**
 * An image whose values are scalar.
 */
class ScalarImage[D: NDSpace, A: Scalar: ClassTag] protected (override val domain: Domain[D],
                                                              override val f: Point[D] => A)
    extends ScalarField[D, A](domain, f) {

  /** adds two images. The domain of the new image is the intersection of both */
  def +(that: ScalarImage[D, A]): ScalarImage[D, A] = {
    val field = super.+(that)
    new ScalarImage(field.domain, field.f)
  }

  /** subtract two images. The domain of the new image is the intersection of the domains of the individual images*/
  def -(that: ScalarImage[D, A]): ScalarImage[D, A] = {
    val field = super.-(that)
    new ScalarImage(field.domain, field.f)
  }

  /** element wise multiplication. The domain of the new image is the intersection of the domains of the individual images*/
  def :*(that: ScalarImage[D, A]): ScalarImage[D, A] = {
    val field = super.:*(that)
    new ScalarImage(field.domain, field.f)
  }

  /** scalar multiplication of an image */
  override def *(s: Double): ScalarImage[D, Double] = {
    val field = super.*(s)
    new ScalarImage(field.domain, field.f)
  }

  /** composes (i.e. warp) an image with a transformation. */
  override def compose(t: Point[D] => Point[D]): ScalarImage[D, A] = {
    val field = super.compose(t)
    new ScalarImage(field.domain, field.f)
  }

  /** applies the given function to the image values */
  def andThen[B: Scalar: ClassTag](g: A => B): ScalarImage[D, B] = {
    new ScalarImage(domain, f andThen g)
  }

  /**
   * Convolution of an image with a given filter. The convolution is carried out by
   * numerical integration, using the given number of points elevated to the power of dimensionality as an approximation.
   *
   * @param filter Filter to be used in the convolution.
   * @param  numberOfPointsPerDim Number of points to be used to approximate the filter. Depending on the
   * support size of the filter and the Frequency of the image, increasing this value can help avoid artifacts (at the cost of heavier computation)
   */
  def convolve(filter: Filter[D],
               numberOfPointsPerDim: Int)(implicit c: CreateDiscreteImageDomain[D]): ScalarImage[D, A] = {
    val scalar = Scalar[A]

    val dim = implicitly[NDSpace[D]].dimensionality
    val supportSpacing = filter.support.extent * (1f / numberOfPointsPerDim.toFloat)
    val supportSize = IntVector[D]((0 until dim).map(_ => numberOfPointsPerDim).toArray)
    val origin = (supportSpacing * ((numberOfPointsPerDim - 1) * -0.5f)).toPoint

    val support = DiscreteImageDomain[D](origin, supportSpacing, supportSize)
    val lifted = liftValues

    val integrator = Integrator[D](GridSampler(support))

    def intermediateF(imageX: Point[D])(t: Point[D]): Option[Float] = {

      val p = (imageX - t).toPoint
      lifted(p).map(v => scalar.toFloat(v) * filter(t))
    }

    def f(imageX: Point[D]): A = {
      scalar.fromFloat(integrator.integrateScalar(intermediateF(imageX) _))
    }

    ScalarImage(domain, f)
  }

  /**
   * Returns a discrete scalar image with the given domain, whose values are obtained by sampling the scalarImage at the domain points.
   * If the image is not defined at a domain point, the outside value is used.
   */
  def sample(domain: DiscreteImageDomain[D], outsideValue: A): DiscreteScalarImage[D, A] = {

    val nbChunks = Runtime.getRuntime().availableProcessors() * 2
    val parallelArrays = domain.pointSet.pointsInChunks(nbChunks).par.map { chunkIterator =>
      chunkIterator
        .map(pt => {
          if (isDefinedAt(pt)) f(pt)
          else outsideValue
        })
        .toArray
    }

    DiscreteScalarImage(domain, ScalarArray(parallelArrays.reduce(_ ++ _)))
  }

}

/**
 * Factory methods for creating scalar images
 */
object ScalarImage {

  /**
   *  Creates a new scalar image with given domain and values
   *
   * @param domain The domain over which the image is defined
   * @param f A function which yields for each point of the domain its value
   */
  def apply[D: NDSpace, A: Scalar: ClassTag](domain: Domain[D], f: Point[D] => A) = new ScalarImage[D, A](domain, f)

}

/**
 * A scalar image that is once differentiable
 */
class DifferentiableScalarImage[D: NDSpace, A: Scalar: ClassTag](_domain: Domain[D],
                                                                 _f: Point[D] => A,
                                                                 val df: Point[D] => EuclideanVector[D])
    extends ScalarImage[D, A](_domain, _f) {

  def differentiate: VectorField[D, D] = VectorField(domain, df)

  def +(that: DifferentiableScalarImage[D, A]): DifferentiableScalarImage[D, A] = {
    val addedField = super.+(that)
    def df = (x: Point[D]) => this.df(x) + that.df(x)
    new DifferentiableScalarImage(addedField.domain, addedField.f, df)
  }

  def -(that: DifferentiableScalarImage[D, A]): DifferentiableScalarImage[D, A] = {
    val subtractedField = super.-(that)
    def df = (x: Point[D]) => this.df(x) - that.df(x)
    new DifferentiableScalarImage(subtractedField.domain, subtractedField.f, df)
  }

  def :*(that: DifferentiableScalarImage[D, A]): DifferentiableScalarImage[D, A] = {
    val scalar = Scalar[A]
    val mutipliedField = super.:*(that)
    def df = (x: Point[D]) => this.df(x) * scalar.toDouble(that(x)) + that.df(x) * scalar.toDouble(this.f(x))
    new DifferentiableScalarImage(mutipliedField.domain, mutipliedField.f, df)
  }

  override def *(s: Double): DifferentiableScalarImage[D, Double] = {
    val multipliedField = super.*(s)
    val df = (x: Point[D]) => this.df(x) * s.toFloat
    new DifferentiableScalarImage(multipliedField.domain, multipliedField.f, df)
  }

  def compose(t: Transformation[D] with CanDifferentiate[D]): DifferentiableScalarImage[D, A] = {
    val composedField = super.compose(t)
    val df = (x: Point[D]) => t.takeDerivative(x) * this.df(t(x))

    new DifferentiableScalarImage(composedField.domain, composedField.f, df)
  }

  override def convolve(filter: Filter[D], numberOfPointsPerDim: Int)(
    implicit
    c: CreateDiscreteImageDomain[D]
  ): DifferentiableScalarImage[D, A] = {

    val convolvedImage = super.convolve(filter, numberOfPointsPerDim)

    val dim = implicitly[NDSpace[D]].dimensionality
    val supportSpacing = filter.support.extent * (1f / numberOfPointsPerDim.toFloat)
    val supportSize = IntVector[D]((0 until dim).map(_ => numberOfPointsPerDim).toArray)
    val origin = (supportSpacing * ((numberOfPointsPerDim - 1) * -0.5f)).toPoint
    val support = DiscreteImageDomain[D](origin, supportSpacing, supportSize)

    val integrator = Integrator[D](GridSampler(support))

    def intermediateDF(imageX: Point[D])(t: Point[D]): Option[EuclideanVector[D]] = {
      val p = (imageX - t).toPoint
      if (this.isDefinedAt(p)) Some(df(p) * filter(t)) else None
    }

    def convolvedImgDerivative(imageX: Point[D]): EuclideanVector[D] = {
      integrator.integrateVector(intermediateDF(imageX) _)
    }

    new DifferentiableScalarImage(domain, convolvedImage.f, convolvedImgDerivative)
  }

}

/**
 * Factory methods to create a differentiableScalarImage
 */
object DifferentiableScalarImage {

  /**
   * creates a new differentiable image.
   *
   * @param domain the domain of the image
   * @param f a function that yields the intensity for each point of the domain
   * @param df the derivative of the function f
   */
  def apply[D: NDSpace, A: Scalar: ClassTag](domain: Domain[D], f: Point[D] => A, df: Point[D] => EuclideanVector[D]) =
    new DifferentiableScalarImage[D, A](domain, f, df)

}
