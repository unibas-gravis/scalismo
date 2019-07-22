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
import scalismo.common.interpolation.{ BSplineImageInterpolator, DifferentiableFieldInterpolator, FieldInterpolator }
import scalismo.geometry._

import scala.reflect.ClassTag

/**
 * A scalar valued discrete image.
 *
 * @param domain The domain over which this image is defined
 * @param data The values for each grid points.
 * @tparam D  The dimensionality of the image
 * @tparam A The type of the pixel (needs to implement Scalar).
 */
class DiscreteScalarImage[D: NDSpace, A: Scalar: ClassTag](override val domain: DiscreteImageDomain[D], override val data: ScalarArray[A])
    extends DiscreteImage[D, A](domain, data) {

  require(domain.numberOfPoints == data.size)

  protected override def ndSpace = implicitly[NDSpace[D]]

  /** returns a new image whose whose pixel values have been mapped using the function f */
  def map[B: Scalar: ClassTag](f: A => B): DiscreteScalarImage[D, B] = {
    DiscreteScalarImage(domain, data.map(f))
  }

  /**
   * Interpolates the image with the given interpolator.
   *
   * @param interpolator The interpolator used to interpolate the image
   * @param dummy Used to distinguish the method from the generic one inherited from [[DiscreteField]]
   */
  def interpolate(interpolator: FieldInterpolator[D, DiscreteImageDomain[D], A])(implicit dummy: DummyImplicit): ScalarImage[D] = {
    val f = interpolator.interpolate(this)
    ScalarImage(f.domain, f.f andThen (Scalar[A].toFloat))
  }

  /**
   * Interpolates the image with the given interpolator.
   * Note, that in contrast to the method [[DiscreteField.interpolate]] this method returns a scalar image
   *
   * @param interpolator The interpolator used to interpolate the image
   * @param dummy Used to distinguish the method from the generic one inherited from [[DiscreteField]]
   */
  def interpolate(interpolator: DifferentiableFieldInterpolator[D, DiscreteImageDomain[D], A, EuclideanVector[D]])(implicit dummy: DummyImplicit): DifferentiableScalarImage[D] = {
    val f = interpolator.interpolate(this)
    DifferentiableScalarImage(f.domain, f.f andThen (Scalar[A].toFloat), f.df)
  }

  /** Returns a new ContinuousScalarImage by interpolating the given DiscreteScalarImage using b-spline interpolation of given order */
  @deprecated("please use interpolate(BSplineImageInterpolatorxD[A](order)) instead", "v0.18")
  def interpolate(order: Int)(implicit bsplineCreator: BSplineImageInterpolator.Create[D]): DifferentiableScalarImage[D] = {
    val interpolator = bsplineCreator.createBSplineInterpolator(order)
    interpolate(interpolator)
  }

  /** Returns a new DiscreteScalarImage which is obtained by resampling the given image on the points defined by the new domain */
  def resample(newDomain: DiscreteImageDomain[D], interpolationDegree: Int, outsideValue: Float)(implicit bsplineCreator: BSplineImageInterpolator.Create[D]): DiscreteScalarImage[D, A] = {

    val contImg = interpolate(bsplineCreator.createBSplineInterpolator(interpolationDegree))
    contImg.sample(newDomain, outsideValue)
  }

}

object DiscreteScalarImage {
  def apply[D: NDSpace, A: Scalar: ClassTag](domain: DiscreteImageDomain[D], data: ScalarArray[A]): DiscreteScalarImage[D, A] = {
    new DiscreteScalarImage(domain, data)
  }

  /** create a new DiscreteScalarImage, with all pixel values set to the given value */
  def apply[D: NDSpace, A: Scalar: ClassTag](domain: DiscreteImageDomain[D], v: => A): DiscreteScalarImage[D, A] = {
    DiscreteScalarImage(domain, ScalarArray(Array.fill(domain.numberOfPoints)(v)))
  }

  /** create a new DiscreteScalarImage, with all pixel values set to the given value */
  def apply[D: NDSpace, A: Scalar: ClassTag](domain: DiscreteImageDomain[D], f: Point[D] => A): DiscreteScalarImage[D, A] = {
    val data = domain.points.map(f)
    DiscreteScalarImage(domain, ScalarArray(data.toArray))
  }

  /** create a new DiscreteScalarImage with given domain and values */
  def apply[D: NDSpace, A: Scalar: ClassTag](domain: DiscreteImageDomain[D], values: Traversable[A]): DiscreteScalarImage[D, A] = {
    DiscreteScalarImage(domain, ScalarArray(values.toArray))
  }

}

object DiscreteScalarImage1D {
  def apply[A: Scalar: ClassTag](domain: DiscreteImageDomain[_1D], data: ScalarArray[A]): DiscreteScalarImage[_1D, A] = {
    new DiscreteScalarImage[_1D, A](domain, data)
  }

  def apply[A: Scalar: ClassTag](domain: DiscreteImageDomain[_1D], v: => A): DiscreteScalarImage[_1D, A] = {
    DiscreteScalarImage(domain, v)
  }

  def apply[A: Scalar: ClassTag](domain: DiscreteImageDomain[_1D], f: Point[_1D] => A): DiscreteScalarImage[_1D, A] = {
    DiscreteScalarImage(domain, f)
  }

  def apply[A: Scalar: ClassTag](domain: DiscreteImageDomain[_1D], values: Traversable[A]): DiscreteScalarImage[_1D, A] = {
    DiscreteScalarImage(domain, values)
  }

}

object DiscreteScalarImage2D {
  def apply[A: Scalar: ClassTag](domain: DiscreteImageDomain[_2D], data: ScalarArray[A]): DiscreteScalarImage[_2D, A] = {
    new DiscreteScalarImage[_2D, A](domain, data)
  }

  def apply[A: Scalar: ClassTag](domain: DiscreteImageDomain[_2D], v: => A): DiscreteScalarImage[_2D, A] = {
    DiscreteScalarImage(domain, v)
  }

  def apply[A: Scalar: ClassTag](domain: DiscreteImageDomain[_2D], f: Point[_2D] => A): DiscreteScalarImage[_2D, A] = {
    DiscreteScalarImage(domain, f)
  }

  def apply[A: Scalar: ClassTag](domain: DiscreteImageDomain[_2D], values: Traversable[A]): DiscreteScalarImage[_2D, A] = {
    DiscreteScalarImage(domain, values)
  }
}

object DiscreteScalarImage3D {
  def apply[A: Scalar: ClassTag](domain: DiscreteImageDomain[_3D], data: ScalarArray[A]): DiscreteScalarImage[_3D, A] = {
    new DiscreteScalarImage[_3D, A](domain, data)
  }

  def apply[A: Scalar: ClassTag](domain: DiscreteImageDomain[_3D], v: => A): DiscreteScalarImage[_3D, A] = {
    DiscreteScalarImage(domain, v)
  }

  def apply[A: Scalar: ClassTag](domain: DiscreteImageDomain[_3D], f: Point[_3D] => A): DiscreteScalarImage[_3D, A] = {
    DiscreteScalarImage(domain, f)
  }

  def apply[A: Scalar: ClassTag](domain: DiscreteImageDomain[_3D], values: Traversable[A]): DiscreteScalarImage[_3D, A] = {
    DiscreteScalarImage(domain, values)
  }
}

