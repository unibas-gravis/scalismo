///*
// * Copyright 2015 University of Basel, Graphics and Vision Research Group
// *
// * Licensed under the Apache License, Version 2.0 (the "License");
// * you may not use this file except in compliance with the License.
// * You may obtain a copy of the License at
// *
// *     http://www.apache.org/licenses/LICENSE-2.0
// *
// * Unless required by applicable law or agreed to in writing, software
// * distributed under the License is distributed on an "AS IS" BASIS,
// * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// * See the License for the specific language governing permissions and
// * limitations under the License.
// */
//
//package scalismo.image
//
//import scalismo.common._
//import scalismo.common.interpolation.{BSplineImageInterpolator, DifferentiableFieldInterpolator, FieldInterpolator}
//import scalismo.geometry._
//
//import scala.reflect.ClassTag
//
//
///**
// * A scalar valued discrete image.
// *
// * @param domain The domain over which this image is defined
// * @param data The values for each grid points.
// * @tparam D  The dimensionality of the image
// * @tparam A The type of the pixel (needs to implement Scalar).
// */
//class DiscreteImage[D: NDSpace, A: Scalar: ClassTag](override val domain: DiscreteImageDomain[D],
//                                                           override val data: ScalarArray[A])
//    extends DiscreteImage[D, A](domain, data) {
//
//  require(domain.pointSet.numberOfPoints == data.size)
//
//  override protected def ndSpace = implicitly[NDSpace[D]]
//
//  /** returns a new image whose whose pixel values have been mapped using the function f */
//  def map[B: Scalar: ClassTag](f: A => B): DiscreteImage[D, B] = {
//    DiscreteImage(domain, data.map(f))
//  }
//
//  /**
//   * Interpolates the image with the given interpolator.
//   *
//   * @param interpolator The interpolator used to interpolate the image
//   */
//  override def interpolate(interpolator: FieldInterpolator[D, DiscreteImageDomain, A]): ScalarImage[D, A] = {
//    val f = interpolator.interpolate(this)
//    ScalarImage(f.domain, f.f)
//  }
//
//  /**
//   * Interpolates the image with the given interpolator.
//   * Note, that in contrast to the method [[DiscreteField.interpolate]] this method returns a scalar image
//   *
//   * @param interpolator The interpolator used to interpolate the image
//   */
//  def interpolate(
//    interpolator: DifferentiableFieldInterpolator[D, DiscreteImageDomain, A, EuclideanVector[D]]
//  ): DifferentiableScalarImage[D, A] = {
//    val f = interpolator.interpolate(this)
//    DifferentiableScalarImage(f.domain, f.f, f.df)
//  }
//
//  /** Returns a new ContinuousScalarImage by interpolating the given DiscreteImage using b-spline interpolation of given order */
//  def resample(newDomain: DiscreteImageDomain[D],
//               interpolator: FieldInterpolator[D, DiscreteImageDomain, A],
//               outsideValue: A): DiscreteImage[D, A] = {
//
//    val contImg = interpolate(interpolator)
//    contImg.sample(newDomain, outsideValue)
//  }
//}
//
//object DiscreteImage {
//  def apply[D: NDSpace, A: Scalar: ClassTag](domain: DiscreteImageDomain[D],
//                                             data: ScalarArray[A]): DiscreteImage[D, A] = {
//    new DiscreteImage(domain, data)
//  }
//
//  /** create a new DiscreteImage, with all pixel values set to the given value */
//  def apply[D: NDSpace, A: Scalar: ClassTag](domain: DiscreteImageDomain[D], v: => A): DiscreteImage[D, A] = {
//    DiscreteImage(domain, ScalarArray(Array.fill(domain.pointSet.numberOfPoints)(v)))
//  }
//
//  /** create a new DiscreteImage, with all pixel values set to the given value */
//  def apply[D: NDSpace, A: Scalar: ClassTag](domain: DiscreteImageDomain[D],
//                                             f: Point[D] => A): DiscreteImage[D, A] = {
//    val data = domain.pointSet.points.map(f)
//    DiscreteImage(domain, ScalarArray(data.toArray))
//  }
//
//  /** create a new DiscreteImage with given domain and values */
//  def apply[D: NDSpace, A: Scalar: ClassTag](domain: DiscreteImageDomain[D],
//                                             values: Traversable[A]): DiscreteImage[D, A] = {
//    DiscreteImage(domain, ScalarArray(values.toArray))
//  }
//
//}
//
//object DiscreteScalarImage1D {
//  def apply[A: Scalar: ClassTag](domain: DiscreteImageDomain[_1D],
//                                 data: ScalarArray[A]): DiscreteImage[_1D, A] = {
//    new DiscreteImage[_1D, A](domain, data)
//  }
//
//  def apply[A: Scalar: ClassTag](domain: DiscreteImageDomain[_1D], v: => A): DiscreteImage[_1D, A] = {
//    DiscreteImage(domain, v)
//  }
//
//  def apply[A: Scalar: ClassTag](domain: DiscreteImageDomain[_1D], f: Point[_1D] => A): DiscreteImage[_1D, A] = {
//    DiscreteImage(domain, f)
//  }
//
//  def apply[A: Scalar: ClassTag](domain: DiscreteImageDomain[_1D],
//                                 values: Traversable[A]): DiscreteImage[_1D, A] = {
//    DiscreteImage(domain, values)
//  }
//
//}
//
//object DiscreteScalarImage2D {
//  def apply[A: Scalar: ClassTag](domain: DiscreteImageDomain[_2D],
//                                 data: ScalarArray[A]): DiscreteImage[_2D, A] = {
//    new DiscreteImage[_2D, A](domain, data)
//  }
//
//  def apply[A: Scalar: ClassTag](domain: DiscreteImageDomain[_2D], v: => A): DiscreteImage[_2D, A] = {
//    DiscreteImage(domain, v)
//  }
//
//  def apply[A: Scalar: ClassTag](domain: DiscreteImageDomain[_2D], f: Point[_2D] => A): DiscreteImage[_2D, A] = {
//    DiscreteImage(domain, f)
//  }
//
//  def apply[A: Scalar: ClassTag](domain: DiscreteImageDomain[_2D],
//                                 values: Traversable[A]): DiscreteImage[_2D, A] = {
//    DiscreteImage(domain, values)
//  }
//}
//
//object DiscreteScalarImage3D {
//  def apply[A: Scalar: ClassTag](domain: DiscreteImageDomain[_3D],
//                                 data: ScalarArray[A]): DiscreteImage[_3D, A] = {
//    new DiscreteImage[_3D, A](domain, data)
//  }
//
//  def apply[A: Scalar: ClassTag](domain: DiscreteImageDomain[_3D], v: => A): DiscreteImage[_3D, A] = {
//    DiscreteImage(domain, v)
//  }
//
//  def apply[A: Scalar: ClassTag](domain: DiscreteImageDomain[_3D], f: Point[_3D] => A): DiscreteImage[_3D, A] = {
//    DiscreteImage(domain, f)
//  }
//
//  def apply[A: Scalar: ClassTag](domain: DiscreteImageDomain[_3D],
//                                 values: Traversable[A]): DiscreteImage[_3D, A] = {
//    DiscreteImage(domain, values)
//  }
//}
