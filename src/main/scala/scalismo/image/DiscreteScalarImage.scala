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

import breeze.linalg.DenseVector
import scalismo.common._
import scalismo.geometry._
import scalismo.image.DiscreteScalarImage.Create
import scalismo.numerics.BSpline

import scala.reflect.ClassTag

/**
 * A scalar valued discrete image.
 *
 * @param domain The domain over which this image is defined
 * @param data The values for each grid points.
 * @tparam D  The dimensionality of the image
 * @tparam A The type of the pixel (needs to implement Scalar).
 */
abstract class DiscreteScalarImage[D <: Dim: NDSpace: Create, A: Scalar: ClassTag] protected (override val domain: DiscreteImageDomain[D], data: ScalarArray[A])
    extends DiscreteScalarField[D, A](domain, data) with DiscreteImage[D, A] {

  require(domain.numberOfPoints == data.size)

  protected override def ndSpace = implicitly[NDSpace[D]]

  /** returns a new image whose whose pixel values have been mapped using the function f */
  override def map[B: Scalar: ClassTag](f: A => B): DiscreteScalarImage[D, B] = {
    DiscreteScalarImage(domain, data.map(f))
  }

  /** Returns a new ContinuousScalarImage by interpolating the given DiscreteScalarImage using b-spline interpolation of given order */
  def interpolate(order: Int): DifferentiableScalarImage[D]

  /** Returns a continuous scalar field. If you want a nearest neighbor interpolation that returns a [[ScalarImage]], use [[interpolate(0)]] instead*/
  override def interpolateNearestNeighbor: ScalarField[D, A] = {
    val ev = implicitly[Scalar[A]]
    ScalarField(RealSpace[D], this.interpolate(0) andThen ev.fromFloat _)
  }

  /** Returns a new DiscreteScalarImage which is obtained by resampling the given image on the points defined by the new domain */
  def resample(newDomain: DiscreteImageDomain[D], interpolationDegree: Int, outsideValue: Float): DiscreteScalarImage[D, A] = {

    val contImg = interpolate(interpolationDegree)
    contImg.sample(newDomain, outsideValue)
  }

}

/**
 * Factory methods for creating a new DiscreteScalarImage, as well as method to interpolate and resample images.
 */
object DiscreteScalarImage {

  trait Create[D <: Dim] {
    def create[A: Scalar: ClassTag](domain: DiscreteImageDomain[D], values: ScalarArray[A]): DiscreteScalarImage[D, A]
  }

  implicit object Create1D extends Create[_1D] {
    def create[A: Scalar: ClassTag](domain: DiscreteImageDomain[_1D], values: ScalarArray[A]): DiscreteScalarImage[_1D, A] = {
      new DiscreteScalarImage1D(domain, values)
    }
  }

  implicit object Create2D extends Create[_2D] {
    def create[A: Scalar: ClassTag](domain: DiscreteImageDomain[_2D], values: ScalarArray[A]): DiscreteScalarImage[_2D, A] = {
      new DiscreteScalarImage2D(domain, values)
    }
  }

  implicit object Create3D extends Create[_3D] {
    def create[A: Scalar: ClassTag](domain: DiscreteImageDomain[_3D], values: ScalarArray[A]): DiscreteScalarImage[_3D, A] = {
      new DiscreteScalarImage3D(domain, values)
    }
  }

  /** create a new DiscreteScalarImage with given domain and values */
  def apply[D <: Dim: NDSpace, A: Scalar: ClassTag](domain: DiscreteImageDomain[D], values: ScalarArray[A])(implicit create: Create[D]) = {
    create.create(domain, values)
  }

  /** create a new DiscreteScalarImage with given domain and values which are defined by the given function f */
  def apply[D <: Dim: NDSpace, A: Scalar: ClassTag](domain: DiscreteImageDomain[D], f: Point[D] => A)(implicit create: Create[D]) = {
    create.create(domain, ScalarArray(domain.points.map(f).toArray))
  }

  /** create a new DiscreteScalarImage, with all pixel values set to the given value */
  def apply[D <: Dim: NDSpace, A: Scalar: ClassTag](domain: DiscreteImageDomain[D])(v: => A)(implicit create: Create[D]) = {
    create.create(domain, ScalarArray(Array.fill(domain.numberOfPoints)(v)))
  }

  /**
   * computes the right index for the coefficient,
   * taking the boundary conditions into account (it mirrors at the border)
   */
  private[scalismo] def applyMirrorBoundaryCondition(k: Int, numCoefficients: Int) = {
    if (k < 0) -k
    else if (k >= numCoefficients) numCoefficients - (k - numCoefficients) - 2
    else k
  }

}

private class DiscreteScalarImage1D[A: Scalar: ClassTag](domain: DiscreteImageDomain[_1D], data: ScalarArray[A]) extends DiscreteScalarImage[_1D, A](domain, data) {

  def interpolate(degree: Int): DifferentiableScalarImage[_1D] = {

    val ck = determineCoefficients1D(degree, this)

    /*
     * Computes values at given point with corresponding coefficients and spline basis
     * */
    def iterateOnPoints(x: Point[_1D], splineBasis: ((Double) => Double)): Double = {
      val xUnit = (x(0) - domain.origin(0)) / domain.spacing(0)

      val k1 = scala.math.ceil(xUnit - 0.5f * (degree + 1)).toInt
      val K = degree + 1

      var result = 0.0
      var k = k1
      while (k <= k1 + K - 1) {
        val kBC = DiscreteScalarImage.applyMirrorBoundaryCondition(k, domain.size(0))
        result = result + splineBasis(xUnit.toDouble - k) * ck(kBC)
        k = k + 1
      }
      result
    }

    // the continuous interpolation function
    def f(x: Point[_1D]) = {
      val splineBasis: (Double => Double) = BSpline.nthOrderBSpline(degree)
      iterateOnPoints(x, splineBasis).toFloat
    }
    // the derivative
    def df(x: Point[_1D]) = {
      //derivative
      val splineBasisD1: (Double => Double) = { x => (BSpline.nthOrderBSpline(degree - 1)(x + 0.5f) - BSpline.nthOrderBSpline(degree - 1)(x - 0.5f)) * (1 / domain.spacing(0)) }
      Vector(iterateOnPoints(x, splineBasisD1).toFloat)
    }
    DifferentiableScalarImage(domain.boundingBox, f, df)
  }

  /* determine the b-spline coefficients for a 1D image */
  private def determineCoefficients1D[Pixel: Scalar](degree: Int, img: DiscreteScalarImage[_1D, Pixel]): Array[Float] = {
    val numeric = implicitly[Scalar[Pixel]]

    // the c is an input-output argument here
    val c = img.data.map(numeric.toFloat)
    val floats: Array[Float] = c.asInstanceOf[PrimitiveScalarArray[Float]].rawData
    BSplineCoefficients.getSplineInterpolationCoefficients(degree, floats)
    floats
  }
}

private class DiscreteScalarImage2D[A: Scalar: ClassTag](domain: DiscreteImageDomain[_2D], data: ScalarArray[A]) extends DiscreteScalarImage[_2D, A](domain, data) {

  def interpolate(degree: Int): DifferentiableScalarImage[_2D] = {
    val ck = determineCoefficients2D(degree, this)

    def iterateOnPoints(x: Point[_2D], splineBasis: ((Double, Double) => Double)): Double = {
      val xUnit = (x(0) - domain.origin(0)) / domain.spacing(0)
      val yUnit = (x(1) - domain.origin(1)) / domain.spacing(1)

      val k1 = scala.math.ceil(xUnit - 0.5f * (degree + 1)).toInt
      val l1 = scala.math.ceil(yUnit - 0.5f * (degree + 1)).toInt

      val K = degree + 1

      var result = 0.0
      var l = l1
      while (l <= l1 + K - 1) {
        val lBC = DiscreteScalarImage.applyMirrorBoundaryCondition(l, domain.size(1))
        var k = k1
        while (k <= k1 + K - 1) {
          val kBC = DiscreteScalarImage.applyMirrorBoundaryCondition(k, domain.size(0))
          val pointId = domain.pointId(Index(kBC, lBC))
          result = result + ck(pointId.id) * splineBasis(xUnit - k, yUnit - l)
          k = k + 1
        }
        l = l + 1
      }
      result
    }

    val bSplineNthOrder = BSpline.nthOrderBSpline(degree) _
    val bSplineNmin1thOrder = BSpline.nthOrderBSpline(degree - 1) _

    def f(x: Point[_2D]) = {
      val splineBasis = (x: Double, y: Double) => bSplineNthOrder(x) * bSplineNthOrder(y) // apply function
      iterateOnPoints(x, splineBasis).toFloat
    }
    def df(x: Point[_2D]) = {
      //derivativescalismo.
      val splineBasisD1 = (x: Double, y: Double) => (bSplineNmin1thOrder(x + 0.5f) - bSplineNmin1thOrder(x - 0.5f)) * bSplineNthOrder(y)
      val splineBasisD2 = (x: Double, y: Double) => bSplineNthOrder(x) * (bSplineNmin1thOrder(y + 0.5f) - bSplineNmin1thOrder(y - 0.5f))
      val dfx = (iterateOnPoints(x, splineBasisD1) * (1 / domain.spacing(0))).toFloat
      val dfy = (iterateOnPoints(x, splineBasisD2) * (1 / domain.spacing(1))).toFloat
      Vector(dfx, dfy)
    }

    DifferentiableScalarImage(domain.boundingBox, f, df)

  }

  /* determine the b-spline coefficients for a 2D image. The coefficients are retunred
  * as a DenseVector, i.e. the rows are written one after another */
  private def determineCoefficients2D[Pixel: Scalar](degree: Int, img: DiscreteScalarImage[_2D, Pixel]): Array[Float] = {
    val numeric = implicitly[Scalar[Pixel]]
    val coeffs = DenseVector.zeros[Float](img.values.size)
    var y = 0
    while (y < img.domain.size(1)) {
      val rowValues = (0 until img.domain.size(0)).map(x => img(img.domain.pointId(Index(x, y))))

      // the c is an input-output argument here
      val c = rowValues.map(numeric.toFloat).toArray
      BSplineCoefficients.getSplineInterpolationCoefficients(degree, c)

      val idxInCoeffs = img.domain.pointId(Index(0, y)).id
      coeffs(idxInCoeffs until idxInCoeffs + img.domain.size(0)) := DenseVector(c)
      y = y + 1
    }
    coeffs.data
  }
}

private class DiscreteScalarImage3D[A: Scalar: ClassTag](domain: DiscreteImageDomain[_3D], data: ScalarArray[A]) extends DiscreteScalarImage[_3D, A](domain, data) {
  def interpolate(degree: Int): DifferentiableScalarImage[_3D] = {
    val ck = determineCoefficients3D(degree, this)
    val pointToIdx = domain.indexToPhysicalCoordinateTransform.inverse

    def iterateOnPoints(x: Point[_3D], splineBasis: ((Double, Double, Double) => Double)): Double = {

      val unitCoords = pointToIdx(x)
      val xUnit = unitCoords(0)
      val yUnit = unitCoords(1)
      val zUnit = unitCoords(2)

      val k1 = scala.math.ceil(xUnit - 0.5f * (degree + 1)).toInt
      val l1 = scala.math.ceil(yUnit - 0.5f * (degree + 1)).toInt
      val m1 = scala.math.ceil(zUnit - 0.5f * (degree + 1)).toInt

      val K = degree + 1

      var result = 0.0
      var k = k1
      var l = l1
      var m = m1

      while (m <= m1 + K - 1) {
        val mBC = DiscreteScalarImage.applyMirrorBoundaryCondition(m, domain.size(2))
        l = l1
        while (l <= l1 + K - 1) {
          val lBC = DiscreteScalarImage.applyMirrorBoundaryCondition(l, domain.size(1))
          k = k1
          while (k <= k1 + K - 1) {
            val kBC = DiscreteScalarImage.applyMirrorBoundaryCondition(k, domain.size(0))
            val pointId = domain.pointId(Index(kBC, lBC, mBC))
            result = result + ck(pointId.id) * splineBasis(xUnit - k, yUnit - l, zUnit - m)
            k = k + 1
          }
          l = l + 1
        }
        m = m + 1
      }
      result
    }

    val bSplineNthOrder = BSpline.nthOrderBSpline(degree) _
    val bSplineNmin1thOrder = BSpline.nthOrderBSpline(degree - 1) _

    def f(x: Point[_3D]) = {
      val splineBasis = (x: Double, y: Double, z: Double) => bSplineNthOrder(x) * bSplineNthOrder(y) * bSplineNthOrder(z)
      iterateOnPoints(x, splineBasis).toFloat
    }
    def df(x: Point[_3D]) = {
      val splineBasisD1 = (x: Double, y: Double, z: Double) => (bSplineNmin1thOrder(x + 0.5f) - bSplineNmin1thOrder(x - 0.5f)) * bSplineNthOrder(y) * bSplineNthOrder(z)
      val splineBasisD2 = (x: Double, y: Double, z: Double) => bSplineNthOrder(x) * (bSplineNmin1thOrder(y + 0.5f) - bSplineNmin1thOrder(y - 0.5f)) * bSplineNthOrder(z)
      val splineBasisD3 = (x: Double, y: Double, z: Double) => bSplineNthOrder(x) * bSplineNthOrder(y) * (bSplineNmin1thOrder(z + 0.5f) - bSplineNmin1thOrder(z - 0.5f))
      val dfx = (iterateOnPoints(x, splineBasisD1) * (1 / domain.spacing(0))).toFloat
      val dfy = (iterateOnPoints(x, splineBasisD2) * (1 / domain.spacing(1))).toFloat
      val dfz = (iterateOnPoints(x, splineBasisD3) * (1 / domain.spacing(2))).toFloat
      Vector(dfx, dfy, dfz)
    }
    DifferentiableScalarImage(domain.boundingBox, f, df)

  }

  private def determineCoefficients3D[Pixel: Scalar](degree: Int, img: DiscreteScalarImage[_3D, Pixel]): Array[Float] = {
    val numeric = implicitly[Scalar[Pixel]]
    val coeffs = DenseVector.zeros[Float](img.values.size)
    var z = 0
    var y = 0
    while (z < img.domain.size(2)) {
      y = 0
      while (y < img.domain.size(1)) {
        val rowValues = (0 until img.domain.size(0)).map(x => img(Index(x, y, z)))

        // the c is an input-output argument here
        val c = rowValues.map(numeric.toFloat).toArray
        BSplineCoefficients.getSplineInterpolationCoefficients(degree, c)
        val idxInCoeffs = img.domain.pointId(Index(0, y, z)).id
        coeffs(idxInCoeffs until idxInCoeffs + img.domain.size(0)) := DenseVector(c)
        y = y + 1
      }
      z = z + 1
    }
    coeffs.data
  }

}

