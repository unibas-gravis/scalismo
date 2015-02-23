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
import scalismo.numerics.BSpline

import scala.language.implicitConversions
import scala.reflect.ClassTag

/**
 * Basic interface for a discrete image of arbitrary Pixel type
 *
 * @tparam D  The dimensionality of the image
 * @tparam Pixel The type of the pixel (usually a scalar or a vector)
 */
trait DiscreteImage[D <: Dim, Pixel] extends DiscreteField[D, Pixel] {

  protected[this] def ndSpace: NDSpace[D]

  override def domain: DiscreteImageDomain[D]

  val dimensionality = ndSpace.dimensionality

  def apply(idx: Index[D]): Pixel = this(domain.pointId(idx))

  def isDefinedAt(idx: Index[D]): Boolean = {
    (0 until dimensionality).foldLeft(true)((res, d) => res && idx(d) >= 0 && idx(d) < domain.size(d))
  }

}

/**
 * A scalar valued discrete image.
 *
 * @param domain The domain over which this image is defined
 * @param data The values for each grid points.
 * @tparam D  The dimensionality of the image
 * @tparam A The type of the pixel (needs to implement Scalar).
 */
class DiscreteScalarImage[D <: Dim: NDSpace, A: Scalar: ClassTag] private (override val domain: DiscreteImageDomain[D], data: ScalarArray[A])
    extends DiscreteScalarField[D, A](domain, data) with DiscreteImage[D, A] {

  require(domain.numberOfPoints == data.size)

  protected override def ndSpace = implicitly[NDSpace[D]]

  /** returns a new image whose whose pixel values have been mapped using the function f */
  override def map[B: Scalar: ClassTag](f: A => B): DiscreteScalarImage[D, B] = {
    new DiscreteScalarImage(domain, data.map(f))
  }

  /** Returns a new ContinuousScalarImage by interpolating the given DiscreteScalarImage using b-spline interpoation of given order */
  def interpolate(order: Int)(implicit ev: DiscreteScalarImage.CanInterpolate[D]): DifferentiableScalarImage[D] = {
    ev.interpolate(this, order)
  }

  /** Returns a new DiscreteScalarImage which is obtained by resampling the given image on the points defined by the new domain */
  def resample(newDomain: DiscreteImageDomain[D], interpolationDegree: Int, outsideValue: Float)(implicit ev: DiscreteScalarImage.CanInterpolate[D]): DiscreteScalarImage[D, A] = {
    val contImg = interpolate(interpolationDegree)
    contImg.sample(newDomain, outsideValue)
  }

}

/**
 * Factory methods for creating a new DiscreteScalarImage, as well as method to interpolate and resample images.
 */
object DiscreteScalarImage {

  /** create a new DiscreteScalarImage with given domain and values */
  def apply[D <: Dim: NDSpace, A: Scalar: ClassTag](domain: DiscreteImageDomain[D], values: ScalarArray[A]) = {
    new DiscreteScalarImage[D, A](domain, values)
  }

  /** create a new DiscreteScalarImage with given domain and values which are defined by the given function f */
  def apply[D <: Dim: NDSpace, A: Scalar: ClassTag](domain: DiscreteImageDomain[D], f: Point[D] => A) = {
    new DiscreteScalarImage[D, A](domain, ScalarArray(domain.points.map(f).toArray))
  }

  /** create a new DiscreteScalarImage, with all pixel values set to the given value */
  def apply[D <: Dim: NDSpace, A: Scalar: ClassTag](domain: DiscreteImageDomain[D])(v: => A) = {
    new DiscreteScalarImage[D, A](domain, ScalarArray(Array.fill(domain.numberOfPoints)(v)))
  }

  /**
   * Typeclass for doing interpolation
   */
  trait CanInterpolate[D <: Dim] {
    def interpolate[S: Scalar](image: DiscreteScalarImage[D, S], degree: Int): DifferentiableScalarImage[D]
  }

  object CanInterpolate {

    implicit object _1DImageinterpolate extends CanInterpolate[_1D] {
      def interpolate[S: Scalar](image: DiscreteScalarImage[_1D, S], degree: Int): DifferentiableScalarImage[_1D] = {

        val ck = determineCoefficients1D(degree, image)

        /*
         * Computes values at given point with corresponding coefficients and spline basis
         * */
        def iterateOnPoints(x: Point[_1D], splineBasis: ((Double) => Double)): Double = {
          val xUnit = (x(0) - image.domain.origin(0)) / image.domain.spacing(0)

          val k1 = scala.math.ceil(xUnit - 0.5f * (degree + 1)).toInt
          val K = degree + 1

          var result = 0.0
          var k = k1
          while (k <= k1 + K - 1) {
            val kBC = applyMirrorBoundaryCondition(k, image.domain.size(0))
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
          val splineBasisD1: (Double => Double) = { x => (BSpline.nthOrderBSpline(degree - 1)(x + 0.5f) - BSpline.nthOrderBSpline(degree - 1)(x - 0.5f)) * (1 / image.domain.spacing(0)) }
          Vector(iterateOnPoints(x, splineBasisD1).toFloat)
        }
        DifferentiableScalarImage(image.domain.imageBox, f, df)
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

    implicit object _2DImageinterpolate extends CanInterpolate[_2D] {
      def interpolate[S: Scalar](image: DiscreteScalarImage[_2D, S], degree: Int): DifferentiableScalarImage[_2D] = {
        val ck = determineCoefficients2D(degree, image)

        def iterateOnPoints(x: Point[_2D], splineBasis: ((Double, Double) => Double)): Double = {
          val xUnit = (x(0) - image.domain.origin(0)) / image.domain.spacing(0)
          val yUnit = (x(1) - image.domain.origin(1)) / image.domain.spacing(1)

          val k1 = scala.math.ceil(xUnit - 0.5f * (degree + 1)).toInt
          val l1 = scala.math.ceil(yUnit - 0.5f * (degree + 1)).toInt

          val K = degree + 1

          var result = 0.0
          var l = l1
          while (l <= l1 + K - 1) {
            val lBC = applyMirrorBoundaryCondition(l, image.domain.size(1))
            var k = k1
            while (k <= k1 + K - 1) {
              val kBC = applyMirrorBoundaryCondition(k, image.domain.size(0))
              val idx = image.domain.pointId(Index(kBC, lBC))
              result = result + ck(idx) * splineBasis(xUnit - k, yUnit - l)
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
          //derivative
          val splineBasisD1 = (x: Double, y: Double) => (bSplineNmin1thOrder(x + 0.5f) - bSplineNmin1thOrder(x - 0.5f)) * bSplineNthOrder(y)
          val splineBasisD2 = (x: Double, y: Double) => bSplineNthOrder(x) * (bSplineNmin1thOrder(y + 0.5f) - bSplineNmin1thOrder(y - 0.5f))
          val dfx = (iterateOnPoints(x, splineBasisD1) * (1 / image.domain.spacing(0))).toFloat
          val dfy = (iterateOnPoints(x, splineBasisD2) * (1 / image.domain.spacing(1))).toFloat
          Vector(dfx, dfy)
        }

        DifferentiableScalarImage(image.domain.imageBox, f, df)

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

          val idxInCoeffs = img.domain.pointId(Index(0, y))
          coeffs(idxInCoeffs until idxInCoeffs + img.domain.size(0)) := DenseVector(c)
          y = y + 1
        }
        coeffs.data
      }

    }

    implicit object _3DImageinterpolate extends CanInterpolate[_3D] {
      def interpolate[S: Scalar](image: DiscreteScalarImage[_3D, S], degree: Int): DifferentiableScalarImage[_3D] = {
        val ck = determineCoefficients3D(degree, image)

        def iterateOnPoints(x: Point[_3D], splineBasis: ((Double, Double, Double) => Double)): Double = {
          val xUnit = (x(0) - image.domain.origin(0)) / image.domain.spacing(0)
          val yUnit = (x(1) - image.domain.origin(1)) / image.domain.spacing(1)
          val zUnit = (x(2) - image.domain.origin(2)) / image.domain.spacing(2)

          val k1 = scala.math.ceil(xUnit - 0.5f * (degree + 1)).toInt
          val l1 = scala.math.ceil(yUnit - 0.5f * (degree + 1)).toInt
          val m1 = scala.math.ceil(zUnit - 0.5f * (degree + 1)).toInt

          val K = degree + 1

          var result = 0.0
          var k = k1
          var l = l1
          var m = m1

          while (m <= m1 + K - 1) {
            val mBC = applyMirrorBoundaryCondition(m, image.domain.size(2))
            l = l1
            while (l <= l1 + K - 1) {
              val lBC = applyMirrorBoundaryCondition(l, image.domain.size(1))
              k = k1
              while (k <= k1 + K - 1) {
                val kBC = applyMirrorBoundaryCondition(k, image.domain.size(0))
                val idx = image.domain.pointId(Index(kBC, lBC, mBC))
                result = result + ck(idx) * splineBasis(xUnit - k, yUnit - l, zUnit - m)
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
          val dfx = (iterateOnPoints(x, splineBasisD1) * (1 / image.domain.spacing(0))).toFloat
          val dfy = (iterateOnPoints(x, splineBasisD2) * (1 / image.domain.spacing(1))).toFloat
          val dfz = (iterateOnPoints(x, splineBasisD3) * (1 / image.domain.spacing(2))).toFloat
          Vector(dfx, dfy, dfz)
        }
        DifferentiableScalarImage(image.domain.imageBox, f, df)

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
            val idxInCoeffs = img.domain.pointId(Index(0, y, z))
            coeffs(idxInCoeffs until idxInCoeffs + img.domain.size(0)) := DenseVector(c)
            y = y + 1
          }
          z = z + 1
        }
        coeffs.data
      }

    }

    /**
     * computes the right index for the coefficient,
     * taking the boundary conditions into account (it mirrors at the border)
     */
    private def applyMirrorBoundaryCondition(k: Int, numCoefficients: Int) = {
      if (k < 0) -k
      else if (k >= numCoefficients) numCoefficients - (k - numCoefficients) - 2
      else k
    }

  }

}

