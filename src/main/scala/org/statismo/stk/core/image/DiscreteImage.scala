package org.statismo.stk.core
package image

import breeze.linalg.DenseVector
import org.statismo.stk.core.numerics.BSpline._

import scala.language.implicitConversions
import scala.{specialized => spec}
import reflect.runtime.universe.{TypeTag, typeOf}
import scala.reflect.ClassTag
import scala.reflect.ClassTag
import org.statismo.stk.core.geometry._
import org.statismo.stk.core.common.PointData
import org.statismo.stk.core.common.ScalarPointData
import spire.math.Numeric

trait DiscreteImage[D <: Dim, @specialized(Float, Short) Pixel] extends PointData[D, Pixel] {
  def ndSpace: NDSpace[D]

  def domain: DiscreteImageDomain[D]

  val dimensionality = ndSpace.dimensionality

  def apply(idx: Index[D]): Pixel = values(domain.indexToLinearIndex(idx))

  def isDefinedAt(idx: Index[D]): Boolean = {
    (0 until dimensionality).foldLeft(true)((res, d) => res && idx(d) >= 0 && idx(d) < domain.size(d))
  }

  override lazy val hashCode = super.hashCode

}

class DiscreteScalarImage[D <: Dim : NDSpace, A: Numeric : ClassTag] private(val domain: DiscreteImageDomain[D], val values: Array[A]) extends DiscreteImage[D, A] with ScalarPointData[D, A] {
  require(domain.numberOfPoints == values.size)

  override def numeric = implicitly[Numeric[A]]

  override def ndSpace = implicitly[NDSpace[D]]


  def map[B: Numeric : ClassTag](f: A => B): DiscreteScalarImage[D, B] = {
    new DiscreteScalarImage(domain, values.map(f))
  }
}


object DiscreteScalarImage {


  trait Create[D <: Dim] {
    def createDiscreteScalarImage[A: Numeric : ClassTag](domain: DiscreteImageDomain[D], values: Array[A]): DiscreteScalarImage[D, A]
  }

  object Create {

    implicit object createDiscreteScalarImage1D extends Create[_1D] {
      override def createDiscreteScalarImage[A: Numeric : ClassTag](domain: DiscreteImageDomain[_1D], values: Array[A]): DiscreteScalarImage[_1D, A] = {
        new DiscreteScalarImage[_1D, A](domain, values)
      }
    }

    implicit object createDiscreteScalarImage2D extends Create[_2D] {
      override def createDiscreteScalarImage[A: Numeric : ClassTag](domain: DiscreteImageDomain[_2D], values: Array[A]): DiscreteScalarImage[_2D, A] = {
        new DiscreteScalarImage[_2D, A](domain, values)
      }
    }

    implicit object createDiscreteScalarImage3D extends Create[_3D] {
      override def createDiscreteScalarImage[A: Numeric : ClassTag](domain: DiscreteImageDomain[_3D], values: Array[A]): DiscreteScalarImage[_3D, A] = {
        new DiscreteScalarImage[_3D, A](domain, values)
      }
    }

  }

  def apply[D <: Dim : NDSpace, A: Numeric : ClassTag](domain: DiscreteImageDomain[D], values: Array[A])(implicit evCreateImage: Create[D]) = {
    evCreateImage.createDiscreteScalarImage(domain, values)
  }

  def apply[D <: Dim : NDSpace, A: Numeric : ClassTag](domain: DiscreteImageDomain[D], f: Point[D] => A)(implicit evCreateImage: Create[D]) = {
    evCreateImage.createDiscreteScalarImage(domain, domain.points.map(f).toArray)
  }


  def interpolate[D <: Dim : Interpolator, @specialized(Short, Int, Float, Double) Scalar: Numeric](image: DiscreteScalarImage[D, Scalar], degree: Int)
  : ContinuousScalarImage[D] = {
    implicitly[Interpolator[D]].interpolate(image, degree)
  }


  // interpolation stuff
  //
  trait Interpolator[D <: Dim] {
    def interpolate[@specialized(Short, Int, Float, Double) Scalar: Numeric](image: DiscreteScalarImage[D, Scalar], degree: Int)
    : ContinuousScalarImage[D]
  }

  object Interpolator {

    implicit object _1DImageinterpolate extends Interpolator[_1D] {
      def interpolate[@specialized(Short, Int, Float, Double) Scalar: Numeric](image: DiscreteScalarImage[_1D, Scalar], degree: Int)
      : ContinuousScalarImage[_1D] = {

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
          val splineBasis: (Double => Double) = nthOrderBSpline(degree)
          iterateOnPoints(x, splineBasis).toFloat
        }
        // the derivative
        def df(x: Point[_1D]) = {
          //derivative
          val splineBasisD1: (Double => Double) = { x => (nthOrderBSpline(degree - 1)(x + 0.5f) - nthOrderBSpline(degree - 1)(x - 0.5f)) * (1 / image.domain.spacing(0))}
          Vector(iterateOnPoints(x, splineBasisD1).toFloat)
        }
        new ContinuousScalarImage1D(image.domain, f, Some(df))
      }

      /* determine the b-spline coefficients for a 1D image */
      private def determineCoefficients1D[@specialized(Short, Int, Float, Double) Pixel: Numeric](degree: Int, img: DiscreteScalarImage[_1D, Pixel]): Array[Float] = {
        val numeric = implicitly[Numeric[Pixel]]

        // the c is an input-output argument here
        val c = img.values.map(numeric.toFloat)
        BSplineCoefficients.getSplineInterpolationCoefficients(degree, c)
        c
      }
    }



    implicit object _2DImageinterpolate extends Interpolator[_2D] {
      def interpolate[@specialized(Short, Int, Float, Double) Scalar: Numeric](image: DiscreteScalarImage[_2D, Scalar], degree: Int)
      : ContinuousScalarImage[_2D] =
      {
        val ck = determineCoefficients2D(degree, image)

        def iterateOnPoints(x: Point[_2D], splineBasis: ((Double, Double) => Double)): Double = {
          val xUnit = ((x(0) - image.domain.origin(0)) / image.domain.spacing(0))
          val yUnit = ((x(1) - image.domain.origin(1)) / image.domain.spacing(1))

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
              val idx = image.domain.indexToLinearIndex(Index(kBC, lBC))
              result = result + ck(idx) * splineBasis(xUnit - k, yUnit - l)
              k = k + 1
            }
            l = l + 1
          }
          result
        }

        val bSplineNthOrder = nthOrderBSpline(degree) _
        val bSplineNmin1thOrder = nthOrderBSpline(degree - 1) _

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

        new ContinuousScalarImage2D(image.domain, f, Some(df))

      }

      /* determine the b-spline coefficients for a 2D image. The coefficients are retunred
      * as a DenseVector, i.e. the rows are written one after another */
      private def determineCoefficients2D[@specialized(Short, Int, Float, Double) Pixel: Numeric](degree: Int, img: DiscreteScalarImage[_2D, Pixel]): Array[Float] = {
        val numeric = implicitly[Numeric[Pixel]]
        val coeffs = DenseVector.zeros[Float](img.values.size)
        var y = 0
        while (y < img.domain.size(1)) {
          val rowValues = (0 until img.domain.size(0)).map(x => img.values(img.domain.indexToLinearIndex(Index(x, y))))

          // the c is an input-output argument here
          val c = rowValues.map(numeric.toFloat).toArray
          BSplineCoefficients.getSplineInterpolationCoefficients(degree, c)

          val idxInCoeffs = img.domain.indexToLinearIndex(Index(0, y))
          coeffs(idxInCoeffs until idxInCoeffs + img.domain.size(0)) := DenseVector(c)
          y = y + 1
        }
        coeffs.data
      }



    }


    implicit object _3DImageinterpolate extends Interpolator[_3D] {
      def interpolate[@specialized(Short, Int, Float, Double) Scalar: Numeric](image: DiscreteScalarImage[_3D, Scalar], degree: Int)
      : ContinuousScalarImage[_3D] =
      {
        val ck = determineCoefficients3D(degree, image)

        def iterateOnPoints(x: Point[_3D], splineBasis: ((Double, Double, Double) => Double)): Double = {
          val xUnit = ((x(0) - image.domain.origin(0)) / image.domain.spacing(0))
          val yUnit = ((x(1) - image.domain.origin(1)) / image.domain.spacing(1))
          val zUnit = ((x(2) - image.domain.origin(2)) / image.domain.spacing(2))

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
                val idx = image.domain.indexToLinearIndex(Index(kBC, lBC, mBC))
                result = result + ck(idx) * splineBasis(xUnit - k, yUnit - l, zUnit - m)
                k = k + 1
              }
              l = l + 1
            }
            m = m + 1
          }
          result
        }

        val bSplineNthOrder = nthOrderBSpline(degree) _
        val bSplineNmin1thOrder = nthOrderBSpline(degree - 1) _

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
        new ContinuousScalarImage3D(image.domain, f, Some(df))

      }



      private def determineCoefficients3D[@specialized(Short, Int, Float, Double) Pixel: Numeric](degree: Int, img: DiscreteScalarImage[_3D, Pixel]): Array[Float] = {
        val numeric = implicitly[Numeric[Pixel]]
        val coeffs = DenseVector.zeros[Float](img.values.size)
        var z = 0
        var y = 0
        while (z < img.domain.size(2)) {
          y = 0
          while (y < img.domain.size(1)) {
            val rowValues = (0 until img.domain.size(0)).map(x => img.values(img.domain.indexToLinearIndex(Index(x, y, z))))

            // the c is an input-output argument here
            val c = rowValues.map(numeric.toFloat).toArray
            BSplineCoefficients.getSplineInterpolationCoefficients(degree, c)
            val idxInCoeffs = img.domain.indexToLinearIndex(Index(0, y, z))
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
     **/
    private def applyMirrorBoundaryCondition(k: Int, numCoefficients: Int) = {
      if (k < 0) -k
      else if (k >= numCoefficients) numCoefficients - (k - numCoefficients) - 2
      else k
    }


  }


}




