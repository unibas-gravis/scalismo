package smptk.image

import scala.language.higherKinds
import scala.language.implicitConversions

import breeze.plot._
import breeze.linalg._
import scala.util._
import java.io.IOException

import Image._
import smptk.image.Geometry.CoordVector1D
import smptk.image.Geometry.CoordVector2D
import smptk.image.Geometry.CoordVector3D
import scala.reflect.runtime.universe.{ TypeTag }
import scala.reflect.ClassTag

object Interpolation {

  def splineInterpolate[Point](p: Point): (Point => Float) = p => 0

  def bSpline(n: Int)(x: Float): Float = {
    val absX = scala.math.abs(x)
    n match {
      case 0 => {
        if (-0.5 < x && x < 0.5) 1f
        else if (absX == 0.5) 0.5f
        else 0

      }
      case 1 => {
        if (-1 <= x && x <= 0) 1f + x
        else if (0 < x && x <= 1) 1f - x
        else 0
      }
      case 2 => {
        if (-1.5 <= x && x < -0.5) 0.5f * scala.math.pow(x + 1.5f, 2).toFloat
        else if (-0.5 <= x && x < 0.5) -scala.math.pow(x + 0.5, 2).toFloat + (x - 0.5f) + 1.5f
        else if (x >= 0.5 && x < 1.5) 0.5f * scala.math.pow(1 - (x - 0.5f), 2).toFloat
        else 0

      }
      case 3 => {

        if (absX >= 0 && absX < 1)
          (0.66666666 - scala.math.pow(absX, 2) + 0.5f * scala.math.pow(absX, 3)).toFloat
        else if (absX >= 1 && absX < 2)
          (scala.math.pow((2 - absX), 3) / 6).toFloat
        else 0
      }
      case _ => throw new NotImplementedError("Bspline of order " + n + " is not implemented yet")
    }
  }

  def determineCoefficientsWithLinearSystem[Scalar <% Double: ClassTag](degree: Int, img: DiscreteScalarImage1D[Scalar]): DenseVector[Float] = {
    val N: Int = img.domain.numberOfPoints
    val splineBasis: (Float => Float) = bSpline(degree)
    val I = DenseVector(img.pixelValues.toArray).map(_.toDouble)

    val betaMat = DenseMatrix.zeros[Double](N, N)

    for (ptNumber <- 0 until N; i <- 0 until N) {
      betaMat(i, ptNumber) = splineBasis(ptNumber - i)
    }

    val c1 = (betaMat \ I).map(_.toFloat)
    c1

  }

  def determineCoefficients[Scalar <% Double: ClassTag](degree: Int, img: DiscreteScalarImage1D[Scalar]): DenseVector[Float] = {
    // the c is an input-output argument here
    val c = img.pixelValues.toArray.map(_.toDouble)
    BSplineCoefficients.getSplineInterpolationCoefficients(degree, c)
    DenseVector(c.map(_.toFloat))
  }

  def determineCoefficientsWithLinearSystem[Scalar <% Double: ClassTag](degree: Int, img: DiscreteScalarImage2D[Scalar]): DenseVector[Float] = {

    val N: Int = img.domain.numberOfPoints
    val splineBasis = (a: Float, b: Float) => bSpline(degree)(a) * bSpline(degree)(b)
    val I = DenseVector(img.pixelValues.toArray).map(_.toDouble)

    val betaMat = DenseMatrix.zeros[Double](N, N)

    for (
      ptNumber <- 0 until N;
      j <- 0 until img.domain.size(1);
      i <- 0 until img.domain.size(0)
    ) {
      val coordvector2D = img.domain.linearIndexToIndex(ptNumber)
      betaMat(ptNumber, img.domain.indexToLinearIndex((i, j))) = splineBasis(coordvector2D(0) - i, coordvector2D(1) - j)
    }

    (betaMat \ I).map(_.toFloat)

  }

  def determineCoefficients[Scalar <% Double: ClassTag](degree: Int, img: DiscreteScalarImage2D[Scalar]): DenseVector[Float] = {
    val coeffs = DenseVector.zeros[Float](img.pixelValues.size)
    for (y <- 0 until img.domain.size(1)) {
      val rowValues = (0 until img.domain.size(0)).map(x => img.pixelValues(img.domain.indexToLinearIndex((x, y))))

      // the c is an input-output argument here
      val c = rowValues.toArray.map(_.toDouble)
      BSplineCoefficients.getSplineInterpolationCoefficients(degree, c)

      val idxInCoeffs = img.domain.indexToLinearIndex((0, y))
      coeffs(idxInCoeffs until idxInCoeffs + img.domain.size(0)) := DenseVector(c.map(_.toFloat))
      //coeffs(img.domain.indexToLinearIndex((0,y)) until img.domain.indexToLinearIndex((img.domain.size(0)-1,y))) := DenseVector(c.map(_.toFloat))
    }
    coeffs
  }

  def determineCoefficients[Scalar <% Double: ClassTag](degree: Int, img: DiscreteScalarImage3D[Scalar]): DenseVector[Float] = {
    val coeffs = DenseVector.zeros[Float](img.pixelValues.size)
    for (z <- 0 until img.domain.size(2)) {
      for (y <- 0 until img.domain.size(1)) {
        val rowValues = (0 until img.domain.size(0)).map(x => img.pixelValues(img.domain.indexToLinearIndex((x, y, z))))

        // the c is an input-output argument here
        val c = rowValues.toArray.map(_.toDouble)
        BSplineCoefficients.getSplineInterpolationCoefficients(degree, c)
        val idxInCoeffs = img.domain.indexToLinearIndex((0, y, z))
        coeffs(idxInCoeffs until idxInCoeffs + img.domain.size(0)) := DenseVector(c.map(_.toFloat))
      }
    }
    coeffs
  }

  def interpolate[Scalar <% Double: ClassTag](degree: Int)(image: DiscreteScalarImage1D[Scalar]): ContinuousScalarImage1D = {

    val ck = determineCoefficients(degree, image)

    def iterateOnPoints(x: CoordVector1D[Float], splineBasis: ((Float) => Float)): Float = {
      val xUnit = (x(0) - image.domain.origin(0)) / image.domain.spacing(0)

      val k1 = scala.math.max(scala.math.ceil(xUnit - 0.5f * (degree + 1)).toInt, 0)
      val K = scala.math.min(degree + 1, ck.size - 1)

      var result = 0f
      var k = k1
      while (k <= scala.math.min(k1 + K - 1, ck.size - 1)) {
        result = result + splineBasis(xUnit - k) * ck(k)
        k = k + 1
      }
      result
    }

    new ContinuousScalarImage1D(
      (x : CoordVector1D[Float]) => image.domain.isInside(x),
      (x: CoordVector1D[Float]) => {
        val splineBasis: (Float => Float) = bSpline(degree) // apply function
        iterateOnPoints(x, splineBasis)
      },

      (x: CoordVector1D[Float]) => { //derivative
        val splineBasisD1: (Float => Float) = { x => bSpline(degree - 1)(x + 0.5f) - bSpline(degree - 1)(x - 0.5f) }
        DenseVector(iterateOnPoints(x, splineBasisD1))

      })

  }

  def interpolate2D[Scalar <% Double: ClassTag](degree: Int)(image: DiscreteScalarImage2D[Scalar]): ContinuousScalarImage2D = {
    val ck = determineCoefficients(degree, image)

    def iterateOnPoints( x: CoordVector2D[Float], splineBasis: ((Float, Float) => Float)): Float = {
      val xUnit = ((x(0) - image.domain.origin(0)) / image.domain.spacing(0))
      val yUnit = ((x(1) - image.domain.origin(1)) / image.domain.spacing(1))

      val k1 = scala.math.max(scala.math.ceil(xUnit - 0.5f * (degree + 1)).toInt, 0)
      val l1 = scala.math.max(scala.math.ceil(yUnit - 0.5f * (degree + 1)).toInt, 0)

      val K = degree + 1

      var result = 0f
      var k = k1
      var l = l1

      while (l <= scala.math.min(l1 + K - 1, image.domain.size(1) - 1)) {
        k = k1
        while (k <= scala.math.min(k1 + K - 1, image.domain.size(0) - 1)) {

          val idx = image.domain.indexToLinearIndex((k, l))
          result = result + ck(idx) * splineBasis(xUnit - k, yUnit - l)
          k = k + 1
        }
        l = l + 1
      }

      result
    }

    val bSplineNthOrder = bSpline(degree)_
    val bSplineNmin1thOrder = bSpline(degree - 11)_
    new ContinuousScalarImage2D(
      (x: CoordVector2D[Float]) => image.domain.isInside(x),
      (x: CoordVector2D[Float]) => {
        val splineBasis = (x: Float, y: Float) => bSplineNthOrder(x) * bSplineNthOrder(y) // apply function
        iterateOnPoints( x,  splineBasis)
      },

      (x: CoordVector2D[Float]) => { //derivative
        val splineBasisD1 = (x: Float, y: Float) => (bSplineNmin1thOrder(x + 0.5f) - bSplineNmin1thOrder(x - 0.5f)) * bSplineNthOrder(y)
        val splineBasisD2 = (x: Float, y: Float) => bSplineNthOrder(x) * (bSplineNmin1thOrder(y + 0.5f) - bSplineNmin1thOrder(y - 0.5f))
        DenseVector(iterateOnPoints( x,  splineBasisD1), iterateOnPoints(x,splineBasisD2))
      })
  }

  def interpolate3D[Scalar <% Double: ClassTag](degree: Int)(image: DiscreteScalarImage3D[Scalar]): ContinuousScalarImage3D = {
    val ck = determineCoefficients(degree, image)

    def iterateOnPoints(x: CoordVector3D[Float], splineBasis: ((Float, Float, Float) => Float)): Float = {
      val xUnit = ((x(0) - image.domain.origin(0)) / image.domain.spacing(0))
      val yUnit = ((x(1) - image.domain.origin(1)) / image.domain.spacing(1))
      val zUnit = ((x(2) - image.domain.origin(2)) / image.domain.spacing(2))

      val k1 = scala.math.max(scala.math.ceil(xUnit - 0.5f * (degree + 1)).toInt, 0)
      val l1 = scala.math.max(scala.math.ceil(yUnit - 0.5f * (degree + 1)).toInt, 0)
      val m1 = scala.math.max(scala.math.ceil(zUnit - 0.5f * (degree + 1)).toInt, 0)

      val K = degree + 1

      var result = 0f
      var k = k1
      var l = l1
      var m = m1

      while (m <= scala.math.min(m1 + K - 1, image.domain.size(2) - 1)) {
        l = l1
        while (l <= scala.math.min(l1 + K - 1, image.domain.size(1) - 1)) {
          k = k1
          while (k <= scala.math.min(k1 + K - 1, image.domain.size(0) - 1)) {

            val idx = image.domain.indexToLinearIndex((k, l, m))
            result = result + ck(idx) * splineBasis(xUnit - k, yUnit - l, zUnit - m)
            k = k + 1
          }
          l = l + 1
        }
        m = m + 1
      }
      result
    }

    val bSplineNthOrder = bSpline(degree)_
    val bSplineNmin1thOrder = bSpline(degree - 11)_

    new ContinuousScalarImage3D(
      (x : CoordVector3D[Float]) => image.domain.isInside(x),
      (x: CoordVector3D[Float]) => { // apply function
        val splineBasis = (x: Float, y: Float, z: Float) => bSplineNthOrder(x) * bSplineNthOrder(y) * bSplineNthOrder(z)
        iterateOnPoints(x, splineBasis)
      },

      (x: CoordVector3D[Float]) => { //derivative

        val splineBasisD1 = (x: Float, y: Float, z: Float) => (bSplineNmin1thOrder(x + 0.5f) - bSplineNmin1thOrder(x - 0.5f)) * bSplineNthOrder(y) * bSplineNthOrder(z)
        val splineBasisD2 = (x: Float, y: Float, z: Float) => bSplineNthOrder(x) * (bSplineNmin1thOrder(y + 0.5f) - bSplineNmin1thOrder(y - 0.5f)) * bSplineNthOrder(z)
        val splineBasisD3 = (x: Float, y: Float, z: Float) => bSplineNthOrder(x) * bSplineNthOrder(y) * (bSplineNmin1thOrder(z + 0.5f) - bSplineNmin1thOrder(z - 0.5f))
        DenseVector(iterateOnPoints(x, splineBasisD1), iterateOnPoints(x, splineBasisD2), iterateOnPoints(x, splineBasisD3))
      })

  }

  def main(args: Array[String]) {
    val a: Try[Int] = Failure(new IOException("abc"))
    val b: Try[Int] = Success(5)
    val f = Figure()
    val p = f.subplot(0)
    val xs = linspace(0, 30, 500).map(_.toFloat)
    val range = (0 until 30).toIndexedSeq
    val ps = DiscreteScalarImage1D(DiscreteImageDomain1D(0f, 1f, 30), range.map(Math.sin(_)).toArray)
    val continuousImg = interpolate(3)(ps)

    p += plot(xs, xs.map(x => continuousImg(x)))
    p += plot(xs, xs.map(x => continuousImg.takeDerivative(x)(0)))
  }
}

//  def determineCoefficients[Scalar <% Double: ClassTag](degree: Int, img: DiscreteScalarImage3D[Scalar]): DenseVector[Float] = {
//
//    val N: Int = img.domain.points.size
//    val splineBasis = (a: Float, b: Float, c: Float) => bSpline(degree)(a) * bSpline(degree)(b) * bSpline(degree)(c)
//    val I = DenseVector(img.pixelValues.toArray).map(_.toDouble)
//
//    val betaMat = DenseMatrix.zeros[Double](N, N)
//
//    for (
//      ptNumber <- 0 until N;
//      k <- 0 until img.domain.size(2);
//      j <- 0 until img.domain.size(1);
//      i <- 0 until img.domain.size(0)
//    ) {
//      val coordvector3D = img.domain.linearIndexToIndex(ptNumber)
//      betaMat(ptNumber, img.domain.indexToLinearIndex((i, j, k))) = splineBasis(coordvector3D(0) - i, coordvector3D(1) - j, coordvector3D(2) - k)
//    }
//
//    (betaMat \ I).map(_.toFloat)
//
//  }