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

  private val twoByThree = 2.0 / 3.0; // a constant used later on
  def splineInterpolate[Point](p: Point): (Point => Double) = p => 0

  def bSpline(n: Int)(x: Double): Double = {
    val absX : Double = scala.math.abs(x)
    val absXSquared : Double = absX * absX
    val absXCube : Double = absXSquared * absX
    val twoMinAbsX : Double = 2. - absX
    
    n match {
      case 0 => {
        if (-0.5 < x && x < 0.5) 1.
        else if (absX == 0.5) 0.5
        else 0

      }
      case 1 => {
        if (-1 <= x && x <= 0) 1. + x
        else if (0 < x && x <= 1) 1. - x
        else 0
      }
      case 2 => {
        if (-1.5 <= x && x < -0.5) 0.5 * (x + 1.5)*(x + 1.5)
        else if  (-0.5 <= x && x < 0.5) -(x + 0.5)*(x + 0.5) + (x - 0.5) + 1.5
        else if (x >= 0.5 && x < 1.5) 0.5 * (1 - (x - 0.5))*(1 - (x - 0.5))
        else 0

      }
      case 3 => {

        if (absX >= 0 && absX < 1)
          twoByThree - absXSquared + 0.5 * absXCube
        else if (absX >= 1 && absX < 2)
        	twoMinAbsX * twoMinAbsX *  twoMinAbsX / 6.
        else 0
      }
      case _ => throw new NotImplementedError("Bspline of order " + n + " is not implemented yet")
    }
  }

  /*
   * not used anymore
   
  def determineCoefficientsWithLinearSystem[Scalar <% Double: ClassTag](degree: Int, img: DiscreteScalarImage1D[Scalar]): DenseVector[Double] = {
    val N: Int = img.domain.numberOfPoints
    val splineBasis: (Double => Double) = bSpline(degree)
    val I = DenseVector(img.pixelValues.toArray).map(_.toDouble)

    val betaMat = DenseMatrix.zeros[Double](N, N)

    for (ptNumber <- 0 until N; i <- 0 until N) {
      betaMat(i, ptNumber) = splineBasis(ptNumber - i)
    }

    val c1 = (betaMat \ I)
    c1

  }*/

    /*
   * not used anymore
   *
  def determineCoefficientsWithLinearSystem[Pixel : ScalarPixel : ClassTag](degree: Int, img: DiscreteScalarImage2D[Pixel]): DenseVector[Double] = {

    val scalarPixel = implicitly[ScalarPixel[Pixel]]

    val N: Int = img.domain.numberOfPoints
    val splineBasis = (a: Double, b: Double) => bSpline(degree)(a) * bSpline(degree)(b)
    val I = DenseVector(img.pixelValues.toArray).map(scalarPixel.toDouble)

    val betaMat = DenseMatrix.zeros[Double](N, N)

    for (
      ptNumber <- 0 until N;
      j <- 0 until img.domain.size(1);
      i <- 0 until img.domain.size(0)
    ) {
      val coordvector2D = img.domain.linearIndexToIndex(ptNumber)
      betaMat(ptNumber, img.domain.indexToLinearIndex(CoordVector2D(i, j))) = splineBasis(coordvector2D(0) - i, coordvector2D(1) - j)
    }

    (betaMat \ I)

  }
  */
  
  
  def determineCoefficients[Pixel : ScalarPixel](degree: Int, img: DiscreteScalarImage1D[Pixel]): DenseVector[Double] = {
    val scalarPixel = implicitly[ScalarPixel[Pixel]]

    // the c is an input-output argument here
    val c = img.pixelValues.map(scalarPixel.toDouble).toArray
    BSplineCoefficients.getSplineInterpolationCoefficients(degree, c)
    DenseVector(c.map(_.toDouble))
  }


  def determineCoefficients[Pixel : ScalarPixel](degree: Int, img: DiscreteScalarImage2D[Pixel]): DenseVector[Double] = {
	val scalarPixel = implicitly[ScalarPixel[Pixel]]
    val coeffs = DenseVector.zeros[Double](img.pixelValues.size)
    for (y <- 0 until img.domain.size(1)) {
      val rowValues = (0 until img.domain.size(0)).map(x => img.pixelValues(img.domain.indexToLinearIndex(CoordVector2D(x, y))))

      // the c is an input-output argument here
      val c = rowValues.map(scalarPixel.toDouble).toArray
      BSplineCoefficients.getSplineInterpolationCoefficients(degree, c)

      val idxInCoeffs = img.domain.indexToLinearIndex(CoordVector2D(0, y))
      coeffs(idxInCoeffs until idxInCoeffs + img.domain.size(0)) := DenseVector(c)
      //coeffs(img.domain.indexToLinearIndex((0,y)) until img.domain.indexToLinearIndex((img.domain.size(0)-1,y))) := DenseVector(c.map(_.toDouble))
    }
    coeffs
  }

  def determineCoefficients[Pixel : ScalarPixel](degree: Int, img: DiscreteScalarImage3D[Pixel]): DenseVector[Double] = {
    val scalarPixel = implicitly[ScalarPixel[Pixel]]
    val coeffs = DenseVector.zeros[Double](img.pixelValues.size)
    for (z <- 0 until img.domain.size(2)) {
      for (y <- 0 until img.domain.size(1)) {
        val rowValues = (0 until img.domain.size(0)).map(x => img.pixelValues(img.domain.indexToLinearIndex(CoordVector3D(x, y, z))))

        // the c is an input-output argument here
        val c = rowValues.map(scalarPixel.toDouble).toArray
        BSplineCoefficients.getSplineInterpolationCoefficients(degree, c)
        val idxInCoeffs = img.domain.indexToLinearIndex(CoordVector3D(0, y, z))
        coeffs(idxInCoeffs until idxInCoeffs + img.domain.size(0)) := DenseVector(c)
      }
    }
    coeffs
  }

  def interpolate[Scalar : ScalarPixel](degree: Int)(image: DiscreteScalarImage1D[Scalar]): ContinuousScalarImage1D = {

    val ck = determineCoefficients(degree, image)

    def iterateOnPoints(x: CoordVector1D[Double], splineBasis: ((Double) => Double)): Double = {
      val xUnit = (x(0) - image.domain.origin(0)) / image.domain.spacing(0)

      val k1 = scala.math.max(scala.math.ceil(xUnit - 0.5f * (degree + 1)).toInt, 0)
      val K = scala.math.min(degree + 1, ck.size - 1)

      var result = 0.
      var k = k1
      val upperBound = scala.math.min(k1 + K - 1, ck.size - 1)
      while (k <= upperBound) {
        result = result + splineBasis(xUnit.toDouble - k) * ck(k)
        k = k + 1
      }
      result
    }

    def f(x: CoordVector1D[Double]) = {
        val splineBasis: (Double => Double) = bSpline(degree) // apply function
        iterateOnPoints(x, splineBasis)
      }
    def df(x: CoordVector1D[Double]) = { //derivative
        val splineBasisD1: (Double => Double) = { x => (bSpline(degree - 1)(x + 0.5f) - bSpline(degree - 1)(x - 0.5f)) * (1/image.domain.spacing(0)) }
        DenseVector(iterateOnPoints(x, splineBasisD1))
      }    
    new ContinuousScalarImage1D(image.domain.isInside,   f, Some(df))
  }

  def interpolate2D[Scalar : ScalarPixel](degree: Int)(image: DiscreteScalarImage2D[Scalar]): ContinuousScalarImage2D = {
    val ck = determineCoefficients(degree, image)

    def iterateOnPoints( x: CoordVector2D[Double], splineBasis: ((Double, Double) => Double)): Double = {
      val xUnit = ((x(0) - image.domain.origin(0)) / image.domain.spacing(0))
      val yUnit = ((x(1) - image.domain.origin(1)) / image.domain.spacing(1))

      val k1 = scala.math.max(scala.math.ceil(xUnit - 0.5f * (degree + 1)).toInt, 0)
      val l1 = scala.math.max(scala.math.ceil(yUnit - 0.5f * (degree + 1)).toInt, 0)

      val K = degree + 1

      var result = 0.
      var k = k1
      var l = l1
      val upperBound1 =  scala.math.min(l1 + K - 1, image.domain.size(1) - 1)
      val upperBound2 = scala.math.min(k1 + K - 1, image.domain.size(0) - 1)
      while (l <= upperBound1) {
        k = k1
        while (k <= upperBound2) {

          val idx = image.domain.indexToLinearIndex(CoordVector2D(k, l))
          result = result + ck(idx) * splineBasis(xUnit - k, yUnit - l)
          k = k + 1
        }
        l = l + 1
      }

      result
    }

    val bSplineNthOrder = bSpline(degree)_
    val bSplineNmin1thOrder = bSpline(degree - 1)_

    def f(x: CoordVector2D[Double]) = { 
        val splineBasis = (x: Double, y: Double) => bSplineNthOrder(x) * bSplineNthOrder(y) // apply function
        iterateOnPoints( x,  splineBasis)      
    }
    def df(x: CoordVector2D[Double]) = { //derivative
        val splineBasisD1 = (x: Double, y: Double) => (bSplineNmin1thOrder(x + 0.5f) - bSplineNmin1thOrder(x - 0.5f)) * bSplineNthOrder(y)
        val splineBasisD2 = (x: Double, y: Double) => bSplineNthOrder(x) * (bSplineNmin1thOrder(y + 0.5f) - bSplineNmin1thOrder(y - 0.5f))
        DenseVector(iterateOnPoints( x,  splineBasisD1)* (1/image.domain.spacing(0)), iterateOnPoints(x,splineBasisD2)* (1/image.domain.spacing(1)))
      }

    new ContinuousScalarImage2D(image.domain.isInside,f, Some(df))
  }

  def interpolate3D[Scalar : ScalarPixel](degree: Int)(image: DiscreteScalarImage3D[Scalar]): ContinuousScalarImage3D = {
    val ck = determineCoefficients(degree, image)

    def iterateOnPoints(x: CoordVector3D[Double], splineBasis: ((Double, Double, Double) => Double)): Double = {
      val xUnit = ((x(0) - image.domain.origin(0)) / image.domain.spacing(0))
      val yUnit = ((x(1) - image.domain.origin(1)) / image.domain.spacing(1))
      val zUnit = ((x(2) - image.domain.origin(2)) / image.domain.spacing(2))

      val k1 = scala.math.max(scala.math.ceil(xUnit - 0.5f * (degree + 1)).toInt, 0)
      val l1 = scala.math.max(scala.math.ceil(yUnit - 0.5f * (degree + 1)).toInt, 0)
      val m1 = scala.math.max(scala.math.ceil(zUnit - 0.5f * (degree + 1)).toInt, 0)

      val K = degree + 1

      var result = 0.
      var k = k1
      var l = l1
      var m = m1
      
      val upperBound1 =  scala.math.min(m1 + K - 1, image.domain.size(2) - 1)
      val upperBound2 =  scala.math.min(l1 + K - 1, image.domain.size(1) - 1)
      val upperBound3 =  scala.math.min(k1 + K - 1, image.domain.size(0) - 1)
      
      while (m <= upperBound1) {
        l = l1
        while (l <= upperBound2) {
          k = k1
          while (k <= upperBound3) {

            val idx = image.domain.indexToLinearIndex(CoordVector3D(k, l, m))
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
    val bSplineNmin1thOrder = bSpline(degree - 1)_

    def f(x: CoordVector3D[Double]) = { 
        val splineBasis = (x: Double, y: Double, z: Double) => bSplineNthOrder(x) * bSplineNthOrder(y) * bSplineNthOrder(z)
        iterateOnPoints(x, splineBasis)
    }
    def df(x: CoordVector3D[Double]) = { 
        val splineBasisD1 = (x: Double, y: Double, z: Double) => (bSplineNmin1thOrder(x + 0.5f) - bSplineNmin1thOrder(x - 0.5f)) * bSplineNthOrder(y) * bSplineNthOrder(z)
        val splineBasisD2 = (x: Double, y: Double, z: Double) => bSplineNthOrder(x) * (bSplineNmin1thOrder(y + 0.5f) - bSplineNmin1thOrder(y - 0.5f)) * bSplineNthOrder(z)
        val splineBasisD3 = (x: Double, y: Double, z: Double) => bSplineNthOrder(x) * bSplineNthOrder(y) * (bSplineNmin1thOrder(z + 0.5f) - bSplineNmin1thOrder(z - 0.5f))
        DenseVector(iterateOnPoints(x, splineBasisD1)* (1/image.domain.spacing(0)), iterateOnPoints(x, splineBasisD2)* (1/image.domain.spacing(1)), iterateOnPoints(x, splineBasisD3)* (1/image.domain.spacing(2)))      
    }
    new ContinuousScalarImage3D(image.domain.isInside, f, Some(df))
  }
  
  
//
//  def main(args: Array[String]) {
//    val a: Try[Int] = Failure(new IOException("abc"))
//    val b: Try[Int] = Success(5)
//    val f = Figure()
//    val p = f.subplot(0)
//    val xs = linspace(0, 30, 500).map(_.toDouble)
//    val range = (0 until 30).toIndexedSeq
//    val ps = DiscreteScalarImage1D(DiscreteImageDomain1D(0f, 1f, 30), range.map(Math.sin(_)))
//    val continuousImg = interpolate(3)(ps)
//
//    p += plot(xs, xs.map(x => continuousImg(x)))
//    p += plot(xs, xs.map(x => continuousImg.takeDerivative(x)(0)))
//  }
}

//  def determineCoefficients[Scalar <% Double: ClassTag](degree: Int, img: DiscreteScalarImage3D[Scalar]): DenseVector[Double] = {
//
//    val N: Int = img.domain.points.size
//    val splineBasis = (a: Double, b: Double, c: Double) => bSpline(degree)(a) * bSpline(degree)(b) * bSpline(degree)(c)
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
//    (betaMat \ I).map(_.toDouble)
//
//  }