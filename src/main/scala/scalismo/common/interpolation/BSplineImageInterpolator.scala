package scalismo.common.interpolation

import breeze.linalg.DenseVector
import scalismo.common._
import scalismo.geometry._
import scalismo.image.DiscreteImageDomain
import scalismo.numerics.BSpline

trait BSplineImageInterpolator[D, A]
    extends DifferentiableFieldInterpolator[D, DiscreteImageDomain, A, EuclideanVector[D]] {
  implicit protected val scalar: Scalar[A]

  private[scalismo] def applyMirrorBoundaryCondition(k: Int, numCoefficients: Int) = {
    if (k < 0) -k
    else if (k >= numCoefficients) numCoefficients - (k - numCoefficients) - 2
    else k
  }
}

object BSplineImageInterpolator {

  trait Create[D] {
    def createBSplineInterpolator[A: Scalar](degree: Int): BSplineImageInterpolator[D, A]
  }

  implicit object create1D extends Create[_1D] {
    override def createBSplineInterpolator[A: Scalar](degree: Int): BSplineImageInterpolator[_1D, A] =
      new BSplineImageInterpolator1D[A](degree)
  }

  implicit object create2D extends Create[_2D] {
    override def createBSplineInterpolator[A: Scalar](degree: Int): BSplineImageInterpolator[_2D, A] =
      new BSplineImageInterpolator2D[A](degree)
  }

  implicit object create3D extends Create[_3D] {
    override def createBSplineInterpolator[A: Scalar](degree: Int): BSplineImageInterpolator[_3D, A] =
      new BSplineImageInterpolator3D[A](degree)
  }

  def apply[D: NDSpace, A: Scalar](degree: Int)(implicit creator: Create[D]): BSplineImageInterpolator[D, A] = {
    creator.createBSplineInterpolator(degree)
  }

}

case class BSplineImageInterpolator1D[A: Scalar](degree: Int) extends BSplineImageInterpolator[_1D, A] {

  override protected val scalar: Scalar[A] = Scalar[A]

  override def interpolate(
    discreteField: DiscreteField[_1D, DiscreteImageDomain, A]
  ): DifferentiableField[_1D, A] = {

    val domain = discreteField.domain

    val ck = determineCoefficients1D(degree, discreteField)

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
        val kBC = applyMirrorBoundaryCondition(k, domain.size(0))
        result = result + splineBasis(xUnit.toDouble - k) * ck(kBC)
        k = k + 1
      }
      result
    }

    // the continuous interpolation function
    def f(x: Point[_1D]) = {
      val splineBasis: (Double => Double) = BSpline.nthOrderBSpline(degree)
      scalar.fromDouble(iterateOnPoints(x, splineBasis))
    }
    // the derivative
    def df(x: Point[_1D]) = {
      //derivative
      val splineBasisD1: (Double => Double) = { x =>
        (BSpline.nthOrderBSpline(degree - 1)(x + 0.5f) - BSpline.nthOrderBSpline(degree - 1)(x - 0.5f)) * (1 / domain
          .spacing(0))
      }
      EuclideanVector(iterateOnPoints(x, splineBasisD1))
    }

    DifferentiableField[_1D, A](domain.boundingBox, f, df)
  }

  /* determine the b-spline coefficients for a 1D image */
  private def determineCoefficients1D(degree: Int, img: DiscreteField[_1D, DiscreteImageDomain, A]): Array[Float] = {

    // floats is an input-output argument here
    val floats = new Array[Float](img.data.size)
    img.data.map((v: A) => scalar.toFloat(v)).copyToArray(floats)
    BSplineCoefficients.getSplineInterpolationCoefficients(degree, floats)
    floats
  }

}

case class BSplineImageInterpolator2D[A: Scalar](degree: Int) extends BSplineImageInterpolator[_2D, A] {

  override protected val scalar = Scalar[A]

  override def interpolate(
    discreteField: DiscreteField[_2D, DiscreteImageDomain, A]
  ): DifferentiableField[_2D, A] = {
    val domain = discreteField.domain
    val pointSet = domain.pointSet

    val ck = determineCoefficients2D(degree, discreteField)

    def iterateOnPoints(x: Point[_2D], splineBasis: ((Double, Double) => Double)): Double = {

      val xUnit = (x(0) - domain.origin(0)) / domain.spacing(0)
      val yUnit = (x(1) - domain.origin(1)) / domain.spacing(1)

      val k1 = scala.math.ceil(xUnit - 0.5f * (degree + 1)).toInt
      val l1 = scala.math.ceil(yUnit - 0.5f * (degree + 1)).toInt

      val K = degree + 1

      var result = 0.0
      var l = l1
      while (l <= l1 + K - 1) {
        val lBC = applyMirrorBoundaryCondition(l, domain.size(1))
        var k = k1
        while (k <= k1 + K - 1) {
          val kBC = applyMirrorBoundaryCondition(k, domain.size(0))
          val pointId = pointSet.pointId(IntVector(kBC, lBC))
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
      scalar.fromDouble(iterateOnPoints(x, splineBasis))
    }
    def df(x: Point[_2D]) = {
      val splineBasisD1 = (x: Double, y: Double) =>
        (bSplineNmin1thOrder(x + 0.5f) - bSplineNmin1thOrder(x - 0.5f)) * bSplineNthOrder(y)
      val splineBasisD2 = (x: Double, y: Double) =>
        bSplineNthOrder(x) * (bSplineNmin1thOrder(y + 0.5f) - bSplineNmin1thOrder(y - 0.5f))
      val dfx = (iterateOnPoints(x, splineBasisD1) * (1 / discreteField.domain.spacing(0))).toFloat
      val dfy = (iterateOnPoints(x, splineBasisD2) * (1 / discreteField.domain.spacing(1))).toFloat
      EuclideanVector(dfx, dfy)
    }

    DifferentiableField[_2D, A](discreteField.domain.boundingBox, f, df)
  }

  /* determine the b-spline coefficients for a 2D image. The coefficients are returned
   * as a DenseVector, i.e. the rows are written one after another */
  private def determineCoefficients2D(degree: Int, img: DiscreteField[_2D, DiscreteImageDomain, A]): Array[Float] = {
    val pointSet = img.domain.pointSet
    val numeric = implicitly[Scalar[A]]
    val coeffs = DenseVector.zeros[Float](img.values.size)
    var y = 0
    while (y < img.domain.size(1)) {
      val rowValues = (0 until img.domain.size(0)).map(x => img(pointSet.pointId(IntVector(x, y))))

      // the c is an input-output argument here
      val c = rowValues.map(numeric.toFloat).toArray
      BSplineCoefficients.getSplineInterpolationCoefficients(degree, c)

      val idxInCoeffs = pointSet.pointId(IntVector(0, y)).id
      coeffs(idxInCoeffs until idxInCoeffs + img.domain.size(0)) := DenseVector(c)
      y = y + 1
    }
    coeffs.data
  }
}

case class BSplineImageInterpolator3D[A: Scalar](degree: Int) extends BSplineImageInterpolator[_3D, A] {

  override protected val scalar = Scalar[A]

  override def interpolate(
    discreteField: DiscreteField[_3D, DiscreteImageDomain, A]
  ): DifferentiableField[_3D, A] = {
    val domain = discreteField.domain
    val pointSet = domain.pointSet

    val ck = determineCoefficients3D(degree, discreteField)
    val pointToIdx = pointSet.pointToContinuousIndex _

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
        val mBC = applyMirrorBoundaryCondition(m, domain.size(2))
        l = l1
        while (l <= l1 + K - 1) {
          val lBC = applyMirrorBoundaryCondition(l, domain.size(1))
          k = k1
          while (k <= k1 + K - 1) {
            val kBC = applyMirrorBoundaryCondition(k, domain.size(0))
            val pointId = pointSet.pointId(IntVector(kBC, lBC, mBC))
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

    def f(x: Point[_3D]): A = {
      val splineBasis = (x: Double, y: Double, z: Double) =>
        bSplineNthOrder(x) * bSplineNthOrder(y) * bSplineNthOrder(z)
      scalar.fromDouble(iterateOnPoints(x, splineBasis))
    }

    def df(x: Point[_3D]) = {
      val splineBasisD1 = (x: Double, y: Double, z: Double) =>
        (bSplineNmin1thOrder(x + 0.5f) - bSplineNmin1thOrder(x - 0.5f)) * bSplineNthOrder(y) * bSplineNthOrder(z)
      val splineBasisD2 = (x: Double, y: Double, z: Double) =>
        bSplineNthOrder(x) * (bSplineNmin1thOrder(y + 0.5f) - bSplineNmin1thOrder(y - 0.5f)) * bSplineNthOrder(z)
      val splineBasisD3 = (x: Double, y: Double, z: Double) =>
        bSplineNthOrder(x) * bSplineNthOrder(y) * (bSplineNmin1thOrder(z + 0.5f) - bSplineNmin1thOrder(z - 0.5f))
      val dfx = (iterateOnPoints(x, splineBasisD1) * (1 / domain.spacing(0))).toFloat
      val dfy = (iterateOnPoints(x, splineBasisD2) * (1 / domain.spacing(1))).toFloat
      val dfz = (iterateOnPoints(x, splineBasisD3) * (1 / domain.spacing(2))).toFloat
      EuclideanVector(dfx, dfy, dfz)
    }

    val bbox = domain.boundingBox
    DifferentiableField(BoxDomain3D(bbox.origin, bbox.oppositeCorner), f, df)
  }

  private def determineCoefficients3D(degree: Int,
                                      discreteField: DiscreteField[_3D, DiscreteImageDomain, A]): Array[Float] = {

    val pointSet = discreteField.domain.pointSet

    val coeffs = DenseVector.zeros[Float](discreteField.values.size)
    var z = 0
    var y = 0
    while (z < discreteField.domain.size(2)) {
      y = 0
      while (y < discreteField.domain.size(1)) {
        val rowValues = (0 until discreteField.domain.size(0))
          .map(x => discreteField.apply(pointSet.pointId(IntVector(x, y, z))))

        // the c is an input-output argument here
        val c = rowValues.map(scalar.toFloat).toArray
        BSplineCoefficients.getSplineInterpolationCoefficients(degree, c)
        val idxInCoeffs = pointSet.pointId(IntVector(0, y, z)).id
        coeffs(idxInCoeffs until idxInCoeffs + discreteField.domain.size(0)) := DenseVector(c)
        y = y + 1
      }
      z = z + 1
    }
    coeffs.data
  }
}
