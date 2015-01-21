package org.statismo.stk.core.statisticalmodel

import breeze.linalg.svd.SVD
import org.statismo.stk.core.geometry._
import org.statismo.stk.core.kernels.{ MatrixValuedPDKernel, Kernel }
import org.statismo.stk.core.common.{FiniteDiscreteDomain, DiscreteDomain, Domain}
import org.statismo.stk.core.registration.Transformation
import org.statismo.stk.core.numerics.Sampler
import org.statismo.stk.core.mesh.kdtree.KDTreeMap
import breeze.linalg.{ *, Axis, DenseVector, DenseMatrix }
import breeze.stats.distributions.Gaussian
import breeze.stats.mean


class LowRankGaussianProcess[DI <: Dim: NDSpace, DO <: Dim : NDSpace](domain: Domain[DI],
                                                  mean: Point[DI] => Vector[DI],
                                                  val eigenPairs: IndexedSeq[(Float, Point[DI] => Vector[DI])])
  extends GaussianProcess[DI](domain, mean, LowRankGaussianProcess.covFromEigenpairs(eigenPairs)) {

  def rank = eigenPairs.size

  def instance(alpha: DenseVector[Float]): Point[DI] => Vector[DI] = {
    require(eigenPairs.size == alpha.size)
    x =>
      {
        val deformationsAtX = (0 until eigenPairs.size).map(i => {
          val (lambda_i, phi_i) = eigenPairs(i)
          phi_i(x) * alpha(i) * math.sqrt(lambda_i).toFloat
        })
        deformationsAtX.foldLeft(mean(x))(_ + _)
      }
  }

  def sample: Point[DI] => Vector[DI] = {
    val coeffs = for (_ <- 0 until eigenPairs.size) yield Gaussian(0, 1).draw().toFloat
    instance(DenseVector(coeffs.toArray))
  }



  override def sampleAtPoints(pts : Seq[Point[DI]]) : Seq[(Point[DI], Vector[DI])] = {
    val aSample = sample
    pts.map(pt => (pt, aSample(pt)))
  }

  def jacobian(p: DenseVector[Float]) = { x: Point[DI] =>
    val dim = x.dimensionality
    val J = DenseMatrix.zeros[Float](dim, eigenPairs.size)
    (0 until eigenPairs.size).map(i => {
      val (lambda_i, phi_i) = eigenPairs(i)
      J(::, i) := (phi_i(x) * math.sqrt(lambda_i).toFloat).toBreezeVector
    })
    J
  }

  def project(trainingData: IndexedSeq[(Point[DI], Vector[DI])], sigma2: Double = 1e-6): (Point[DI] => Vector[DI]) = {
    val newtd = trainingData.map { case (pt, df) => (pt, df, sigma2) }
    project(newtd)
  }

  def project(trainingData: IndexedSeq[(Point[DI], Vector[DI], Double)]): Point[DI] => Vector[DI] = {
    val c = coefficients(trainingData)
    instance(c)
  }

  /**
   * Compute the coefficients alpha, that represent the given trainingData u best under this gp (in the least squares sense)
   * e.g. \sum_i alpha_i \lambda_i \phi_i(x) = u(x)
   */
  def coefficients(trainingData: IndexedSeq[(Point[DI], Vector[DI], Double)]): DenseVector[Float] = ??? /*{
    val (minv, qtL, yVec, mVec) = GaussianProcess.genericRegressionComputations(this, trainingData)
    val mean_coeffs = (minv * qtL).map(_.toFloat) * (yVec - mVec)
    mean_coeffs
  }                                                                                                       */

  def coefficients(trainingData: IndexedSeq[(Point[DI], Vector[DI])], sigma2: Double): DenseVector[Float] = {
    val newtd = trainingData.map { case (pt, df) => (pt, df, sigma2) }
    coefficients(newtd)
  }
}

class LowRankGaussianProcess1D(
  domain: Domain[_1D],
  mean: Point[_1D] => Vector[_1D],
  eigenPairs: IndexedSeq[(Float, Point[_1D] => Vector[_1D])])
  extends LowRankGaussianProcess[_1D, _1D](domain, mean, eigenPairs) {}

class LowRankGaussianProcess2D(
  domain: Domain[_2D],
  mean: Point[_2D] => Vector[_2D],
  eigenPairs: IndexedSeq[(Float, Point[_2D] => Vector[_2D])])
  extends LowRankGaussianProcess[_2D, _2D](domain, mean, eigenPairs) {}

class LowRankGaussianProcess3D(
  domain: Domain[_3D],
  mean: Point[_3D] => Vector[_3D],
  eigenPairs: IndexedSeq[(Float, Point[_3D] => Vector[_3D])])
  extends LowRankGaussianProcess[_3D, _3D](domain, mean, eigenPairs) {}

object LowRankGaussianProcess {
  def createLowRankGaussianProcess[D <: Dim : NDSpace](
    domain: Domain[D],
    sampler: Sampler[D],
    mean: Point[D] => Vector[D],
    cov: MatrixValuedPDKernel[D, D],
    numBasisFunctions: Int) =
  {
    val eigenPairs = Kernel.computeNystromApproximation(cov, numBasisFunctions, sampler)
    new LowRankGaussianProcess[D, D](domain, mean, eigenPairs)
  }




  private def covFromEigenpairs[D <: Dim : NDSpace](eigenPairs : IndexedSeq[(Float, Point[D] => Vector[D])]) : MatrixValuedPDKernel[D, D]= {
    val dimOps = implicitly[NDSpace[D]]
    val cov: MatrixValuedPDKernel[D, D] = new MatrixValuedPDKernel[D, D] {
      def apply(x: Point[D], y: Point[D]): SquareMatrix[D] = {
        val ptDim = dimOps.dimensionality
        val phis = eigenPairs.map(_._2)

        var outer = SquareMatrix.zeros[D]
        for ((lambda_i, phi_i) <- eigenPairs) {
          outer = outer + (phi_i(x) outer phi_i(y)) * lambda_i
        }
        outer

      }
    }
    cov
  }


  /**
   * create a LowRankGaussianProcess using PCA
   * Currently, this is done by discretizing the transformations and computing the mean and the covariance
   * of the discrete transformations.
   * TODO: It should be explicitly enforced (using the type system) that the sampler is uniform
   * TODO: At some point this should be replaced by a functional PCA
   *
   * @param Domain the domain on which the GP will be defined
   * @param transformations
   * @param sampler A (preferably) uniform sampler from which the points are sampled
   *
   */
  def createLowRankGPFromTransformations[D <: Dim: NDSpace](domain: Domain[D], transformations: Seq[Transformation[D]], sampler: Sampler[D]): LowRankGaussianProcess[D, D] = {
    val dim = implicitly[NDSpace[D]].dimensionality

    val samplePts = sampler.sample.map(_._1)

    val kdTreeMap = KDTreeMap.fromSeq(samplePts.zipWithIndex.toIndexedSeq)

    def findClosestPoint(pt: Point[D]): (Point[D], Int) = {
      val nearestPtsAndIndices = (kdTreeMap.findNearest(pt, n = 1))
      nearestPtsAndIndices(0)
    }
    def findClosestPoints(pt: Point[D], n: Int): Seq[(Point[D], Int)] = {
      val nearestPtsAndIndices = (kdTreeMap.findNearest(pt, n))
      nearestPtsAndIndices
    }

    val n = transformations.size
    val p = samplePts.size

    // create the data matrix
    val X = DenseMatrix.zeros[Float](n, p * dim)
    for (p1 <- transformations.zipWithIndex.par; p2 <- samplePts.zipWithIndex) {
      val (t, i) = p1
      val (x, j) = p2
      val ux = t(x) - x
      X(i, j * dim until (j + 1) * dim) := ux.toBreezeVector.t
    }

    def demean(X: DenseMatrix[Float]): (DenseMatrix[Double], DenseVector[Double]) = {
      val X0 = X.map(_.toDouble) // will be the demeaned result matrix
      val m: DenseVector[Double] = mean(X0(::, *)).toDenseVector
      for (i <- 0 until X0.rows) {
        X0(i, ::) := X0(i, ::) - m.t
      }
      (X0, m)
    }

    val (x0, meanVec) = demean(X)
    val SVD(u, d2, vt) = breeze.linalg.svd(x0 * x0.t * (1.0 / (n - 1)))

    val D = d2.map(v => Math.sqrt(v))
    val Dinv = D.map(d => if (d > 1e-6) 1.0 / d else 0.0)

    // a Matrix with the eigenvectors
    val U = x0.t * vt.t * breeze.linalg.diag(Dinv) / Math.sqrt(n - 1)

    // to compensate for the numerical approximation using the sampled poitns
    val normFactor = sampler.volumeOfSampleRegion / sampler.numberOfPoints

    def interpolateAtPoint(x: Point[D], dataVec: DenseVector[Double]): Vector[D] = {
      val nNbrPts = Math.pow(2, dim).toInt
      val ptAndIds = findClosestPoints(x, nNbrPts)

      var v = Vector.zeros[D]
      var sumW = 0.0
      for ((pt, id) <- ptAndIds) {
        val w = 1.0 / math.max((pt - x).norm, 1e-5)
        v += Vector[D](dataVec(id * dim until (id + 1) * dim).map(_.toFloat).data) * w
        sumW += w
      }
      v * (1.0 / sumW)
    }
    def mu(x: Point[D]): Vector[D] = {
      interpolateAtPoint(x, meanVec)
    }
    def phi(i: Int)(x: Point[D]): Vector[D] = {
      interpolateAtPoint(x, U(::, i)) * Math.sqrt(1.0 / normFactor)
    }

    val lambdas = d2.toArray.toIndexedSeq.map(l => (l * normFactor).toFloat)
    val phis = (0 until n).map(i => phi(i) _)
    val eigenPairs = lambdas zip phis
    new LowRankGaussianProcess[D, D](domain, mu _, eigenPairs)
  }

}
