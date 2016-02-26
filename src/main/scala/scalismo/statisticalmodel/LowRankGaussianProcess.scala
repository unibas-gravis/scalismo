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
package scalismo.statisticalmodel

import breeze.linalg.svd.SVD
import breeze.linalg.{ diag, DenseMatrix, DenseVector }
import breeze.stats.distributions.Gaussian
import scalismo.common._
import scalismo.geometry.{ Dim, NDSpace, Point, SquareMatrix, Vector }
import scalismo.kernels.{ Kernel, MatrixValuedPDKernel }
import scalismo.numerics.Sampler
import scalismo.registration.RigidTransformation
import scalismo.statisticalmodel.LowRankGaussianProcess.{ Eigenpair, KLBasis }
import scalismo.utils.Memoize

/**
 *
 * A gaussian process which is represented in terms of a (small) finite set of basis functions.
 * The basis functions are the orthonormal basis functions given by a mercers' decomposition.
 *
 * @param mean The mean function
 * @param klBasis A set of basis functions
 * @tparam D The dimensionality of the input space
 * @tparam DO The dimensionality of the output space
 */
class LowRankGaussianProcess[D <: Dim: NDSpace, DO <: Dim: NDSpace](mean: VectorField[D, DO],
  val klBasis: KLBasis[D, DO])
    extends GaussianProcess[D, DO](mean, LowRankGaussianProcess.covFromKLTBasis(klBasis)) {

  /**
   * the rank (i.e. number of basis functions)
   */
  def rank = klBasis.size

  /**
   * an instance of the gaussian process, which is formed by a linear combination of the klt basis using the given coefficients c.
   * @param c Coefficients that determine the linear combination. Are assumed to be N(0,1) distributed.
   */
  def instance(c: DenseVector[Float]): VectorField[D, DO] = {
    require(klBasis.size == c.size)
    val f: Point[D] => Vector[DO] = x => {
      val deformationsAtX = klBasis.indices.map(i => {
        val Eigenpair(lambda_i, phi_i) = klBasis(i)
        phi_i(x) * c(i) * math.sqrt(lambda_i).toFloat
      })
      deformationsAtX.foldLeft(mean(x))(_ + _)
    }
    VectorField(domain, f)
  }

  /**
   * A random sample of the gaussian process
   */
  def sample: VectorField[D, DO] = {
    val coeffs = for (_ <- klBasis.indices) yield Gaussian(0, 1).draw().toFloat
    instance(DenseVector(coeffs.toArray))
  }

  /**
   * A random sample evaluated at the given points
   */
  override def sampleAtPoints(domain: DiscreteDomain[D]): DiscreteVectorField[D, DO] = {
    // TODO check that points are part of the domain
    val aSample = sample
    val values = domain.points.map(pt => aSample(pt))
    DiscreteVectorField(domain, values.toIndexedSeq)
  }

  /**
   * Returns the sample of the gaussian process that best explains the given training data. It is assumed that the training data (values)
   * are subject to 0 mean Gaussian noise
   *
   * @param trainingData Point/value pairs where that the sample should approximate.
   * @param sigma2 variance of a Gaussian noise that is assumed on every training point
   */
  def project(trainingData: IndexedSeq[(Point[D], Vector[DO])], sigma2: Double = 1e-6): VectorField[D, DO] = {
    val cov = NDimensionalNormalDistribution(Vector.zeros[DO], SquareMatrix.eye[DO] * sigma2)
    val newtd = trainingData.map { case (pt, df) => (pt, df, cov) }
    project(newtd)
  }

  /**
   * Returns the sample of the gaussian process that best explains the given training data. It is assumed that the training data (values)
   * are subject to 0 mean gaussian noise
   *
   * @param trainingData Point/value pairs where that the sample should approximate, together with the variance of the noise model at each point.
   */
  def project(trainingData: IndexedSeq[(Point[D], Vector[DO], NDimensionalNormalDistribution[DO])]): VectorField[D, DO] = {
    val c = coefficients(trainingData)
    instance(c)
  }

  /**
   * Returns the sample of the coefficients of the sample that best explains the given training data. It is assumed that the training data (values)
   * are subject to 0 mean Gaussian noise
   */
  def coefficients(trainingData: IndexedSeq[(Point[D], Vector[DO], NDimensionalNormalDistribution[DO])]): DenseVector[Float] =
    {
      val (minv, qtL, yVec, mVec) = LowRankGaussianProcess.genericRegressionComputations(this, trainingData)
      val mean_coeffs = (minv * qtL).map(_.toFloat) * (yVec - mVec)
      mean_coeffs
    }

  /**
   * Returns the sample of the coefficients of the sample that best explains the given training data. It is assumed that the training data (values)
   * are subject to 0 mean Gaussian noise
   */
  def coefficients(trainingData: IndexedSeq[(Point[D], Vector[DO])], sigma2: Double): DenseVector[Float] = {
    val cov = NDimensionalNormalDistribution(Vector.zeros[DO], SquareMatrix.eye[DO] * sigma2)
    val newtd = trainingData.map { case (pt, df) => (pt, df, cov) }
    coefficients(newtd)
  }

  /**
   * Returns the probability density of the instance produced by the x coefficients
   */
  def pdf(coefficients: DenseVector[Float]) = {
    if (coefficients.size != rank) throw new Exception(s"invalid vector dimensionality (provided ${coefficients.size} should be $rank)")
    val mvnormal = MultivariateNormalDistribution(DenseVector.zeros[Float](rank), diag(DenseVector.ones[Float](rank)))
    mvnormal.pdf(coefficients)
  }

  /**
   * Returns the log of the probability density of the instance produced by the x coefficients.
   *
   * If you are interested in ordinal comparisons of PDFs, use this as it is numerically more stable
   */
  def logpdf(coefficients: DenseVector[Float]) = {
    if (coefficients.size != rank) throw new Exception(s"invalid vector dimensionality (provided ${coefficients.size} should be $rank)")
    val mvnormal = MultivariateNormalDistribution(DenseVector.zeros[Float](rank), diag(DenseVector.ones[Float](rank)))
    mvnormal.logpdf(coefficients)
  }

  override def posterior(trainingData: IndexedSeq[(Point[D], Vector[DO])], sigma2: Double): LowRankGaussianProcess[D, DO] = {
    val cov = NDimensionalNormalDistribution(Vector.zeros[DO], SquareMatrix.eye[DO] * sigma2)
    val newtd = trainingData.map { case (pt, df) => (pt, df, cov) }
    posterior(newtd)
  }

  override def posterior(trainingData: IndexedSeq[(Point[D], Vector[DO], NDimensionalNormalDistribution[DO])]): LowRankGaussianProcess[D, DO] = {
    LowRankGaussianProcess.regression(this, trainingData)
  }

  /**
   * Discretize the gaussian process on the given points.
   */
  def discretize(domain: DiscreteDomain[D]): DiscreteLowRankGaussianProcess[D, DO] = {
    DiscreteLowRankGaussianProcess(domain, this)
  }

}

/**
 * Convenience class to speedup sampling from a LowRankGaussianProcess obtained by nearest neighbor interpolation of a DiscreteLowRankGaussianProcess
 *
 * */
private[scalismo] class NearestNeighbourInterpolatedLowRankGaussianProcess[D <: Dim: NDSpace, DO <: Dim: NDSpace](mean: VectorField[D, DO], klBasis: KLBasis[D, DO], discreteGP: DiscreteLowRankGaussianProcess[D, DO]) extends LowRankGaussianProcess[D, DO](mean, klBasis) {

  require(klBasis.size == discreteGP.rank)

  override def instance(c: DenseVector[Float]): VectorField[D, DO] = discreteGP.instance(c).interpolateNearestNeighbor

  // if all training data points belong to the interpolated discrete domain, then we compute a discrete posterior GP and interpolate it
  // this way the posterior will also remain very efficient when sampling.
  override def posterior(trainingData: IndexedSeq[(Point[D], Vector[DO], NDimensionalNormalDistribution[DO])]): LowRankGaussianProcess[D, DO] = {

    val allInDiscrete = trainingData.forall{case (pt,vc,nz) => discreteGP.domain.isDefinedAt(pt)}

    if(allInDiscrete){
      val discreteTD = trainingData.map {case (pt,vc,nz) => (discreteGP.domain.findClosestPoint(pt).id, vc, nz)}
      discreteGP.posterior(discreteTD).interpolateNearestNeighbor
    }
    else {
      LowRankGaussianProcess.regression(this, trainingData)
    }
  }

}

/**
 * Factory methods for creating Low-rank gaussian processes, as well as generic algorithms to manipulate Gaussian processes.
 */
object LowRankGaussianProcess {

  case class Eigenpair[D <: Dim, DO <: Dim](eigenvalue: Float, eigenfunction: VectorField[D, DO])
  type KLBasis[D <: Dim, DO <: Dim] = Seq[Eigenpair[D, DO]]

  /**
   * Perform a low-rank approximation of the Gaussian process using the Nystrom method. The sample points used for the nystrom method
   * are sampled using the given sample.
   * @param gp The gaussian process to approximate
   * @param sampler determines which points will be used as samples for the nystrom approximation.
   * @param numBasisFunctions The number of basis functions to approximate.
   */
  def approximateGP[D <: Dim: NDSpace, DO <: Dim: NDSpace](gp: GaussianProcess[D, DO],
    sampler: Sampler[D],
    numBasisFunctions: Int) = {
    val kltBasis = Kernel.computeNystromApproximation(gp.cov, numBasisFunctions, sampler)
    new LowRankGaussianProcess[D, DO](gp.mean, kltBasis)
  }

  private def covFromKLTBasis[D <: Dim: NDSpace, DO <: Dim: NDSpace](klBasis: KLBasis[D, DO]): MatrixValuedPDKernel[D, DO] = {
    val dimOps = implicitly[NDSpace[DO]]
    val cov: MatrixValuedPDKernel[D, DO] = new MatrixValuedPDKernel[D, DO] {
      override val domain = klBasis.headOption
        .map { case (Eigenpair(lambda, phi)) => phi.domain }.getOrElse(RealSpace[D])

      override def k(x: Point[D], y: Point[D]): SquareMatrix[DO] = {
        val ptDim = dimOps.dimensionality

        var outer = SquareMatrix.zeros[DO]
        for (Eigenpair(lambda_i, phi_i) <- klBasis) {
          outer = outer + (phi_i(x) outer phi_i(y)) * lambda_i
        }
        outer

      }
    }
    cov
  }

  /**
   * * Performs a Gaussian process regression, where we assume that each training point (vector) is subject to  zero-mean noise with given variance.
   *
   * @param gp  The gaussian process
   * @param trainingData Point/value pairs where that the sample should approximate, together with an error model (the uncertainty) at each point.
   */
  def regression[D <: Dim: NDSpace, DO <: Dim: NDSpace](gp: LowRankGaussianProcess[D, DO],
    trainingData: IndexedSeq[(Point[D], Vector[DO], NDimensionalNormalDistribution[DO])]): LowRankGaussianProcess[D, DO] = {
    val outputDim = implicitly[NDSpace[DO]].dimensionality

    val (_Minv, _QtL, yVec, mVec) = genericRegressionComputations(gp, trainingData)
    val mean_coeffs = (_Minv * _QtL).map(_.toFloat) * (yVec - mVec)

    val mean_p = gp.instance(mean_coeffs)

    val D = breeze.linalg.diag(DenseVector(gp.klBasis.map(basisPair => Math.sqrt(basisPair.eigenvalue)).toArray))
    val Sigma = D * _Minv * D
    val SVD(innerUDbl, innerD2, _) = breeze.linalg.svd(Sigma)
    val innerU = innerUDbl.map(_.toFloat)

    def phip(i: Int)(x: Point[D]): Vector[DO] = {
      // should be phi_p but _ is treated as partial function
      val phisAtX = {
        val newPhisAtX = {
          val innerPhisAtx = DenseMatrix.zeros[Float](outputDim, gp.rank)

          for ((eigenPair, j) <- gp.klBasis.zipWithIndex) {
            val phi_j = eigenPair.eigenfunction
            innerPhisAtx(0 until outputDim, j) := phi_j(x).toBreezeVector
          }
          innerPhisAtx
        }
        newPhisAtX
      }
      val vec = phisAtX * innerU(::, i)
      Vector[DO](vec.data)
    }

    val klBasis_p = for (i <- gp.klBasis.indices) yield {
      val phipi_memo = Memoize(phip(i), 1000)
      val newEf = (VectorField(gp.domain, (x: Point[D]) => phipi_memo(x)))
      val newEv = innerD2(i).toFloat
      Eigenpair(newEv, newEf)
    }
    new LowRankGaussianProcess[D, DO](mean_p, klBasis_p)
  }

  /*
  * Internal computations of the regression.
   */
  private def genericRegressionComputations[D <: Dim: NDSpace, DO <: Dim: NDSpace](gp: LowRankGaussianProcess[D, DO],
    trainingData: IndexedSeq[(Point[D], Vector[DO], NDimensionalNormalDistribution[DO])]) = {

    val outputDimensionality = implicitly[NDSpace[DO]].dimensionality

    val outputDim = outputDimensionality

    val dim = implicitly[NDSpace[DO]].dimensionality
    def flatten(v: IndexedSeq[Vector[DO]]) = DenseVector(v.flatten(_.toArray).toArray)

    val (xs, ys, errorDistributions) = trainingData.unzip3

    val yVec = flatten(ys)
    val meanValues = xs.map(gp.mean)
    val mVec = flatten(meanValues)

    val Q = DenseMatrix.zeros[Double](trainingData.size * dim, gp.klBasis.size)
    for ((x_i, i) <- xs.zipWithIndex; (Eigenpair(lambda_j, phi_j), j) <- gp.klBasis.zipWithIndex) {
      Q(i * dim until i * dim + dim, j) := phi_j(x_i).toBreezeVector.map(_.toDouble) * math.sqrt(lambda_j)
    }

    // What we are actually computing here is the following:
    // L would be a block diagonal matrix, which contains on the diagonal the blocks that describes the uncertainty
    // for each point (a d x d) block. We then would compute Q.t * L. For efficiency reasons (L could be large but is sparse)
    // we avoid ever constructing the matrix L and do the multiplication by hand.
    val QtL = Q.t.copy
    assert(QtL.cols == errorDistributions.size * dim)
    assert(QtL.rows == gp.rank)
    for ((errDist, i) <- errorDistributions.zipWithIndex) {
      QtL(::, i * dim until (i + 1) * dim) := QtL(::, i * dim until (i + 1) * dim) * breeze.linalg.inv(errDist.cov.toBreezeMatrix)
    }

    val M = QtL * Q + DenseMatrix.eye[Double](gp.klBasis.size)
    val Minv = breeze.linalg.pinv(M)

    (Minv, QtL, yVec, mVec)
  }

  /**
   * perform a rigid transformation of the gaussian process, i.e. it is later defined on the transformed domain and its
   * vectors are transformed along the domain.
   */
  def transform[D <: Dim: NDSpace](gp: LowRankGaussianProcess[D, D], rigidTransform: RigidTransformation[D]): LowRankGaussianProcess[D, D] = {
    val invTransform = rigidTransform.inverse

    val newDomain = gp.domain.warp(rigidTransform)

    def newMean(pt: Point[D]): Vector[D] = {
      val ptOrigGp = invTransform(pt)
      rigidTransform(ptOrigGp + gp.mean(ptOrigGp)) - rigidTransform(ptOrigGp)
    }

    val newBasis = for (Eigenpair(ev, phi) <- gp.klBasis) yield {
      def newPhi(pt: Point[D]): Vector[D] = {
        val ptOrigGp = invTransform(pt)
        rigidTransform(ptOrigGp + phi(ptOrigGp)) - pt
      }
      val newBasis = VectorField(newDomain, newPhi _)
      Eigenpair(ev, newBasis)
    }

    new LowRankGaussianProcess[D, D](VectorField(newDomain, newMean _), newBasis)
  }

}
