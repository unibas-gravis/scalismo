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
import breeze.linalg.{diag, DenseMatrix, DenseVector}
import breeze.stats.distributions.Gaussian
import scalismo.common._
import scalismo.common.interpolation.FieldInterpolator
import scalismo.geometry.{EuclideanVector, NDSpace, Point}
import scalismo.kernels.{Kernel, MatrixValuedPDKernel}
import scalismo.numerics.PivotedCholesky.RelativeTolerance
import scalismo.numerics.{PivotedCholesky, Sampler}
import scalismo.transformations.RigidTransformation
import scalismo.statisticalmodel.LowRankGaussianProcess.{Eigenpair, KLBasis}
import scalismo.utils.{Memoize, Random}

import scala.language.higherKinds

/**
 *
 * A gaussian process which is represented in terms of a (small) finite set of basis functions.
 * The basis functions are the orthonormal basis functions given by a mercers' decomposition.
 *
 * @param mean    The mean function
 * @param klBasis A set of basis functions
 * @tparam D     The dimensionality of the input space
 * @tparam Value The output type
 */
class LowRankGaussianProcess[D: NDSpace, Value](mean: Field[D, Value], val klBasis: KLBasis[D, Value])(
  implicit
  vectorizer: Vectorizer[Value]
) extends GaussianProcess[D, Value](mean, LowRankGaussianProcess.covFromKLTBasis(klBasis)) {

  /**
   * the rank (i.e. number of basis functions)
   */
  def rank = klBasis.size

  /**
   * an instance of the gaussian process, which is formed by a linear combination of the klt basis using the given coefficients c.
   *
   * @param c Coefficients that determine the linear combination. Are assumed to be N(0,1) distributed.
   */
  def instance(c: DenseVector[Double]): Field[D, Value] = {
    require(klBasis.size == c.size)
    val f: Point[D] => Value = x => {
      val deformationsAtX = klBasis.indices.map(i => {
        val Eigenpair(lambda_i, phi_i) = klBasis(i)
        vectorizer.vectorize(phi_i(x)) * c(i) * math.sqrt(lambda_i)
      })
      val e = deformationsAtX.foldLeft(vectorizer.vectorize(mean(x)))(_ + _)
      vectorizer.unvectorize(e)
    }
    Field[D, Value](domain, f)
  }

  /**
   * A random sample of the gaussian process
   */
  def sample()(implicit rand: Random): Field[D, Value] = {
    val standardNormal = Gaussian(0, 1)(rand.breezeRandBasis)
    val coeffs = standardNormal.sample(klBasis.length)
    instance(DenseVector(coeffs.toArray))
  }

  /**
   * A random sample evaluated at the given points
   */
  override def sampleAtPoints[DDomain[DD] <: DiscreteDomain[DD]](
    domain: DDomain[D]
  )(implicit rand: Random): DiscreteField[D, DDomain, Value] = {
    // TODO check that points are part of the domain
    val aSample = sample()
    val values = domain.pointSet.points.map(pt => aSample(pt))
    DiscreteField[D, DDomain, Value](domain, values.toIndexedSeq)
  }

  /**
   * Returns a reduced rank model, using only the leading basis function of the Karhunen-loeve expansion.
   *
   * @param newRank: The rank of the new Gaussian process.
   */
  def truncate(newRank: Int): LowRankGaussianProcess[D, Value] = {
    new LowRankGaussianProcess(mean, klBasis.take(newRank))
  }

  /**
   * Returns the sample of the gaussian process that best explains the given training data. It is assumed that the training data (values)
   * are subject to 0 mean Gaussian noise
   *
   * @param trainingData Point/value pairs where that the sample should approximate.
   * @param sigma2       variance of a Gaussian noise that is assumed on every training point
   */
  def project(trainingData: IndexedSeq[(Point[D], Value)], sigma2: Double = 1e-6): Field[D, Value] = {
    val cov =
      MultivariateNormalDistribution(DenseVector.zeros[Double](outputDim), DenseMatrix.eye[Double](outputDim) * sigma2)
    val newtd = trainingData.map { case (pt, df) => (pt, df, cov) }
    project(newtd)
  }

  /**
   * Returns the sample of the gaussian process that best explains the given training data. It is assumed that the training data (values)
   * are subject to 0 mean gaussian noise
   *
   * @param trainingData Point/value pairs where that the sample should approximate, together with the variance of the noise model at each point.
   */
  def project(trainingData: IndexedSeq[(Point[D], Value, MultivariateNormalDistribution)]): Field[D, Value] = {
    val c = coefficients(trainingData)
    instance(c)
  }

  /**
   * Returns the sample of the coefficients of the sample that best explains the given training data. It is assumed that the training data (values)
   * are subject to 0 mean Gaussian noise
   */
  def coefficients(trainingData: IndexedSeq[(Point[D], Value, MultivariateNormalDistribution)]): DenseVector[Double] = {
    val (minv, qtL, yVec, mVec) = LowRankGaussianProcess.genericRegressionComputations(this, trainingData)
    val mean_coeffs = (minv * qtL) * (yVec - mVec)
    mean_coeffs
  }

  /**
   * Returns the sample of the coefficients of the sample that best explains the given training data. It is assumed that the training data (values)
   * are subject to 0 mean Gaussian noise
   */
  def coefficients(trainingData: IndexedSeq[(Point[D], Value)], sigma2: Double): DenseVector[Double] = {
    val cov =
      MultivariateNormalDistribution(DenseVector.zeros[Double](outputDim), DenseMatrix.eye[Double](outputDim) * sigma2)
    val newtd = trainingData.map { case (pt, df) => (pt, df, cov) }
    coefficients(newtd)
  }

  /**
   * Returns the probability density of the instance produced by the x coefficients
   */
  def pdf(coefficients: DenseVector[Double]): Double = {
    if (coefficients.size != rank)
      throw new Exception(s"invalid vector dimensionality (provided ${coefficients.size} should be $rank)")
    val mvnormal = MultivariateNormalDistribution(DenseVector.zeros[Double](rank), diag(DenseVector.ones[Double](rank)))
    mvnormal.pdf(coefficients)
  }

  /**
   * Returns the log of the probability density of the instance produced by the x coefficients.
   *
   * If you are interested in ordinal comparisons of PDFs, use this as it is numerically more stable
   */
  def logpdf(coefficients: DenseVector[Double]): Double = {
    if (coefficients.size != rank)
      throw new Exception(s"invalid vector dimensionality (provided ${coefficients.size} should be $rank)")
    val mvnormal = MultivariateNormalDistribution(DenseVector.zeros[Double](rank), diag(DenseVector.ones[Double](rank)))
    mvnormal.logpdf(coefficients)
  }

  override def posterior(trainingData: IndexedSeq[(Point[D], Value)],
                         sigma2: Double): LowRankGaussianProcess[D, Value] = {
    val cov =
      MultivariateNormalDistribution(DenseVector.zeros[Double](outputDim), DenseMatrix.eye[Double](outputDim) * sigma2)
    val newtd = trainingData.map { case (pt, df) => (pt, df, cov) }
    posterior(newtd)
  }

  override def posterior(
    trainingData: IndexedSeq[(Point[D], Value, MultivariateNormalDistribution)]
  ): LowRankGaussianProcess[D, Value] = {
    LowRankGaussianProcess.regression(this, trainingData)
  }

  /**
   * Discretize the gaussian process on the given points.
   */
  def discretize[DDomain[DD] <: DiscreteDomain[DD]](
    domain: DDomain[D]
  ): DiscreteLowRankGaussianProcess[D, DDomain, Value] = {
    DiscreteLowRankGaussianProcess[D, DDomain, Value](domain, this)
  }

}

/**
 * Factory methods for creating Low-rank gaussian processes, as well as generic algorithms to manipulate Gaussian processes.
 */
object LowRankGaussianProcess {

  case class Eigenpair[D, Value](eigenvalue: Double, eigenfunction: Field[D, Value])

  type KLBasis[D, Value] = Seq[Eigenpair[D, Value]]

  /**
   * Perform a low-rank approximation of the Gaussian process using the Nystrom method. The sample points used for the nystrom method
   * are sampled using the given sample.
   *
   * @param gp                The gaussian process to approximate
   * @param sampler           determines which points will be used as samples for the nystrom approximation.
   * @param numBasisFunctions The number of basis functions to approximate.
   */
  def approximateGPNystrom[D: NDSpace, Value](gp: GaussianProcess[D, Value],
                                              sampler: Sampler[D],
                                              numBasisFunctions: Int)(implicit vectorizer: Vectorizer[Value]) = {
    val kltBasis: KLBasis[D, Value] = Kernel.computeNystromApproximation[D, Value](gp.cov, sampler)
    new LowRankGaussianProcess[D, Value](gp.mean, kltBasis.take(numBasisFunctions))
  }

  @deprecated("the method has been renamed to approximateGPNystrom", "0.17")
  def approximateGP[D: NDSpace, Value](gp: GaussianProcess[D, Value],
                                       sampler: Sampler[D],
                                       numBasisFunctions: Int)(implicit vectorizer: Vectorizer[Value], rand: Random) = {
    approximateGPNystrom(gp, sampler, numBasisFunctions)
  }

  /**
   * Perform a low-rank approximation of the Gaussian process using the Nystrom method. The sample points used for the nystrom method
   * are sampled using the given sample.
   *
   * @param gp                The gaussian process to approximate
   * @param sampler           determines which points will be used as samples for the nystrom approximation.
   * @
   */
  def approximateGPNystrom[D: NDSpace, Value](
    gp: GaussianProcess[D, Value],
    sampler: Sampler[D]
  )(implicit vectorizer: Vectorizer[Value]) = {
    val kltBasis: KLBasis[D, Value] = Kernel.computeNystromApproximation[D, Value](gp.cov, sampler)
    new LowRankGaussianProcess[D, Value](gp.mean, kltBasis)
  }

  @deprecated("the method has been renamed to approximateGPNystrom", "0.17")
  def approximateGP[D: NDSpace, Value](gp: GaussianProcess[D, Value],
                                       sampler: Sampler[D])(implicit vectorizer: Vectorizer[Value], rand: Random) = {
    approximateGPNystrom(gp, sampler)
  }

  /**
   * Perform a low-rank approximation of the Gaussian process using a pivoted Cholesky approximation.
   * This approximation will automatically compute the required number of basis functions, to achieve a given
   * approximation quality.
   *
   * Note:
   * The number of basis functions that are generated for a fixed approximation quality depends very much on the
   * smoothness of the process. The more smooth the modelled functions are, the less basis functions are required.
   * For models which are highly non-smooth, this method may generate a lot of basis functions.
   *
   *
   * @param domain The domain on which the approximation is performed. This can, for example be the points of a mesh,
   *               an image domain (regular grid) or any other suitable domain. Note that the number of points in this
   *               domain influences the approximation accuracy. As the complexity of this method grows at most
   *               linearly with the number of points, efficient approximations can be computed for domains which contain
   *               millions of points.
   *
   * @param gp     The gaussian process to be approximated
   *
   * @param relativeTolerance  The relative tolerance defines the stopping criterion for the approximation. When we
   *                           choose this parameter as 0.01, the method will stop computing new basis functions, when
   *                           the variance represented by the approximated Gaussian Process is 99% of the total variance
   *                           of the original process. A value of 0 will generate new basis functions until all the variance
   *                           is approximated. Note that this will only terminate when the Gaussian process has finite rank.
   *
   * @param interpolator       An interpolator used to interpolate values that lie between the points of the domain
   *
   * @return       A low rank approximation of the Gaussian process
   */
  def approximateGPCholesky[D: NDSpace, DDomain[D] <: DiscreteDomain[D], Value](
    domain: DDomain[D],
    gp: GaussianProcess[D, Value],
    relativeTolerance: Double,
    interpolator: FieldInterpolator[D, DDomain, Value]
  )(implicit vectorizer: Vectorizer[Value]): LowRankGaussianProcess[D, Value] = {

    val (basis, scale) = PivotedCholesky.computeApproximateEig(kernel = gp.cov,
                                                               xs = domain.pointSet.points.toIndexedSeq,
                                                               stoppingCriterion = RelativeTolerance(relativeTolerance))

    // Assemble a discrete Gaussian process from the matrix given by the pivoted cholesky and use the interpolator
    // to interpolate it.
    val nBasisFunctions = basis.cols

    val klBasis: DiscreteLowRankGaussianProcess.KLBasis[D, DDomain, Value] = for (i <- 0 until nBasisFunctions) yield {
      val discreteEV = DiscreteField.createFromDenseVector[D, DDomain, Value](domain, basis(::, i))
      DiscreteLowRankGaussianProcess.Eigenpair(scale(i), discreteEV)
    }

    val mean = DiscreteField[D, DDomain, Value](domain, domain.pointSet.points.toIndexedSeq.map(p => gp.mean(p)))

    val dgp = DiscreteLowRankGaussianProcess[D, DDomain, Value](mean, klBasis)

    // interpolate to get a continuous GP
    dgp.interpolate(interpolator)

  }

  private def covFromKLTBasis[D: NDSpace, Value](
    klBasis: KLBasis[D, Value]
  )(implicit vectorizer: Vectorizer[Value]): MatrixValuedPDKernel[D] = {
    val dimOps = vectorizer.dim
    val cov: MatrixValuedPDKernel[D] = new MatrixValuedPDKernel[D] {
      override val domain = klBasis.headOption
        .map { case (Eigenpair(lambda, phi)) => phi.domain }
        .getOrElse(RealSpace[D])

      override def k(x: Point[D], y: Point[D]): DenseMatrix[Double] = {
        var outer = DenseMatrix.zeros[Double](dimOps, dimOps)
        for (Eigenpair(lambda_i, phi_i) <- klBasis) {
          // TODO: check if this is not too slow
          val px = vectorizer.vectorize(phi_i(x))
          val py = vectorizer.vectorize(phi_i(y))
          outer = outer + (px * py.t) * lambda_i
        }
        outer
      }

      override def outputDim = dimOps
    }
    cov
  }

  /**
   * * Performs a Gaussian process regression, where we assume that each training point (vector) is subject to  zero-mean noise with given variance.
   *
   * @param gp           The gaussian process
   * @param trainingData Point/value pairs where that the sample should approximate, together with an error model (the uncertainty) at each point.
   */
  def regression[D: NDSpace, Value](
    gp: LowRankGaussianProcess[D, Value],
    trainingData: IndexedSeq[(Point[D], Value, MultivariateNormalDistribution)]
  )(implicit vectorizer: Vectorizer[Value]): LowRankGaussianProcess[D, Value] = {
    val outputDim = gp.outputDim

    val (_Minv, _QtL, yVec, mVec) = genericRegressionComputations(gp, trainingData)
    val mean_coeffs = (_Minv * _QtL) * (yVec - mVec)

    val mean_p = gp.instance(mean_coeffs)

    val D = breeze.linalg.diag(DenseVector(gp.klBasis.map(basisPair => Math.sqrt(basisPair.eigenvalue)).toArray))
    val Sigma = D * _Minv * D
    val SVD(innerUDbl, innerD2, _) = breeze.linalg.svd(Sigma)
    val innerU = innerUDbl

    def phip(i: Int)(x: Point[D]): Value = {
      // should be phi_p but _ is treated as partial function
      val phisAtX = {
        val newPhisAtX = {
          val innerPhisAtx = DenseMatrix.zeros[Double](outputDim, gp.rank)

          for ((eigenPair, j) <- gp.klBasis.zipWithIndex) {
            val phi_j = eigenPair.eigenfunction
            innerPhisAtx(0 until outputDim, j) := vectorizer.vectorize(phi_j(x))
          }
          innerPhisAtx
        }
        newPhisAtX
      }
      val vec = phisAtX * innerU(::, i)
      vectorizer.unvectorize(vec)
    }

    val klBasis_p = for (i <- gp.klBasis.indices) yield {
      val phipi_memo = Memoize(phip(i), 1000)
      val newEf = Field(gp.domain, (x: Point[D]) => phipi_memo(x))
      val newEv = innerD2(i)
      Eigenpair(newEv, newEf)
    }
    new LowRankGaussianProcess[D, Value](mean_p, klBasis_p)
  }

  /*
   * Internal computations of the regression.
   */
  private def genericRegressionComputations[D: NDSpace, Value](
    gp: LowRankGaussianProcess[D, Value],
    trainingData: IndexedSeq[(Point[D], Value, MultivariateNormalDistribution)]
  )(implicit vectorizer: Vectorizer[Value]) = {

    val outputDim = gp.outputDim

    val (xs, ys, errorDistributions) = trainingData.unzip3

    val yVec = DiscreteField.vectorize[D, Value](ys)
    val meanValues = xs.map(gp.mean)
    val mVec = DiscreteField.vectorize[D, Value](meanValues)

    val Q = DenseMatrix.zeros[Double](trainingData.size * outputDim, gp.klBasis.size)
    for ((x_i, i) <- xs.zipWithIndex; (Eigenpair(lambda_j, phi_j), j) <- gp.klBasis.zipWithIndex) {
      // TODO: check if not too slow
      Q(i * outputDim until i * outputDim + outputDim, j) := vectorizer.vectorize(phi_j(x_i)) * math.sqrt(lambda_j)
    }

    // What we are actually computing here is the following:
    // L would be a block diagonal matrix, which contains on the diagonal the blocks that describes the uncertainty
    // for each point (a d x d) block. We then would compute Q.t * L. For efficiency reasons (L could be large but is sparse)
    // we avoid ever constructing the matrix L and do the multiplication by hand.
    val QtL = Q.t.copy
    assert(QtL.cols == errorDistributions.size * outputDim)
    assert(QtL.rows == gp.rank)
    for ((errDist, i) <- errorDistributions.zipWithIndex) {
      QtL(::, i * outputDim until (i + 1) * outputDim) := QtL(::, i * outputDim until (i + 1) * outputDim) * breeze.linalg
        .inv(errDist.cov)
    }

    val M = QtL * Q + DenseMatrix.eye[Double](gp.klBasis.size)
    val Minv = breeze.linalg.pinv(M)

    (Minv, QtL, yVec, mVec)
  }

  /**
   * perform a rigid transformation of the gaussian process, i.e. it is later defined on the transformed domain and its
   * vectors are transformed along the domain.
   */
  def transform[D: NDSpace](gp: LowRankGaussianProcess[D, EuclideanVector[D]], rigidTransform: RigidTransformation[D])(
    implicit
    vectorizer: Vectorizer[EuclideanVector[D]]
  ): LowRankGaussianProcess[D, EuclideanVector[D]] = {
    val invTransform = rigidTransform.inverse

    val newDomain = gp.domain.warp(rigidTransform)

    def newMean(pt: Point[D]): EuclideanVector[D] = {
      val ptOrigGp = invTransform(pt)
      rigidTransform(ptOrigGp + gp.mean(ptOrigGp)) - rigidTransform(ptOrigGp)
    }

    val newBasis = for (Eigenpair(ev, phi) <- gp.klBasis) yield {
      def newPhi(pt: Point[D]): EuclideanVector[D] = {
        val ptOrigGp = invTransform(pt)
        rigidTransform(ptOrigGp + phi(ptOrigGp)) - pt
      }
      val newBasis = Field(newDomain, newPhi _)
      Eigenpair(ev, newBasis)
    }

    new LowRankGaussianProcess[D, EuclideanVector[D]](Field(newDomain, newMean _), newBasis)
  }

}
