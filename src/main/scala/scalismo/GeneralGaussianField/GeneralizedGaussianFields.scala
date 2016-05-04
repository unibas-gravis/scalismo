//package scalismo.GeneralGaussianField.Adapted
//
//import java.io.File
//
//import breeze.linalg._
//import breeze.linalg.DenseVector
//import breeze.linalg.svd.SVD
//import breeze.stats.distributions.Gaussian
//import scalismo.statisticalmodel.MultivariateNormalDistribution
//import scalismo.GeneralGaussianField
//import scalismo.GeneralGaussianField.Adapted.DiscreteLowRankGaussianRandomField.{ DiscEigenpair, DiscKLBasis }
//import scalismo.GeneralGaussianField.Adapted.LowRankGaussianRandomRandomField.{ Eigenpair, KLBasis }
//import scalismo.GeneralGaussianField.{ ContinuousMatrixValuedKernel, DiscreteMatrixValuedKernel }
//import scalismo.common._
//import scalismo.geometry.{ Dim, NDSpace, Point, Vector, _3D }
//import scalismo.mesh.{ TriangleMesh, TriangleMesh3D }
//import scalismo.numerics.PivotedCholesky.NumberOfEigenfunctions
//import scalismo.numerics.{ PivotedCholesky, Sampler }
//import scalismo.utils.Memoize
//
//import scala.collection.immutable.IndexedSeq
//
//object Kernel {
//  def computeNystromApproximation[D <: Dim: NDSpace, Value](representer: Vectorizer[Value],
//    k: ContinuousMatrixValuedKernel[D],
//    numBasisFunctions: Int,
//    sampler: Sampler[D]): KLBasis[D, Value] = {
//
//    // procedure for the nystrom approximation as described in
//    // Gaussian Processes for machine Learning (Rasmussen and Williamson), Chapter 4, Page 99
//
//    val (ptsForNystrom, _) = sampler.sample.unzip
//
//    // depending on the sampler, it may happen that we did not sample all the points we wanted
//    val effectiveNumberOfPointsSampled = ptsForNystrom.size
//
//    val (uMat: DenseMatrix[Double], lambdaMat: DenseVector[Double]) = (DenseMatrix.eye[Double](10), DenseVector.zeros[Double](5)) //PivotedCholesky.computeApproximateEig(k, ptsForNystrom, 1.0, NumberOfEigenfunctions(numBasisFunctions))
//
//    val lambda = lambdaMat.map(lmbda => (lmbda / effectiveNumberOfPointsSampled.toDouble))
//    val numParams = (for (i <- (0 until lambda.size) if lambda(i) >= 1e-8) yield 1).size
//
//    val W = uMat(::, 0 until numParams) * math.sqrt(effectiveNumberOfPointsSampled) * pinv(diag(lambdaMat(0 until numParams)))
//
//    def computePhis(x: Point[D]): DenseMatrix[Double] = ContinuousMatrixValuedKernel.computeKernelVectorFor(x, ptsForNystrom, k) * W
//    val computePhisMemoized = Memoize(computePhis, 1000)
//
//    def phi(i: Int)(x: Point[D]) = {
//      val value = computePhisMemoized(x)
//      // extract the right entry for the i-th phi function
//      representer.unvectorize(value(::, i))
//    }
//
//    for (i <- 0 until numParams) yield {
//      Eigenpair(lambda(i), Field(k.domain, phi(i) _))
//    }
//  }
//
//}
//
///**
// * A gaussian process from a D dimensional input space, whose input values are points,
// * to a DO dimensional output space. The output space is a Euclidean vector space of dimensionality DO.
// *
// * @param representer The vectorizer of the output type
// * @param mean        The mean function
// * @param cov         The covariance function. Needs to be positive definite
// * @tparam D     The dimensionality of the input space
// * @tparam Value The output type
// */
//class GaussianRandomField[D <: Dim: NDSpace, Value] protected (val representer: Vectorizer[Value],
//    val mean: Field[D, Value],
//    val cov: GeneralGaussianField.ContinuousMatrixValuedKernel[D]) {
//
//  //  protected[this] val dimOps: NDSpace[DO] = implicitly[NDSpace[DO]]
//
//  private def outputDimensionality = representer.dim
//
//  val domain = Domain.intersection(mean.domain, cov.domain)
//
//  /**
//   *
//   * Sample values of the Gaussian process evaluated at the given points.
//   */
//  def sampleAtPoints(domain: DiscreteDomain[D]): DiscreteField[D, Value] = {
//    this.marginal(domain).sample
//  }
//
//  /**
//   * Compute the marginal distribution for the given points. The result is again a Gaussian process, whose domain
//   * is defined by the given points.
//   */
//  def marginal(domain: DiscreteDomain[D]): DiscreteGaussianRandomField[D, Value] = {
//    val meanField = new DiscreteField[D, Value](domain, domain.points.toIndexedSeq.map(pt => mean(pt)))
//    val pts = domain.points.toIndexedSeq
//    def newCov(i: PointId, j: PointId): DenseMatrix[Double] = {
//      cov(pts(i.id), pts(j.id))
//    }
//
//    val discreteCov = new GeneralGaussianField.DiscreteMatrixValuedKernel[D](domain, newCov, cov.outputDim)
//    new DiscreteGaussianRandomField(representer, meanField, discreteCov)
//  }
//
//  /**
//   * Compute the marginal distribution at a single point.
//   */
//  def marginal(pt: Point[D]) = MultivariateNormalDistribution(representer.vectorize(mean(pt)), cov(pt, pt))
//
//  /**
//   * The posterior distribution of the gaussian process, with respect to the given trainingData.
//   * It is computed using Gaussian process regression.
//   * We assume that the trainingData is subject to isotropic Gaussian noise with variance sigma2.
//   */
//  def posterior(trainingData: IndexedSeq[(Point[D], Value)], sigma2: Double): GaussianRandomField[D, Value] = {
//    val cov = MultivariateNormalDistribution(DenseVector.zeros[Double](outputDimensionality), DenseMatrix.eye[Double](outputDimensionality) * sigma2)
//    val fullTrainingData = trainingData.map { case (p, v) => (p, v, cov) }
//    GaussianRandomField.regression(this, fullTrainingData)
//  }
//
//  /**
//   * The posterior distribution of the gaussian process, with respect to the given trainingData.
//   * It is computed using Gaussian process regression.
//   */
//  def posterior(trainingData: IndexedSeq[(Point[D], Value, MultivariateNormalDistribution)]): GaussianRandomField[D, Value] = {
//    GaussianRandomField.regression(this, trainingData)
//  }
//}
//
///**
// * Factory methods for creating Gaussian processes
// */
//object GaussianRandomField {
//
//  /**
//   * Creates a new Gaussian process with given mean and covariance, which is defined on the given domain.
//   */
//  def apply[D <: Dim: NDSpace, Value](representer: Vectorizer[Value],
//    mean: Field[D, Value],
//    cov: GeneralGaussianField.ContinuousMatrixValuedKernel[D]) = {
//    new GaussianRandomField[D, Value](representer, mean, cov)
//  }
//
//  /**
//   * * Performs a Gaussian process regression, where we assume that each training point (vector) is subject to  zero-mean noise with given variance.
//   *
//   * @param gp           The gaussian process
//   * @param trainingData Point/value pairs where that the sample should approximate, together with an error model (the uncertainty) at each point.
//   */
//  def regression[D <: Dim: NDSpace, Value](gp: GaussianRandomField[D, Value],
//    trainingData: IndexedSeq[(Point[D], Value, MultivariateNormalDistribution)]): GaussianRandomField[D, Value] = {
//
//    val outputDimensionality = gp.outputDimensionality
//    val (xs, ys, errorDists) = trainingData.unzip3
//
//    val mVec = gp.representer.vectorize(xs.map(gp.mean))
//    val yVec = gp.representer.vectorize(ys)
//    val fVec = yVec - mVec
//
//    val K = GeneralGaussianField.ContinuousMatrixValuedKernel.computeKernelMatrix(xs, gp.cov)
//    for ((errorDist, i) <- errorDists.zipWithIndex) {
//      K(i * outputDimensionality until (i + 1) * outputDimensionality, i * outputDimensionality until (i + 1) * outputDimensionality) += errorDist.cov
//    }
//
//    val K_inv = breeze.linalg.inv(K)
//
//    def xstar(x: Point[D]) = {
//      GeneralGaussianField.ContinuousMatrixValuedKernel.computeKernelVectorFor[D](x, xs, gp.cov)
//    }
//
//    def posteriorMean(x: Point[D]): Value = {
//      gp.representer.unvectorize((xstar(x) * K_inv) * fVec)
//    }
//
//    val posteriorKernel = new GeneralGaussianField.ContinuousMatrixValuedKernel[D](
//      gp.domain,
//      (x: Point[D], y: Point[D]) => {
//        gp.cov(x, y) - (xstar(x) * K_inv * xstar(y).t)
//      },
//      outputDimensionality
//    )
//
//    new GaussianRandomField[D, Value](gp.representer, Field[D, Value](gp.domain, posteriorMean _), posteriorKernel)
//  }
//
//  /**
//   * * Computes the marginal likelihood of the observed data, according to the given GP.
//   *
//   * This can for example be used in a model selection setting, where the GP with the maximum marginal likelihood of the observed data would be selected.
//   *
//   * @param gp           The gaussian process
//   * @param trainingData Point/value pairs where that the sample should approximate, together with an error model (the uncertainty) at each point.
//   * @todo The current implementation can be optimized as it inverts the data covariance matrix (that can be heavy for more than a few points). Instead an implementation
//   *       with a Cholesky decomposition would be more efficient.
//   */
//  def marginalLikelihood[D <: Dim: NDSpace, Value](gp: GaussianRandomField[D, Value],
//    trainingData: IndexedSeq[(Point[D], Value, MultivariateNormalDistribution)]): Double = {
//
//    val outputDim = gp.outputDimensionality
//
//    // below is the implementation according to Rassmussen Gaussian Processes, Chapter 5, page 113
//
//    val (ptIds, ys, errorDistributions) = trainingData.unzip3
//
//    val yVec = gp.representer.vectorize(ys)
//
//    val Ky = DenseMatrix.zeros[Double](trainingData.size * outputDim, trainingData.size * outputDim)
//
//    for ((ptIdI, i) <- ptIds.zipWithIndex; (ptIdJ, j) <- ptIds.zipWithIndex) {
//
//      val covBlock = gp.cov(ptIdI, ptIdJ)
//
//      val Kyyp = if (i == j) {
//        // in this case add observation uncertainty
//        val noiseBlock = errorDistributions(i).cov
//        covBlock + noiseBlock
//      } else covBlock
//
//      // insert the block in the big covariance matrix
//      for (l <- (0 until outputDim); m <- (0 until outputDim)) {
//        Ky(l + (i * outputDim), m + (j * outputDim)) = Kyyp(l, m)
//      }
//    }
//
//    val KyInv = inv(Ky)
//    val const = trainingData.length * 0.5 * math.log(math.Pi * 2)
//    val margLikehood = ((yVec.t * KyInv * yVec) * -0.5f) - (0.5f * math.log(det(Ky))) - const
//    margLikehood
//  }
//
//}
//
///**
// * A representation of a gaussian process, which is only defined on a discrete domain.
// * While this is technically similar to a MultivariateNormalDistribution, we highlight with this
// * class that we represent (discrete) functions, defined on the given domain.
// */
//class DiscreteGaussianRandomField[D <: Dim: NDSpace, Value] private[scalismo] (val representer: Vectorizer[Value],
//    val mean: DiscreteField[D, Value],
//    val cov: GeneralGaussianField.DiscreteMatrixValuedKernel[D]) {
//  self =>
//
//  require(mean.domain == cov.domain)
//
//  val domain = mean.domain
//
//  val outputDimensionality = representer.dim
//
//  val meanVector = representer.vectorize(mean.data)
//
//  def sample: DiscreteField[D, Value] = {
//    // define the mean and kernel matrix for the given points and construct the
//    // corresponding MV Normal distribution, from which we then sample
//
//    val d = outputDimensionality
//
//    val mu = representer.vectorize(mean.data)
//    val K = cov.asBreezeMatrix
//
//    val mvNormal = MultivariateNormalDistribution(mu, K)
//
//    val sampleVec = mvNormal.sample()
//
//    DiscreteField[D, Value](domain, representer.unvectorizeField(sampleVec))
//  }
//
//  /**
//   * The marginal distribution at a given (single) point, specified by the pointId.
//   */
//  def marginal(pointId: PointId) = {
//    MultivariateNormalDistribution(representer.vectorize(mean(pointId)), cov(pointId, pointId))
//  }
//
//  /**
//   * The marginal distribution for the points specified by the given point ids.
//   * Note that this is again a DiscreteGaussianProcess.
//   */
//  def marginal(pointIds: Seq[PointId])(implicit domainCreator: CreateUnstructuredPointsDomain[D]): DiscreteGaussianRandomField[D, Value] = {
//    val domainPts = domain.points.toIndexedSeq
//
//    val newPts = pointIds.map(pointId => domainPts(pointId.id)).toIndexedSeq
//    val newDomain = domainCreator.create(newPts)
//
//    val newMean = new DiscreteField[D, Value](newDomain, pointIds.toIndexedSeq.map(id => mean(id)))
//    val newCov = (i: PointId, j: PointId) => {
//      cov(i, j)
//    }
//    val newDiscreteCov = new GeneralGaussianField.DiscreteMatrixValuedKernel(newDomain, newCov, cov.outputDim)
//
//    new DiscreteGaussianRandomField(representer, newMean, newDiscreteCov)
//  }
//
//  /**
//   * Interpolates discrete Gaussian process to have a new, continuous representation as a [[DiscreteLowRankGaussianRandomField]],
//   * using nearest neighbor interpolation (for both mean and covariance function)
//   */
//  def interpolateNearestNeighbor: GaussianRandomField[D, Value] = {
//
//    val meanDiscreteGp = this.mean
//
//    val newDomain = RealSpace[D]
//    def meanFun: (Point[D]) => Value = (pt: Point[D]) => {
//      val closestPtId = domain.findClosestPoint(pt).id
//      meanDiscreteGp(closestPtId)
//    }
//
//    val newCov = new GeneralGaussianField.ContinuousMatrixValuedKernel[D](
//      newDomain,
//      (pt1: Point[D], pt2: Point[D]) => {
//        val closestPtId1 = self.domain.findClosestPoint(pt1).id
//        val closestPtId2 = self.domain.findClosestPoint(pt2).id
//        cov(closestPtId1, closestPtId2)
//      },
//      outputDimensionality)
//    GaussianRandomField(representer, Field[D, Value](newDomain, meanFun), newCov)
//  }
//
//  /**
//   * Discrete version of [[LowRankGaussianRandomRandomField.project(IndexedSeq[(Point[D], Value)], Double)]]
//   */
//
//  def project(s: DiscreteField[D, Value]): DiscreteField[D, Value] = {
//
//    val sigma2 = 1e-5 // regularization weight to avoid numerical problems
//    val noiseDist = MultivariateNormalDistribution(DenseVector.zeros[Double](representer.dim),
//      DenseMatrix.eye[Double](representer.dim) * sigma2)
//    val td = s.values.zipWithIndex.map {
//      case (v, id) => (id, v, noiseDist)
//    }.toIndexedSeq
//    DiscreteGaussianRandomField.regression(this, td).mean
//
//  }
//
//  /**
//   * Returns the probability density of the given instance
//   */
//  def pdf(instance: DiscreteField[D, Value]): Double = {
//    val mvnormal = MultivariateNormalDistribution(representer.vectorize(mean.data), cov.asBreezeMatrix)
//    val instvec = representer.vectorize(instance.data)
//    mvnormal.pdf(instvec)
//  }
//
//  /**
//   * Returns the log of the probability density of the given instance
//   *
//   * If you are interested in ordinal comparisons of PDFs, use this as it is numerically more stable
//   */
//  def logpdf(instance: DiscreteField[D, Value]): Double = {
//    val mvnormal = MultivariateNormalDistribution(representer.vectorize(mean.data), cov.asBreezeMatrix)
//    val instvec = representer.vectorize(instance.data)
//    mvnormal.logpdf(instvec)
//  }
//
//}
//
//object DiscreteGaussianRandomField {
//
//  def apply[D <: Dim: NDSpace, Value](representer: Vectorizer[Value],
//    mean: DiscreteField[D, Value],
//    cov: GeneralGaussianField.DiscreteMatrixValuedKernel[D]) = {
//    new DiscreteGaussianRandomField[D, Value](representer, mean, cov)
//  }
//
//  def apply[D <: Dim: NDSpace, Value](representer: Vectorizer[Value],
//    domain: DiscreteDomain[D],
//    gp: GaussianRandomField[D, Value]) = {
//    val domainPoints = domain.points.toIndexedSeq
//
//    val discreteMean = new DiscreteField[D, Value](domain, domainPoints.map(pt => gp.mean(pt)))
//
//    val k = (i: PointId, j: PointId) => gp.cov(domain.point(i), domain.point(j))
//    val discreteCov = new GeneralGaussianField.DiscreteMatrixValuedKernel(domain, k, representer.dim)
//
//    new DiscreteGaussianRandomField[D, Value](representer, discreteMean, discreteCov)
//  }
//
//  def regression[D <: Dim: NDSpace, Value](discreteGp: DiscreteGaussianRandomField[D, Value],
//    trainingData: IndexedSeq[(Int, Value, MultivariateNormalDistribution)]): DiscreteGaussianRandomField[D, Value] = {
//
//    // TODO, this is somehow a hack to reuse the code written for the general GP regression. We should think if that has disadvantages
//    // TODO We should think whether we can do it in  a conceptually more clean way.
//
//    val domainPoints = discreteGp.domain.points.toIndexedSeq
//    val gp = discreteGp.interpolateNearestNeighbor
//    val tdForGp = trainingData.map({ case (id, vec, error) => (domainPoints(id), vec, error) })
//    val posterior = GaussianRandomField.regression(gp, tdForGp)
//
//    DiscreteGaussianRandomField(discreteGp.representer, discreteGp.domain, gp)
//  }
//}
//
///**
// *
// * A gaussian process which is represented in terms of a (small) finite set of basis functions.
// * The basis functions are the orthonormal basis functions given by a mercers' decomposition.
// *
// * @param mean    The mean function
// * @param klBasis A set of basis functions
// * @tparam D     The dimensionality of the input space
// * @tparam Value The type of the output element
// */
//class LowRankGaussianRandomRandomField[D <: Dim: NDSpace, Value](representer: Vectorizer[Value],
//  mean: Field[D, Value],
//  val klBasis: KLBasis[D, Value])
//    extends GaussianRandomField[D, Value](representer, mean, LowRankGaussianRandomRandomField.covFromKLTBasis(representer, klBasis)) {
//
//  /**
//   * the rank (i.e. number of basis functions)
//   */
//  def rank = klBasis.size
//
//  val outputDimension = representer.dim
//
//  /**
//   * an instance of the gaussian process, which is formed by a linear combination of the klt basis using the given coefficients c.
//   *
//   * @param c Coefficients that determine the linear combination. Are assumed to be N(0,1) distributed.
//   */
//  def instance(c: DenseVector[Double]): Field[D, Value] = {
//    require(klBasis.size == c.size)
//    val f: Point[D] => Value = x => {
//      val deformationsAtX = klBasis.indices.map(i => {
//        val Eigenpair(lambda_i: Double, phi_i: Field[D, Value]) = klBasis(i)
//        representer.vectorize(phi_i(x)) * c(i) * math.sqrt(lambda_i)
//      })
//      representer.unvectorize(deformationsAtX.foldLeft(representer.vectorize(mean(x)))(_ + _))
//    }
//    Field(domain, f)
//  }
//
//  /**
//   * A random sample of the gaussian process
//   */
//  def sample: Field[D, Value] = {
//    val coeffs = for (_ <- klBasis.indices) yield Gaussian(0, 1).draw()
//    instance(DenseVector(coeffs.toArray))
//  }
//
//  /**
//   * A random sample evaluated at the given points
//   */
//  override def sampleAtPoints(domain: DiscreteDomain[D]): DiscreteField[D, Value] = {
//    // TODO check that points are part of the domain
//    val aSample = sample
//    val values = domain.points.map(pt => aSample(pt))
//    new DiscreteField[D, Value](domain, values.toIndexedSeq)
//  }
//
//  /**
//   * Returns the sample of the gaussian process that best explains the given training data. It is assumed that the training data (values)
//   * are subject to 0 mean Gaussian noise
//   *
//   * @param trainingData Point/value pairs where that the sample should approximate.
//   * @param sigma2       variance of a Gaussian noise that is assumed on every training point
//   */
//  def project(trainingData: IndexedSeq[(Point[D], Value)], sigma2: Double = 1e-6): Field[D, Value] = {
//    val cov = MultivariateNormalDistribution(DenseVector.zeros[Double](outputDimension), DenseMatrix.eye[Double](outputDimension) * sigma2)
//    val newtd = trainingData.map { case (pt, df) => (pt, df, cov) }
//    project(newtd)
//  }
//
//  /**
//   * Returns the sample of the gaussian process that best explains the given training data. It is assumed that the training data (values)
//   * are subject to 0 mean gaussian noise
//   *
//   * @param trainingData Point/value pairs where that the sample should approximate, together with the variance of the noise model at each point.
//   */
//  def project(trainingData: IndexedSeq[(Point[D], Value, MultivariateNormalDistribution)]): Field[D, Value] = {
//    val c = coefficients(trainingData)
//    instance(c)
//  }
//
//  /**
//   * Returns the sample of the coefficients of the sample that best explains the given training data. It is assumed that the training data (values)
//   * are subject to 0 mean Gaussian noise
//   */
//  def coefficients(trainingData: IndexedSeq[(Point[D], Value, MultivariateNormalDistribution)]): DenseVector[Double] = {
//    val (minv, qtL, yVec, mVec) = LowRankGaussianRandomRandomField.genericRegressionComputations(this, trainingData)
//    val mean_coeffs = (minv * qtL) * (yVec - mVec)
//    mean_coeffs
//  }
//
//  /**
//   * Returns the sample of the coefficients of the sample that best explains the given training data. It is assumed that the training data (values)
//   * are subject to 0 mean Gaussian noise
//   */
//  def coefficients(trainingData: IndexedSeq[(Point[D], Value)], sigma2: Double): DenseVector[Double] = {
//    val cov = MultivariateNormalDistribution(DenseVector.zeros[Double](outputDimension), DenseMatrix.eye[Double](outputDimension) * sigma2)
//    val newtd = trainingData.map { case (pt, df) => (pt, df, cov) }
//    coefficients(newtd)
//  }
//
//  /**
//   * Returns the probability density of the instance produced by the x coefficients
//   */
//  def pdf(coefficients: DenseVector[Double]): Double = {
//    if (coefficients.size != rank) throw new Exception(s"invalid vector dimensionality (provided ${coefficients.size} should be $rank)")
//    val mvnormal = MultivariateNormalDistribution(DenseVector.zeros[Double](rank), diag(DenseVector.ones[Double](rank)))
//    mvnormal.pdf(coefficients)
//  }
//
//  /**
//   * Returns the log of the probability density of the instance produced by the x coefficients.
//   *
//   * If you are interested in ordinal comparisons of PDFs, use this as it is numerically more stable
//   */
//  def logpdf(coefficients: DenseVector[Double]): Double = {
//    if (coefficients.size != rank) throw new Exception(s"invalid vector dimensionality (provided ${coefficients.size} should be $rank)")
//    val mvnormal = MultivariateNormalDistribution(DenseVector.zeros[Double](rank), diag(DenseVector.ones[Double](rank)))
//    mvnormal.logpdf(coefficients)
//  }
//
//  override def posterior(trainingData: IndexedSeq[(Point[D], Value)], sigma2: Double): LowRankGaussianRandomRandomField[D, Value] = {
//    val cov = MultivariateNormalDistribution(DenseVector.zeros[Double](outputDimension), DenseMatrix.eye[Double](outputDimension) * sigma2)
//    val newtd = trainingData.map { case (pt, df) => (pt, df, cov) }
//    posterior(newtd)
//  }
//
//  override def posterior(trainingData: IndexedSeq[(Point[D], Value, MultivariateNormalDistribution)]): LowRankGaussianRandomRandomField[D, Value] = {
//    LowRankGaussianRandomRandomField.regression(this, trainingData)
//  }
//
//  /**
//   * Discretize the gaussian process on the given points.
//   */
//  def discretize(domain: DiscreteDomain[D]): DiscreteLowRankGaussianRandomField[D, Value] = {
//    DiscreteLowRankGaussianRandomField(domain, this)
//  }
//
//}
//
///**
// * Factory methods for creating Low-rank gaussian processes, as well as generic algorithms to manipulate Gaussian processes.
// */
//object LowRankGaussianRandomRandomField {
//
//  case class Eigenpair[D <: Dim, Value](eigenvalue: Double, eigenfunction: Field[D, Value])
//
//  type KLBasis[D <: Dim, Value] = Seq[Eigenpair[D, Value]]
//
//  /**
//   * Perform a low-rank approximation of the Gaussian process using the Nystrom method. The sample points used for the nystrom method
//   * are sampled using the given sample.
//   *
//   * @param gp                The gaussian process to approximate
//   * @param sampler           determines which points will be used as samples for the nystrom approximation.
//   * @param numBasisFunctions The number of basis functions to approximate.
//   */
//  def approximateGP[D <: Dim: NDSpace, Value](gp: GaussianRandomField[D, Value],
//    sampler: Sampler[D],
//    numBasisFunctions: Int) = {
//    val kltBasis = Kernel.computeNystromApproximation[D, Value](gp.representer, gp.cov, numBasisFunctions, sampler)
//    new LowRankGaussianRandomRandomField[D, Value](gp.representer, gp.mean, kltBasis)
//  }
//
//  private def covFromKLTBasis[D <: Dim: NDSpace, Value](representer: Vectorizer[Value],
//    klBasis: KLBasis[D, Value]): GeneralGaussianField.ContinuousMatrixValuedKernel[D] = {
//    val cov: GeneralGaussianField.ContinuousMatrixValuedKernel[D] = {
//      val outputDim = 3
//      val domain = klBasis.headOption.map { case (Eigenpair(lambda, phi)) => phi.domain }.getOrElse(RealSpace[D])
//
//      def k(x: Point[D], y: Point[D]): DenseMatrix[Double] = {
//        val ptDim = outputDim
//
//        var outer = DenseMatrix.zeros[Double](outputDim, outputDim)
//        for (Eigenpair(lambda_i, phi_i) <- klBasis) {
//          val v = representer.vectorize(phi_i(x))
//          outer = outer + (representer.vectorize(phi_i(x)) * representer.vectorize(phi_i(y)).t) * lambda_i
//        }
//        outer
//
//      }
//      new GeneralGaussianField.ContinuousMatrixValuedKernel[D](domain, k, outputDim)
//    }
//    cov
//  }
//
//  /**
//   * * Performs a Gaussian process regression, where we assume that each training point (vector) is subject to  zero-mean noise with given variance.
//   *
//   * @param gp           The gaussian process
//   * @param trainingData Point/value pairs where that the sample should approximate, together with an error model (the uncertainty) at each point.
//   */
//  def regression[D <: Dim: NDSpace, Value](gp: LowRankGaussianRandomRandomField[D, Value],
//    trainingData: IndexedSeq[(Point[D], Value, MultivariateNormalDistribution)]): LowRankGaussianRandomRandomField[D, Value] = {
//    val outputDim = gp.outputDimension
//    val (_Minv, _QtL, yVec, mVec) = genericRegressionComputations(gp, trainingData)
//    val mean_coeffs = (_Minv * _QtL) * (yVec - mVec)
//
//    val mean_p = gp.instance(mean_coeffs)
//
//    val D = breeze.linalg.diag(DenseVector(gp.klBasis.map(basisPair => Math.sqrt(basisPair.eigenvalue)).toArray))
//    val Sigma = D * _Minv * D
//    val SVD(innerUDbl, innerD2, _) = breeze.linalg.svd(Sigma)
//    val innerU = innerUDbl
//
//    def phip(i: Int)(x: Point[D]): Value = {
//      // should be phi_p but _ is treated as partial function
//      val phisAtX = {
//        val newPhisAtX = {
//          val innerPhisAtx = DenseMatrix.zeros[Double](outputDim, gp.rank)
//
//          for ((eigenPair, j) <- gp.klBasis.zipWithIndex) {
//            val phi_j = eigenPair.eigenfunction
//            innerPhisAtx(0 until outputDim, j) := gp.representer.vectorize(phi_j(x))
//          }
//          innerPhisAtx
//        }
//        newPhisAtX
//      }
//      val vec = phisAtX * innerU(::, i)
//      gp.representer.unvectorize(vec)
//    }
//
//    val klBasis_p = for (i <- gp.klBasis.indices) yield {
//      val phipi_memo = Memoize(phip(i), 1000)
//      val newEf = Field(gp.domain, (x: Point[D]) => phipi_memo(x))
//      val newEv = innerD2(i)
//      Eigenpair(newEv, newEf)
//    }
//    new LowRankGaussianRandomRandomField[D, Value](gp.representer, mean_p, klBasis_p)
//  }
//
//  /*
//  * Internal computations of the regression.
//   */
//  private def genericRegressionComputations[D <: Dim: NDSpace, Value](gp: LowRankGaussianRandomRandomField[D, Value],
//    trainingData: IndexedSeq[(Point[D], Value, MultivariateNormalDistribution)]) = {
//
//    val outputDimension = gp.outputDimension
//
//    val (xs, ys, errorDistributions) = trainingData.unzip3
//
//    val yVec = gp.representer.vectorize(ys)
//    val meanValues = xs.map(gp.mean)
//    val mVec = gp.representer.vectorize(meanValues)
//
//    val Q = DenseMatrix.zeros[Double](trainingData.size * outputDimension, gp.klBasis.size)
//    for ((x_i, i) <- xs.zipWithIndex; (Eigenpair(lambda_j, phi_j), j) <- gp.klBasis.zipWithIndex) {
//      Q(i * outputDimension until i * outputDimension + outputDimension, j) := gp.representer.vectorize(phi_j(x_i)) * math.sqrt(lambda_j)
//    }
//
//    // What we are actually computing here is the following:
//    // L would be a block diagonal matrix, which contains on the diagonal the blocks that describes the uncertainty
//    // for each point (a d x d) block. We then would compute Q.t * L. For efficiency reasons (L could be large but is sparse)
//    // we avoid ever constructing the matrix L and do the multiplication by hand.
//    val QtL = Q.t.copy
//    assert(QtL.cols == errorDistributions.size * outputDimension)
//    assert(QtL.rows == gp.rank)
//    for ((errDist, i) <- errorDistributions.zipWithIndex) {
//      QtL(::, i * outputDimension until (i + 1) * outputDimension) := QtL(::, i * outputDimension until (i + 1) * outputDimension) * breeze.linalg.inv(errDist.cov)
//    }
//
//    val M = QtL * Q + DenseMatrix.eye[Double](gp.klBasis.size)
//    val Minv = breeze.linalg.pinv(M)
//
//    (Minv, QtL, yVec, mVec)
//  }
//
//  /**
//   * perform a rigid transformation of the gaussian process, i.e. it is later defined on the transformed domain and its
//   * vectors are transformed along the domain.
//   */
//  //  def transform[D <: Dim: NDSpace, Value](gp: LowRankGaussianField[D, Value], rigidTransform: RigidTransformation[D]): LowRankGaussianField[D, Value] = {
//  //    val invTransform = rigidTransform.inverse
//  //
//  //    val newDomain = gp.domain.warp(rigidTransform)
//  //
//  //    def newMean(pt: Point[D]): Value = {
//  //      val ptOrigGp = invTransform(pt)
//  //      rigidTransform(ptOrigGp + gp.mean(ptOrigGp)) - rigidTransform(ptOrigGp)
//  //    }
//  //
//  //    val newBasis = for (Eigenpair(ev, phi) <- gp.klBasis) yield {
//  //      def newPhi(pt: Point[D]): Value = {
//  //        val ptOrigGp = invTransform(pt)
//  //        rigidTransform(ptOrigGp + phi(ptOrigGp)) - pt
//  //      }
//  //      val newBasis = Field(newDomain, newPhi _)
//  //      Eigenpair(ev, newBasis)
//  //    }
//  //
//  //    new LowRankGaussianField[D, Value](gp.representer, Field(newDomain, newMean _), newBasis)
//  //  }
//
//}
//
///**
// * Represents a low-rank gaussian process, that is only defined at a finite, discrete set of points.
// * It supports the same operations as the LowRankGaussianProcess class, but always returns instead a
// * discrete representation. Furthermore, most operations are much more efficient, as they are implemented
// * using fast matrix/vector operations.
// *
// * Where the modeled functions in a LowRankGaussianProcess are of type Point[D]=>Vector[D], this discretized version is of type VectorPointData.
// *
// * It is possible to convert a DiscreteLowRankGaussianProcess to a LowRankGaussianProcess by calling the interpolation method.
// *
// * @see [[scalismo.common.DiscreteVectorField]]
// * @see [[DiscreteLowRankGaussianRandomField]]
// */
//
//import scalismo.GeneralGaussianField.Adapted.DiscreteLowRankGaussianRandomField.basisMatrixToCov
//
//case class DiscreteLowRankGaussianRandomField[D <: Dim: NDSpace, Value] private[scalismo] (override val representer: Vectorizer[Value],
//  override val domain: DiscreteDomain[D],
//  override val meanVector: DenseVector[Double],
//  variance: DenseVector[Double],
//  val kernelMatrix: DenseMatrix[Double])
//    extends DiscreteGaussianRandomField[D, Value](representer, DiscreteField(domain, representer.unvectorizeField(meanVector)), basisMatrixToCov(domain, variance, kernelMatrix)) {
//  self =>
//
//  /** See [[DiscreteLowRankGaussianRandomField.rank]] */
//  val rank: Int = kernelMatrix.cols
//
//  /**
//   * Discrete version of [[DiscreteLowRankGaussianRandomField.instance]]
//   */
//  def instance(c: DenseVector[Double]): DiscreteField[D, Value] = {
//    require(rank == c.size)
//    val instVal = instanceVector(c)
//    DiscreteField(domain, representer.unvectorizeField(instVal))
//  }
//
//  /**
//   * Returns the probability density of the instance produced by the x coefficients
//   */
//  def pdf(coefficients: DenseVector[Double]) = {
//    if (coefficients.size != rank) throw new Exception(s"invalid vector dimensionality (provided ${coefficients.size} should be $rank)")
//    val mvnormal = MultivariateNormalDistribution(DenseVector.zeros[Double](rank), diag(DenseVector.ones[Double](rank)))
//    mvnormal.pdf(coefficients)
//  }
//
//  /**
//   * Returns the log of the probability density of the instance produced by the x coefficients.
//   *
//   * If you are interested in ordinal comparisons of PDFs, use this as it is numerically more stable
//   */
//  def logpdf(coefficients: DenseVector[Double]) = {
//    if (coefficients.size != rank) throw new Exception(s"invalid vector dimensionality (provided ${coefficients.size} should be $rank)")
//    val mvnormal = MultivariateNormalDistribution(DenseVector.zeros[Double](rank), diag(DenseVector.ones[Double](rank)))
//    mvnormal.logpdf(coefficients)
//  }
//
//  /**
//   * Returns the probability density of the given instance
//   */
//  override def pdf(instance: DiscreteField[D, Value]): Double = pdf(coefficients(instance))
//
//  /**
//   * Returns the log of the probability density of the instance
//   *
//   * If you are interested in ordinal comparisons of PDFs, use this as it is numerically more stable
//   */
//  override def logpdf(instance: DiscreteField[D, Value]): Double = logpdf(coefficients(instance))
//
//  /**
//   * Discrete version of [[DiscreteLowRankGaussianRandomField.sample]]
//   */
//  override def sample: DiscreteField[D, Value] = {
//    val coeffs = for (_ <- 0 until rank) yield Gaussian(0, 1).draw()
//    instance(DenseVector(coeffs.toArray))
//  }
//
//  /**
//   * Returns the variance and associated basis function that defines the process.
//   * The basis is the (discretized) Karhunen Loeve basis (e.g. it is obtained from a Mercer's decomposition of the covariance function
//   */
//  def klBasis: DiscKLBasis[D, Value] = {
//    for (i <- 0 until rank) yield {
//      val eigenValue = variance(i)
//      val eigenFunction = new DiscreteField(domain, representer.unvectorizeField(kernelMatrix(::, i).toDenseVector))
//      DiscEigenpair(eigenValue, eigenFunction)
//    }
//  }
//
//  /**
//   * Discrete version of [[LowRankGaussianRandomRandomField.project(IndexedSeq[(Point[D], Vector[DO])], Double)]]
//   */
//  override def project(s: DiscreteField[D, Value]): DiscreteField[D, Value] = {
//    instance(coefficients(s))
//  }
//
//  /**
//   * Discrete version of [[DiscreteLowRankGaussianRandomField.coefficients(IndexedSeq[(Point[D], Vector[DO], Double)])]]
//   */
//  def coefficients(s: DiscreteField[D, Value]): DenseVector[Double] = {
//    val sigma2 = 1e-5 // regularization weight to avoid numerical problems
//    val noiseDist = MultivariateNormalDistribution(DenseVector.zeros[Double](outputDimension), DenseMatrix.eye[Double](outputDimension) * sigma2)
//    val td = s.valuesWithIds.map { case (v, id) => (id, v, noiseDist) }.toIndexedSeq
//    val (minv, qtL, yVec, mVec) = DiscreteLowRankGaussianRandomField.genericRegressionComputations(this, td)
//    val mean_coeffs = (minv * qtL) * (yVec - mVec)
//    mean_coeffs
//  }
//
//  /**
//   * Discrete version of [[DiscreteLowRankGaussianRandomField.posterior(IndexedSeq[(Point[D], Vector[DO])], sigma2: Double]]. In contrast to this method, the points for the training
//   * data are defined by the pointId. The returned posterior process is defined at the same points.
//   *
//   */
//  def posterior(trainingData: IndexedSeq[(PointId, Value)], sigma2: Double): DiscreteLowRankGaussianRandomField[D, Value] = {
//    val cov = MultivariateNormalDistribution(DenseVector.zeros[Double](outputDimension), DenseMatrix.eye[Double](outputDimension) * sigma2)
//    val newtd = trainingData.map { case (ptId, df) => (ptId, df, cov) }
//    posterior(newtd)
//  }
//
//  /**
//   * Discrete version of [[DiscreteLowRankGaussianRandomField.posterior(IndexedSeq[(Point[D], Vector[DO], Double)])]]. In contrast to this method, the points for the training
//   * data are defined by the pointId. The returned posterior process is defined at the same points.
//   *
//   */
//  def posterior(trainingData: IndexedSeq[(PointId, Value, MultivariateNormalDistribution)]): DiscreteLowRankGaussianRandomField[D, Value] = {
//    DiscreteLowRankGaussianRandomField.regression(this, trainingData)
//  }
//
//  override def marginal(pointIds: Seq[PointId])(implicit domainCreator: CreateUnstructuredPointsDomain[D]): DiscreteLowRankGaussianRandomField[D, Value] = {
//    val domainPts = domain.points.toIndexedSeq
//
//    val newPts = pointIds.map(pointId => domainPts(pointId.id)).toIndexedSeq
//    val newDomain = domainCreator.create(newPts)
//
//    val newMean = new DiscreteField(newDomain, pointIds.toIndexedSeq.map(id => mean(id)))
//
//    val newKLBasis = for (DiscEigenpair(lambda, phi) <- klBasis) yield {
//      val newValues = pointIds.map(i => phi(i)).toIndexedSeq
//      DiscEigenpair(lambda, new DiscreteField(newDomain, newValues))
//
//    }
//
//    DiscreteLowRankGaussianRandomField(representer, newMean, newKLBasis)
//  }
//
//  /**
//   * Interpolates discrete Gaussian process to have a new, continuous representation as a [[DiscreteLowRankGaussianRandomField]].
//   * This is achieved by using a  Nystrom method for computing the kl basis.
//   * The mean function is currently interpolated using a nearest neighbor approach.
//   *
//   * @param nNystromPoints determines how many points of the domain are used to estimate the full
//   *                       kl basis.
//   */
//
//  def interpolateNystrom(nNystromPoints: Int = 2 * rank): LowRankGaussianRandomRandomField[D, Value] = {
//
//    val sampler = new Sampler[D] {
//      override def volumeOfSampleRegion = numberOfPoints.toDouble
//
//      override val numberOfPoints = nNystromPoints
//      val p = volumeOfSampleRegion / numberOfPoints
//      val randGen = new util.Random()
//      val domainPoints = domain.points.toIndexedSeq
//
//      override def sample = {
//        val sampledPtIds = for (_ <- 0 until nNystromPoints) yield randGen.nextInt(domain.numberOfPoints)
//        sampledPtIds.map(ptId => (domainPoints(ptId), p))
//      }
//    }
//
//    // TODO, here we could do something smarter, such as e.g. b-spline interpolation
//    val meanPD = this.mean
//
//    def meanFun(pt: Point[D]): Value = {
//      val closestPtId = self.domain.findClosestPoint(pt).id
//      meanPD(closestPtId)
//    }
//
//    val covFun: GeneralGaussianField.ContinuousMatrixValuedKernel[D] = {
//      val domain = RealSpace[D]
//
//      def k(x: Point[D], y: Point[D]): DenseMatrix[Double] = {
//        val xId = self.domain.findClosestPoint(x).id
//        val yId = self.domain.findClosestPoint(y).id
//        cov(xId, yId)
//      }
//      new GeneralGaussianField.ContinuousMatrixValuedKernel[D](domain, k, outputDimension)
//    }
//    val gp = GaussianRandomField(representer, Field(RealSpace[D], meanFun _), covFun)
//    LowRankGaussianRandomRandomField.approximateGP[D, Value](gp, sampler, rank)
//  }
//
//  /**
//   * Interpolates discrete Gaussian process to have a new, continuous representation as a [[DiscreteLowRankGaussianRandomField]],
//   * using nearest neigbor interpolation (for both mean and covariance function)
//   */
//  override def interpolateNearestNeighbor: LowRankGaussianRandomRandomField[D, Value] = {
//
//    val meanPD = this.mean
//
//    // we cache the closest point computation, as it might be heavy for general domains, and we know that
//    // we will have the same oints for all the eigenfunctions
//    val findClosestPointMemo = Memoize((pt: Point[D]) => domain.findClosestPoint(pt).id, cacheSizeHint = 1000000)
//
//    def meanFun(closestPointFun: Point[D] => PointId)(pt: Point[D]): Value = {
//      val closestPtId = closestPointFun(pt)
//      meanPD(closestPtId)
//    }
//
//    def phi(i: Int, closetPointFun: Point[D] => PointId)(pt: Point[D]): Value = {
//      val closestPtId = closetPointFun(pt)
//      representer.unvectorize(kernelMatrix(closestPtId.id * outputDimension until (closestPtId.id + 1) * outputDimension, i))
//    }
//
//    val interpolatedKLBasis = {
//
//      (0 until rank) map (i => Eigenpair(variance(i), Field(RealSpace[D], phi(i, findClosestPointMemo))))
//    }
//    new NearestNeighbourInterpolatedLowRankGaussianRandomProcess(Field(RealSpace[D], meanFun(findClosestPointMemo)), interpolatedKLBasis, this)
//  }
//
//  protected[scalismo] def instanceVector(alpha: DenseVector[Double]): DenseVector[Double] = {
//    require(rank == alpha.size)
//
//    kernelMatrix * (stddev :* alpha) + meanVector
//  }
//
//  private[this] val outputDimension = representer.dim
//
//  private[this] val stddev = variance.map(x => math.sqrt(x))
//
//}
//
///**
// * Convenience class to speedup sampling from a LowRankGaussianProcess obtained by nearest neighbor interpolation of a DiscreteLowRankGaussianProcess
// *
// */
//private[scalismo] class NearestNeighbourInterpolatedLowRankGaussianRandomProcess[D <: Dim: NDSpace, Value](mean: Field[D, Value],
//    klBasis: LowRankGaussianRandomRandomField.KLBasis[D, Value],
//    discreteGP: DiscreteLowRankGaussianRandomField[D, Value]) extends LowRankGaussianRandomRandomField[D, Value](discreteGP.representer, mean, klBasis) {
//
//  require(klBasis.size == discreteGP.rank)
//
//  override def instance(c: DenseVector[Double]): Field[D, Value] = discreteGP.instance(c).interpolateNearestNeighbor()
//
//  // if all training data points belong to the interpolated discrete domain, then we compute a discrete posterior GP and interpolate it
//  // this way the posterior will also remain very efficient when sampling.
//  override def posterior(trainingData: IndexedSeq[(Point[D], Value, MultivariateNormalDistribution)]): LowRankGaussianRandomRandomField[D, Value] = {
//
//    val allInDiscrete = trainingData.forall { case (pt, vc, nz) => discreteGP.domain.isDefinedAt(pt) }
//
//    if (allInDiscrete) {
//      val discreteTD = trainingData.map { case (pt, vc, nz) => (discreteGP.domain.findClosestPoint(pt).id, vc, nz) }
//      discreteGP.posterior(discreteTD).interpolateNearestNeighbor
//    } else {
//      LowRankGaussianRandomRandomField.regression(this, trainingData)
//    }
//  }
//
//}
//
//object DiscreteLowRankGaussianRandomField {
//
//  case class DiscEigenpair[D <: Dim, Value](eigenvalue: Double, eigenfunction: DiscreteField[D, Value])
//
//  type DiscKLBasis[D <: Dim, Value] = Seq[DiscEigenpair[D, Value]]
//
//  /**
//   * Creates a new DiscreteLowRankGaussianProcess by discretizing the given gaussian process at the domain points.
//   */
//  def apply[D <: Dim: NDSpace, Value](domain: DiscreteDomain[D],
//    gp: LowRankGaussianRandomRandomField[D, Value]): DiscreteLowRankGaussianRandomField[D, Value] = {
//    val points = domain.points.toSeq
//
//    val outputDimension = gp.outputDimension
//
//    // precompute all the at the given points
//
//    val m = DenseVector.zeros[Double](points.size * outputDimension)
//    for (xWithIndex <- points.zipWithIndex.par) {
//      val (x, i) = xWithIndex
//      m(i * outputDimension until (i + 1) * outputDimension) := gp.representer.vectorize(gp.mean(x))
//    }
//
//    val U = DenseMatrix.zeros[Double](points.size * outputDimension, gp.rank)
//    val lambdas = DenseVector.zeros[Double](gp.rank)
//    for (xWithIndex <- points.zipWithIndex.par; (eigenPair_j, j) <- gp.klBasis.zipWithIndex) {
//      val LowRankGaussianRandomRandomField.Eigenpair(lambda_j, phi_j) = eigenPair_j
//      val (x, i) = xWithIndex
//      val v = phi_j(x)
//      U(i * outputDimension until (i + 1) * outputDimension, j) := gp.representer.vectorize(phi_j(x))
//      lambdas(j) = lambda_j
//    }
//
//    DiscreteLowRankGaussianRandomField(gp.representer, domain, m, lambdas, U)
//  }
//
//  def apply[D <: Dim: NDSpace, Value](representer: Vectorizer[Value],
//    mean: DiscreteField[D, Value],
//    klBasis: DiscKLBasis[D, Value]): DiscreteLowRankGaussianRandomField[D, Value] = {
//    val outputDimension = representer.dim
//
//    for (DiscEigenpair(_, phi) <- klBasis) {
//      require(phi.domain == mean.domain)
//    }
//
//    val domain = mean.domain
//    val meanVec = representer.vectorize(mean.data)
//    val varianceVec = DenseVector.zeros[Double](klBasis.size)
//    val basisMat = DenseMatrix.zeros[Double](meanVec.length, klBasis.size)
//    for ((eigenPair, i) <- klBasis.zipWithIndex) yield {
//      val DiscEigenpair(lambda, phi) = eigenPair
//      basisMat(::, i) := representer.vectorize(phi.data)
//      varianceVec(i) = lambda
//    }
//    DiscreteLowRankGaussianRandomField(representer, domain, meanVec, varianceVec, basisMat)
//  }
//
//  /**
//   * Discrete implementation of [[LowRankGaussianRandomRandomField.regression]]
//   */
//  def regression[D <: Dim: NDSpace, Value](gp: DiscreteLowRankGaussianRandomField[D, Value], trainingData: IndexedSeq[(PointId, Value, MultivariateNormalDistribution)]): DiscreteLowRankGaussianRandomField[D, Value] = {
//
//    val (_Minv, _QtL, yVec, mVec) = genericRegressionComputations(gp, trainingData)
//    val mean_coeffs = (_Minv * _QtL) * (yVec - mVec)
//
//    //val mean_p = gp.instance(mean_coeffs)
//    val mean_pVector = gp.instanceVector(mean_coeffs)
//
//    val D = breeze.linalg.diag(DenseVector(gp.variance.map(math.sqrt).toArray))
//    val Sigma = D * _Minv * D
//    val SVD(innerU, innerD2, _) = breeze.linalg.svd(Sigma)
//
//    val lambdas_p = DenseVector[Double](innerD2.toArray)
//
//    // we do the following computation
//    // val eigenMatrix_p = gp.eigenMatrix * innerU // IS this correct?
//    // but in parallel
//    val eigenMatrix_p = DenseMatrix.zeros[Double](gp.kernelMatrix.rows, innerU.cols)
//    for (rowInd <- (0 until gp.kernelMatrix.rows).par) {
//
//      // TODO maybe this strange transposing can be alleviated? It seems breeze does not support
//      // row-vector matrix multiplication
//      eigenMatrix_p(rowInd, ::) := (innerU.t * gp.kernelMatrix(rowInd, ::).t).t
//    }
//
//    DiscreteLowRankGaussianRandomField(gp.representer, gp.domain, mean_pVector, lambdas_p, eigenMatrix_p)
//  }
//
//  /**
//   * Creates a new DiscreteLowRankGaussianProcess, where the mean and covariance matrix are estimated from the given sample of continuous vector fields using Principal Component Analysis.
//   *
//   */
//  def createUsingPCA[D <: Dim: NDSpace, Value](representer: Vectorizer[Value],
//    domain: DiscreteDomain[D],
//    fields: Seq[DiscreteField[D, Value]]): DiscreteLowRankGaussianRandomField[D, Value] = {
//    val dim = implicitly[NDSpace[D]].dimensionality
//
//    val n = fields.size
//    val p = domain.numberOfPoints
//
//    // create the data matrix
//    val X = DenseMatrix.zeros[Double](n, p * dim)
//    for (p1 <- fields.zipWithIndex.par; p2 <- domain.pointsWithId) {
//      val (f, i) = p1
//      val (x, ptId) = p2
//      val ux = f(ptId)
//      X(i, ptId.id * dim until (ptId.id + 1) * dim) := representer.vectorize(ux).t
//    }
//
//    def demean(X0: DenseMatrix[Double]): (DenseMatrix[Double], DenseVector[Double]) = {
//      val m: DenseVector[Double] = breeze.stats.mean(X0(::, *)).toDenseVector
//      for (i <- 0 until X0.rows) {
//        X0(i, ::) := X0(i, ::) - m.t
//      }
//      (X0, m)
//    }
//
//    val (x0, meanVec) = demean(X)
//    val SVD(u, d2, vt) = breeze.linalg.svd(x0 * x0.t * (1.0 / (n - 1)))
//
//    val D = d2.map(v => Math.sqrt(v))
//    val Dinv = D.map(d => if (d > 1e-6) 1.0 / d else 0.0)
//
//    // a Matrix with the eigenvectors
//    val U: DenseMatrix[Double] = x0.t * vt.t * breeze.linalg.diag(Dinv) / Math.sqrt(n - 1)
//
//    new DiscreteLowRankGaussianRandomField(representer, domain, meanVec, d2, U)
//
//  }
//
//  private def genericRegressionComputations[D <: Dim: NDSpace, Value](gp: DiscreteLowRankGaussianRandomField[D, Value],
//    trainingData: IndexedSeq[(PointId, Value, MultivariateNormalDistribution)]) = {
//    val dim = gp.outputDimensionality
//    val (ptIds, ys, errorDistributions) = trainingData.unzip3
//
//    val yVec = gp.representer.vectorize(ys)
//    val meanValues = ptIds.map(pid => gp.mean(pid))
//    val mVec = gp.representer.vectorize(meanValues)
//
//    val Q = DenseMatrix.zeros[Double](trainingData.size * dim, gp.rank)
//    for ((ptId, i) <- ptIds.zipWithIndex; j <- 0 until gp.rank) {
//      val eigenVecAtPoint = gp.kernelMatrix((ptId.id * dim) until ((ptId.id + 1) * dim), j).map(_.toDouble)
//      Q(i * dim until i * dim + dim, j) := eigenVecAtPoint * math.sqrt(gp.variance(j))
//    }
//
//    // What we are actually computing here is the following:
//    // L would be a block diagonal matrix, which contains on the diagonal the blocks that describes the uncertainty
//    // for each point (a d x d) block. We then would compute Q.t * L. For efficiency reasons (L could be large but is sparse)
//    // we avoid ever constructing the matrix L and do the multiplication by hand.
//    val QtL = Q.t.copy
//    assert(QtL.cols == errorDistributions.size * dim)
//    assert(QtL.rows == gp.rank)
//    for ((errDist, i) <- errorDistributions.zipWithIndex) {
//      QtL(::, i * dim until (i + 1) * dim) := QtL(::, i * dim until (i + 1) * dim) * breeze.linalg.inv(errDist.cov)
//    }
//
//    val M = QtL * Q + DenseMatrix.eye[Double](gp.rank)
//    val Minv = breeze.linalg.pinv(M)
//
//    (Minv, QtL, yVec, mVec)
//  }
//
//  private def basisMatrixToCov[D <: Dim: NDSpace, Value](domain: DiscreteDomain[D],
//    variance: DenseVector[Double],
//    basisMatrix: DenseMatrix[Double]): DiscreteMatrixValuedKernel[D] = {
//
//    val outputDimensionality = 3
//    def cov(p1: PointId, p2: PointId): DenseMatrix[Double] = {
//
//      val pid1 = p1.id
//      val pid2 = p2.id
//      val eigenMatrixForPtId1 = basisMatrix(pid1 * outputDimensionality until (pid1 + 1) * outputDimensionality, ::)
//      val eigenMatrixForPtId2 = basisMatrix(pid2 * outputDimensionality until (pid2 + 1) * outputDimensionality, ::)
//      //val covValue = eigenMatrixForPtId1 * breeze.linalg.diag(stddev :* stddev) * eigenMatrixForPtId2.t
//
//      // same as commented line above, but just much more efficient (as breeze does not have diag matrix,
//      // the upper command does a lot of  unnecessary computations
//      val covValue = DenseMatrix.zeros[Double](outputDimensionality, outputDimensionality)
//
//      for (i <- (0 until outputDimensionality).par) {
//        val ind1 = pid1 * outputDimensionality + i
//        var j = 0
//        while (j < outputDimensionality) {
//          val ind2 = pid2 * outputDimensionality + j
//          var k = 0
//          var valueIJ = 0.0
//          while (k < basisMatrix.cols) {
//            valueIJ += basisMatrix(ind1, k) * basisMatrix(ind2, k) * variance(k)
//            k += 1
//          }
//          covValue(i, j) = valueIJ
//          j += 1
//        }
//      }
//
//      covValue
//    }
//
//    new GeneralGaussianField.DiscreteMatrixValuedKernel[D](domain, cov, outputDimensionality)
//  }
//
//}
//
//import scalismo.geometry.Vector._
//
//class StatisticalShapeModel(val reference: TriangleMesh[_3D],
//  val grf: DiscreteLowRankGaussianRandomField[_3D, Vector[_3D]]) extends {
//  def sample = {
//    val s: DiscreteField[_3D, Vector[_3D]] = grf.sample
//    val refPt: IndexedSeq[Point[_3D]] = reference.pointSet.points.toIndexedSeq
//    val newPoints = refPt.zip(s.data).map(e => e._1 + e._2)
//    new TriangleMesh3D(UnstructuredPointsDomain(newPoints), reference.triangulation)
//  }
//}
//
//object StatisticalShapeModel {
//
//  class VectorRepresenter[D <: Dim: NDSpace] extends Vectorizer[Vector[D]] {
//    override def dim: Int = implicitly[NDSpace[D]].dimensionality
//
//    override def vectorize(v: Vector[D]): DenseVector[Double] = v.toBreezeVector
//
//    override def unvectorize(d: DenseVector[Double]): Vector[D] = fromBreezeVector(d)
//  }
//
//  val representer3D = new VectorRepresenter[_3D]
//
//  def apply(reference: TriangleMesh[_3D], fields: Seq[DiscreteField[_3D, Vector[_3D]]]): StatisticalShapeModel = {
//    val grf = DiscreteLowRankGaussianRandomField.createUsingPCA[_3D, Vector[_3D]](representer3D, reference.pointSet, fields)
//    new StatisticalShapeModel(reference, grf)
//  }
//
//}
//
