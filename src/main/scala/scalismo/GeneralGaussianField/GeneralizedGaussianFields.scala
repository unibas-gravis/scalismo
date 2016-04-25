package scalismo.GeneralGaussianField.Adapted

import breeze.linalg._
import breeze.linalg.DenseVector
import scalismo.common._
import scalismo.geometry._
import scalismo.kernels.{DiscreteMatrixValuedPDKernel, MatrixValuedPDKernel}
import scalismo.statisticalmodel.MultivariateNormalDistribution
import scalismo.GeneralGaussianField

/**
  * A representation of a gaussian process, which is only defined on a discrete domain.
  * While this is technically similar to a MultivariateNormalDistribution, we highlight with this
  * class that we represent (discrete) functions, defined on the given domain.
  */
class DiscreteGaussianField[D <: Dim : NDSpace, Value] private[scalismo](val representer: GeneralGaussianField.Vectorizer[Value],
                                                                         val mean: DiscreteField[D, Value],
                                                                         val cov: GeneralGaussianField.DiscreteMatrixValuedKernel[D]) {
  self =>

  require(mean.domain == cov.domain)

  val domain = mean.domain

  val outputDimensionality = representer.dim

  lazy val meanVector = representer.vectorize(mean.data)
  lazy val kernelMatrix = GeneralGaussianField.Kernel.computeKernelMatrix[D](mean.domain.points.toIndexedSeq, cov)
  lazy val multivariateNormalDistribution = MultivariateNormalDistribution(meanVector, kernelMatrix)

  def sample: DiscreteField[D, Value] = {
    // define the mean and kernel matrix for the given points and construct the
    // corresponding MV Normal distribution, from which we then sample

    val d = outputDimensionality
    val sampleVec = multivariateNormalDistribution.sample()

    // The sample is a vector. We convert it back to a discreteVectorField.
    val vecs = (0 until mean.domain.numberOfPoints).map { i =>
      val split = sampleVec(i * outputDimensionality until (i + 1) * outputDimensionality)
      representer.unvectorize(split)
    }
    new DiscreteField[D, Value](domain, vecs)
  }

  /**
    * The marginal distribution at a given (single) point, specified by the pointId.
    */
  def marginal(pointId: PointId) = {
    MultivariateNormalDistribution(representer.vectorize(mean(pointId)), cov(pointId, pointId))
  }

  /**
    * The marginal distribution for the points specified by the given point ids.
    * Note that this is again a DiscreteGaussianProcess.
    */
  def marginal(pointIds: Seq[PointId])(implicit domainCreator: CreateUnstructuredPointsDomain[D]): DiscreteGaussianField[D, Value] = {
    val domainPts = domain.points.toIndexedSeq

    val newPts = pointIds.map(pointId => domainPts(pointId.id)).toIndexedSeq
    val newDomain = domainCreator.create(newPts)

    val newMean = new DiscreteField[D, Value](newDomain, pointIds.toIndexedSeq.map(id => mean(id)))
    val newCov = (i: Point[D], j: Point[D]) => {
      cov(i, j)
    }
    val newDiscreteCov = new GeneralGaussianField.DiscreteMatrixValuedKernel(newDomain, newCov, cov.outputDim)

    new DiscreteGaussianField(representer, newMean, newDiscreteCov)
  }

  /**
    * Interpolates discrete Gaussian process to have a new, continuous representation as a [[DiscreteLowRankGaussianProcess]],
    * using nearest neighbor interpolation (for both mean and covariance function)
    */
  def interpolateNearestNeighbor: GaussianField[D, Value] = {

    val meanDiscreteGp = this.mean

    val newDomain = RealSpace[D]
    def meanFun: (Point[D]) => Value = (pt: Point[D]) => {
      val closestPtId = domain.findClosestPoint(pt).id
      meanDiscreteGp(closestPtId)
    }

    val newCov = new GeneralGaussianField.ContinuousMatrixValuedKernel[D](
      newDomain,
      (pt1: Point[D], pt2: Point[D]) => {
        val closestPtId1 = self.domain.findClosestPoint(pt1).id
        val closestPtId2 = self.domain.findClosestPoint(pt2).id
        cov(closestPtId1, closestPtId2)
      },
      outputDimensionality)
    GaussianField(representer, Field[D, Value](newDomain, meanFun), newCov)
  }


  /**
    * Discrete version of [[LowRankGaussianProcess.project(IndexedSeq[(Point[D], Vector[DO])], Double)]]
    */

  def project(s: DiscreteField[D, Value]): DiscreteField[D, Value] = {

    val sigma2 = 1e-5 // regularization weight to avoid numerical problems
    val noiseDist = MultivariateNormalDistribution(DenseVector.zeros[Double](representer.dim), DenseMatrix.eye[Double](representer.dim) * sigma2)
    val td = s.values.zipWithIndex.map {
      case (v, id) => (id, v, noiseDist)
    }.toIndexedSeq
    DiscreteGaussianField.regression(this, td).mean

  }

  /**
    * Returns the probability density of the given instance
    */
  def pdf(instance: DiscreteField[D, Value]): Double = {
    val instvec = representer.vectorize(instance.data)
    multivariateNormalDistribution.pdf(instvec)
  }

  /**
    * Returns the log of the probability density of the given instance
    *
    * If you are interested in ordinal comparisons of PDFs, use this as it is numerically more stable
    */
  def logpdf(instance: DiscreteField[D, Value]): Double = {
    val instvec = representer.vectorize(instance.data)
    multivariateNormalDistribution.logpdf(instvec)
  }

}

object DiscreteGaussianField {

  def apply[D <: Dim : NDSpace, Value](representer: GeneralGaussianField.Vectorizer[Value],
                                       mean: DiscreteField[D, Value],
                                       cov: GeneralGaussianField.DiscreteMatrixValuedKernel[D]) = {
    new DiscreteGaussianField[D, Value](representer, mean, cov)
  }

  def apply[D <: Dim : NDSpace, Value](representer: GeneralGaussianField.Vectorizer[Value],
                                       domain: DiscreteDomain[D],
                                       gp: GaussianField[D, Value]) = {
    val domainPoints = domain.points.toIndexedSeq

    val discreteMean = new DiscreteField[D, Value](domain, domainPoints.map(pt => gp.mean(pt)))

    val k = (i: Point[D], j: Point[D]) => gp.cov(i, j)
    val discreteCov = new GeneralGaussianField.DiscreteMatrixValuedKernel(domain, k, representer.dim)

    new DiscreteGaussianField[D, Value](representer, discreteMean, discreteCov)
  }

  def regression[D <: Dim : NDSpace, Value](discreteGp: DiscreteGaussianField[D, Value],
                                            trainingData: IndexedSeq[(Int, Value, MultivariateNormalDistribution)]
                                           ): DiscreteGaussianField[D, Value] = {

    // TODO, this is somehow a hack to reuse the code written for the general GP regression. We should think if that has disadvantages
    // TODO We should think whether we can do it in  a conceptually more clean way.

    val domainPoints = discreteGp.domain.points.toIndexedSeq
    val gp = discreteGp.interpolateNearestNeighbor
    val tdForGp = trainingData.map({ case (id, vec, error) => (domainPoints(id), vec, error) })
    val posterior = GaussianField.regression(gp, tdForGp)

    DiscreteGaussianField(discreteGp.representer, discreteGp.domain, gp)
  }
}


/**
  * A gaussian process from a D dimensional input space, whose input values are points,
  * to a DO dimensional output space. The output space is a Euclidean vector space of dimensionality DO.
  *
  * @param representer The vectorizer of the output type
  * @param mean        The mean function
  * @param cov         The covariance function. Needs to be positive definite
  * @tparam D     The dimensionality of the input space
  * @tparam Value The output type
  */
class GaussianField[D <: Dim : NDSpace, Value] protected(val representer: GeneralGaussianField.Vectorizer[Value],
                                                         val mean: Field[D, Value],
                                                         val cov: GeneralGaussianField.ContinuousMatrixValuedKernel[D]) {

  //  protected[this] val dimOps: NDSpace[DO] = implicitly[NDSpace[DO]]

  private def outputDimensionality = representer.dim

  val domain = Domain.intersection(mean.domain, cov.domain)

  /**
    *
    * Sample values of the Gaussian process evaluated at the given points.
    */
  def sampleAtPoints(domain: DiscreteDomain[D]): DiscreteField[D, Value] = {
    this.marginal(domain).sample
  }

  /**
    * Compute the marginal distribution for the given points. The result is again a Gaussian process, whose domain
    * is defined by the given points.
    */
  def marginal(domain: DiscreteDomain[D]): DiscreteGaussianField[D, Value] = {
    val meanField = new DiscreteField[D, Value](domain, domain.points.toIndexedSeq.map(pt => mean(pt)))
    val pts = domain.points.toIndexedSeq
    def newCov(i: Point[D], j: Point[D]): DenseMatrix[Double] = {
      cov(i, j)
    }

    val discreteCov = new GeneralGaussianField.DiscreteMatrixValuedKernel[D](domain, newCov, cov.outputDim)
    new DiscreteGaussianField(representer, meanField, discreteCov)
  }

  /**
    * Compute the marginal distribution at a single point.
    */
  def marginal(pt: Point[D]) = MultivariateNormalDistribution(representer.vectorize(mean(pt)), cov(pt, pt))

  /**
    * The posterior distribution of the gaussian process, with respect to the given trainingData.
    * It is computed using Gaussian process regression.
    * We assume that the trainingData is subject to isotropic Gaussian noise with variance sigma2.
    */
  def posterior(trainingData: IndexedSeq[(Point[D], Value)], sigma2: Double): GaussianField[D, Value] = {
    val cov = MultivariateNormalDistribution(DenseVector.zeros[Double](outputDimensionality), DenseMatrix.eye[Double](outputDimensionality) * sigma2)
    val fullTrainingData = trainingData.map { case (p, v) => (p, v, cov) }
    GaussianField.regression(this, fullTrainingData)
  }

  /**
    * The posterior distribution of the gaussian process, with respect to the given trainingData.
    * It is computed using Gaussian process regression.
    */
  def posterior(trainingData: IndexedSeq[(Point[D], Value, MultivariateNormalDistribution)]): GaussianField[D, Value] = {
    GaussianField.regression(this, trainingData)
  }
}

/**
  * Factory methods for creating Gaussian processes
  */
object GaussianField {

  /**
    * Creates a new Gaussian process with given mean and covariance, which is defined on the given domain.
    */
  def apply[D <: Dim : NDSpace, Value](representer: GeneralGaussianField.Vectorizer[Value],
                                       mean: Field[D, Value],
                                       cov: GeneralGaussianField.ContinuousMatrixValuedKernel[D]) = {
    new GaussianField[D, Value](representer, mean, cov)
  }

  /**
    * * Performs a Gaussian process regression, where we assume that each training point (vector) is subject to  zero-mean noise with given variance.
    *
    * @param gp           The gaussian process
    * @param trainingData Point/value pairs where that the sample should approximate, together with an error model (the uncertainty) at each point.
    */
  def regression[D <: Dim : NDSpace, Value](gp: GaussianField[D, Value],
                                            trainingData: IndexedSeq[(Point[D], Value, MultivariateNormalDistribution)]
                                           ): GaussianField[D, Value] = {

    val outputDimensionality = gp.outputDimensionality
    val (xs, ys, errorDists) = trainingData.unzip3

    val mVec = gp.representer.vectorize(xs.map(gp.mean))
    val yVec = gp.representer.vectorize(ys)
    val fVec = yVec - mVec

    val K = GeneralGaussianField.Kernel.computeKernelMatrix(xs, gp.cov)
    for ((errorDist, i) <- errorDists.zipWithIndex) {
      K(i * outputDimensionality until (i + 1) * outputDimensionality, i * outputDimensionality until (i + 1) * outputDimensionality) += errorDist.cov
    }

    val K_inv = breeze.linalg.inv(K)

    def xstar(x: Point[D]) = {
      GeneralGaussianField.Kernel.computeKernelVectorFor[D](x, xs, gp.cov)
    }

    def posteriorMean(x: Point[D]): Value = {
      gp.representer.unvectorize((xstar(x) * K_inv) * fVec)
    }

    val posteriorKernel = new GeneralGaussianField.ContinuousMatrixValuedKernel[D](
      gp.domain,
      (x: Point[D], y: Point[D]) => {
        gp.cov(x, y) - (xstar(x) * K_inv * xstar(y).t)
      },
      outputDimensionality
    )

    new GaussianField[D, Value](gp.representer, Field[D, Value](gp.domain, posteriorMean _), posteriorKernel)
  }

  /**
    * * Computes the marginal likelihood of the observed data, according to the given GP.
    *
    * This can for example be used in a model selection setting, where the GP with the maximum marginal likelihood of the observed data would be selected.
    *
    * @param gp           The gaussian process
    * @param trainingData Point/value pairs where that the sample should approximate, together with an error model (the uncertainty) at each point.
    * @todo The current implementation can be optimized as it inverts the data covariance matrix (that can be heavy for more than a few points). Instead an implementation
    *       with a Cholesky decomposition would be more efficient.
    */
  def marginalLikelihood[D <: Dim : NDSpace, Value](gp: GaussianField[D, Value],
                                                    trainingData: IndexedSeq[(Point[D], Value, MultivariateNormalDistribution)]
                                                   ): Double = {

    val outputDim = gp.outputDimensionality

    // below is the implementation according to Rassmussen Gaussian Processes, Chapter 5, page 113

    val (ptIds, ys, errorDistributions) = trainingData.unzip3

    val yVec = gp.representer.vectorize(ys)

    val Ky = DenseMatrix.zeros[Double](trainingData.size * outputDim, trainingData.size * outputDim)

    for ((ptIdI, i) <- ptIds.zipWithIndex; (ptIdJ, j) <- ptIds.zipWithIndex) {

      val covBlock = gp.cov(ptIdI, ptIdJ)

      val Kyyp = if (i == j) {
        // in this case add observation uncertainty
        val noiseBlock = errorDistributions(i).cov
        covBlock + noiseBlock
      } else covBlock

      // insert the block in the big covariance matrix
      for (l <- (0 until outputDim); m <- (0 until outputDim)) {
        Ky(l + (i * outputDim), m + (j * outputDim)) = Kyyp(l, m)
      }
    }

    val KyInv = inv(Ky)
    val const = trainingData.length * 0.5 * math.log(math.Pi * 2)
    val margLikehood = ((yVec.t * KyInv * yVec) * -0.5f) - (0.5f * math.log(det(Ky))) - const
    margLikehood
  }

}
