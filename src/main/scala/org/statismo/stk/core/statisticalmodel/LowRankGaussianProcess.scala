package org.statismo.stk.core.statisticalmodel

import breeze.linalg.svd.SVD
import org.statismo.stk.core.geometry._
import org.statismo.stk.core.common.VectorField
import org.statismo.stk.core.kernels.{MatrixValuedPDKernel, Kernel}
import org.statismo.stk.core.common._
import org.statismo.stk.core.registration.{RigidTransformation, Transformation}
import org.statismo.stk.core.numerics.Sampler
import breeze.linalg.{*, Axis, DenseVector, DenseMatrix}
import breeze.stats.distributions.Gaussian


/**
 *
 * A gaussian process which is represented in terms of a (small) finite set of basis functions.
 * The basis functions are the orthonormal basis functions given by a mercers' decomposition.
 *
 * @param domain defines the set of points on which the GP is defined
 * @param mean The mean function
 * @param klBasis A set of basis functions
 * @tparam D The dimensionality of the input space
 * @tparam DO The dimensionality of the output space
 */
class LowRankGaussianProcess[D <: Dim : NDSpace, DO <: Dim : NDSpace](mean: VectorField[D, DO],
                                                                       val klBasis: IndexedSeq[(Float, VectorField[D, DO])])
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
    val f : Point[D] => Vector[DO] = x => {
      val deformationsAtX = (0 until klBasis.size).map(i => {
        val (lambda_i, phi_i) = klBasis(i)
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
    val coeffs = for (_ <- 0 until klBasis.size) yield Gaussian(0, 1).draw().toFloat
    instance(DenseVector(coeffs.toArray))
  }


  /**
   * A random sample evaluated at the given points
   */
  override def sampleAtPoints(pts: IndexedSeq[Point[D]]): DiscreteVectorField[D, DO] = {
    // TODO check that points are part of the domain
    val aSample = sample
    val values = pts.map(pt => aSample(pt))
    val domain = SpatiallyIndexedFiniteDiscreteDomain.fromSeq(pts)
    DiscreteVectorField(domain, values)
  }


  /**
   * Returns the sample of the gaussian process that best explains the given training data. It is assumed that the training data (values)
   * are subject to 0 mean Gaussian noise
   *
   * @param trainingData Point/value pairs where that the sample should approximate.
   * @param sigma2 variance of a GAussian noise that is assumed on every training point
   */
  def project(trainingData: IndexedSeq[(Point[D], Vector[DO])], sigma2: Double = 1e-6): VectorField[D, DO] = {
    val newtd = trainingData.map { case (pt, df) => (pt, df, sigma2)}
    project(newtd)
  }


  /**
   * Returns the sample of the gaussian process that best explains the given training data. It is assumed that the training data (values)
   * are subject to 0 mean Gaussian noise
   *
   * @param trainingData Point/value pairs where that the sample should approximate, together with the variance of the noise model at each point.
   */
  def project(trainingData: IndexedSeq[(Point[D], Vector[DO], Double)]): VectorField[D, DO] = {
    val c = coefficients(trainingData)
    instance(c)
  }

  /**
   * Returns the sample of the coefficients of the sample that best explains the given training data. It is assumed that the training data (values)
   * are subject to 0 mean Gaussian noise
   */
  def coefficients(trainingData: IndexedSeq[(Point[D], Vector[DO], Double)]): DenseVector[Float] =
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
    val newtd = trainingData.map { case (pt, df) => (pt, df, sigma2)}
    coefficients(newtd)
  }


  /**
   * The posterior distribution of the gaussian process, with respect to the given trainingData. It is computed using Gaussian process regression.
   */
  def posterior(trainingData: IndexedSeq[(Point[D], Vector[DO])], sigma2: Double) : LowRankGaussianProcess[D, DO] = {
    val newtd = trainingData.map { case (pt, df) => (pt, df, sigma2)}
    posterior(newtd)
  }

  /**
   * The posterior distribution of the gaussian process, with respect to the given trainingData. It is computed using Gaussian process regression.
   */
  def posterior(trainingData: IndexedSeq[(Point[D], Vector[DO], Double)]) : LowRankGaussianProcess[D, DO] = {
    LowRankGaussianProcess.regression(this, trainingData)
  }


  /**
   * Discretize the gaussian process on the given points.
   */
  def discretize(points : Seq[Point[D]]) : DiscreteLowRankGaussianProcess[D, DO] = {
    val domain = FiniteDiscreteDomain.fromSeq(points.toIndexedSeq)
    DiscreteLowRankGaussianProcess(domain, this)
  }

}


/**
 * Factory methods for creating Low-rank gaussian processes, as well as generic algorithms to manipulate Gaussian proceses.
 */
object LowRankGaussianProcess {

  /**
   * Perform a low-rank approximation of the Gaussian process using the Nystrom method. The sample points used for the nystrom method
   * are sampled using the given sample.
   * @param gp The gaussian process to approximate
   * @param sampler determines which points will be used as samples for the nystrom approximation.
   * @param numBasisFunctions The number of basis functions to approximate.
   */
  def approximateGP[D <: Dim : NDSpace, DO <: Dim : NDSpace]( gp : GaussianProcess[D, DO],
                                                              sampler: Sampler[D],
                                                              numBasisFunctions: Int) = {
    val kltBasis = Kernel.computeNystromApproximation(gp.cov, numBasisFunctions, sampler)
    new LowRankGaussianProcess[D, DO](gp.mean, kltBasis)
  }


  private def covFromKLTBasis[D <: Dim : NDSpace, DO <: Dim : NDSpace](eigenPairs: IndexedSeq[(Float, VectorField[D, DO])]): MatrixValuedPDKernel[D, DO] = {
    val dimOps = implicitly[NDSpace[DO]]
    val cov: MatrixValuedPDKernel[D, DO] = new MatrixValuedPDKernel[D, DO] {
      override val domain = eigenPairs.headOption
        .map{case (_, eigenPair) => eigenPair.domain}.getOrElse(RealSpace[D])

      def apply(x: Point[D], y: Point[D]): SquareMatrix[DO] = {
        val ptDim = dimOps.dimensionality
        val phis = eigenPairs.map(_._2)

        var outer = SquareMatrix.zeros[DO]
        for ((lambda_i, phi_i) <- eigenPairs) {
          outer = outer + (phi_i(x) outer phi_i(y)) * lambda_i
        }
        outer

      }
    }
    cov
  }


  /**
   ** Performs a Gaussian process regression, where we assume that all training points (vectors) are subject to the same zero-mean Gaussian noise with variance simga2.
   *
   * @param gp  The gaussian process
   * @param trainingData Point/value pairs where that the sample should approximate
   * @param sigma2 The variance of the noise model
   * @param meanOnly Computes only the posterior mean and not the full posterior process. Return a Gaussian process of rank 0.
   */
  def regression[D <: Dim : NDSpace, DO <: Dim : NDSpace](gp: LowRankGaussianProcess[D, DO],
                                                          trainingData: IndexedSeq[(Point[D], Vector[DO])],
                                                          sigma2: Double,
                                                          meanOnly: Boolean = false)
  : LowRankGaussianProcess[D, DO] = {

    val trainingDataWithNoise = trainingData.map { case (x, y) => (x, y, sigma2)}
    regression(gp, trainingDataWithNoise, meanOnly)
  }

  /**
   ** Performs a Gaussian process regression, where we assume that each training point (vector) is subject to  zero-mean noise with given variance.
   *
   * @param gp  The gaussian process
   * @param trainingData Point/value pairs where that the sample should approximate, together with the variance of the noise model at each point.
   * @param meanOnly Computes only the posterior mean and not the full posterior process. Return a Gaussian process of rank 0.
   */
  def regression[D <: Dim : NDSpace, DO <: Dim : NDSpace](gp: LowRankGaussianProcess[D, DO],
                                                          trainingData: IndexedSeq[(Point[D], Vector[DO], Double)],
                                                          meanOnly: Boolean = false)
  : LowRankGaussianProcess[D, DO] = {
    val outputDim = implicitly[NDSpace[DO]].dimensionality

    val (lambdas, phis) = gp.klBasis.unzip
    val (_Minv, _QtL, yVec, mVec) = genericRegressionComputations(gp, trainingData)
    val mean_coeffs = (_Minv * _QtL).map(_.toFloat) * (yVec - mVec)

    val mean_p = gp.instance(mean_coeffs)

    if (meanOnly == true) {
      val emptyEigenPairs = IndexedSeq[(Float, VectorField[D, DO])]()
      new LowRankGaussianProcess(mean_p, emptyEigenPairs)

    } else {
      val D = breeze.linalg.diag(DenseVector(lambdas.map(math.sqrt(_)).toArray))
      val Sigma = D * _Minv * D
      val SVD(innerUDbl, innerD2, _) = breeze.linalg.svd(Sigma)
      val innerU = innerUDbl.map(_.toFloat)
      @volatile
      var phisAtXCache = ImmutableLRU[Point[D], DenseMatrix[Float]](1000)

      def phip(i: Int)(x: Point[D]): Vector[DO] = {
        // should be phi_p but _ is treated as partial function
        val (maybePhisAtX, newPhisAtXCache) = phisAtXCache.get(x)
        val phisAtX = maybePhisAtX.getOrElse {
          val newPhisAtX = {
            val innerPhisAtx = DenseMatrix.zeros[Float](outputDim, gp.rank)
            var j = 0;
            while (j < phis.size) {
              val phi_j = phis(j)
              innerPhisAtx(0 until outputDim, j) := phi_j(x).toBreezeVector
              j += 1
            }
            innerPhisAtx
          }
          phisAtXCache = (phisAtXCache + ((x, newPhisAtX)))._2 // ignore evicted key
          newPhisAtX
        }
        val vec = phisAtX * innerU(::, i)
        Vector[DO](vec.data)
      }

      val phis_p = for (i <- 0 until phis.size) yield (VectorField(gp.domain, (x: Point[D]) => phip(i)(x)))
      val lambdas_p = innerD2.toArray.map(_.toFloat).toIndexedSeq
      new LowRankGaussianProcess[D, DO](mean_p, lambdas_p.zip(phis_p))
    }
  }

  /*
  * Internal computations of the regression.
   */
  private def genericRegressionComputations[D <: Dim : NDSpace, DO <: Dim : NDSpace](gp: LowRankGaussianProcess[D, DO],
                                                                                     trainingData: IndexedSeq[(Point[D], Vector[DO], Double)],
                                                                                     meanOnly: Boolean = false) = {

    val outputDimensionality = implicitly[NDSpace[DO]].dimensionality

    val (lambdas, phis) = gp.klBasis.unzip
    val outputDim = outputDimensionality


    val dim = implicitly[NDSpace[DO]].dimensionality
    def flatten(v: IndexedSeq[Vector[DO]]) = DenseVector(v.flatten(_.data).toArray)

    val (xs, ys, sigma2s) = trainingData.unzip3

    val yVec = flatten(ys)
    val meanValues = xs.map(gp.mean)
    val mVec = flatten(meanValues)

    val Q = DenseMatrix.zeros[Double](trainingData.size * dim, phis.size)
    for ((x_i, i) <- xs.zipWithIndex; (phi_j, j) <- phis.zipWithIndex) {
      Q(i * dim until i * dim + dim, j) := phi_j(x_i).toBreezeVector.map(_.toDouble) * math.sqrt(lambdas(j))
    }

    // compute Q^TL where L is a diagonal matrix that contains the inverse of the sigmas in the diagonal.
    // As there is only one sigma for each point (but the point has dim components) we need
    // to correct the index for sigma
    val QtL = Q.t.copy
    val sigma2sInv = sigma2s.map { sigma2 =>
      val divisor = math.max(1e-8, sigma2)
      1.0 / divisor
    }
    for (i <- 0 until QtL.cols) {
      QtL(::, i) *= sigma2sInv(i / dim)
    }

    val M = QtL * Q + DenseMatrix.eye[Double](phis.size)
    val Minv = breeze.linalg.pinv(M)

    (Minv, QtL, yVec, mVec)
  }

  /**
   * perform a rigid transformation of the gaussian process, i.e. it is later defined on the transformed domain and its
   * vectors are transformed along the domain.
   */
  def transform[D <: Dim : NDSpace](gp: LowRankGaussianProcess[D, D], rigidTransform: RigidTransformation[D]): LowRankGaussianProcess[D, D] = {
    val invTransform = rigidTransform.inverse

    val newDomain = gp.domain.warp(rigidTransform)
    val (lambdas, phis) = gp.klBasis.unzip

    def newMean(pt: Point[D]): Vector[D] = {
      val ptOrigGp = invTransform(pt)
      rigidTransform(ptOrigGp + gp.mean(ptOrigGp)) - rigidTransform(ptOrigGp)
    }

    val newPhis = phis.map(phi => {
      def newPhi(pt: Point[D]): Vector[D] = {
        val ptOrigGp = invTransform(pt)
        rigidTransform(ptOrigGp + phi(ptOrigGp)) - pt
      }
      VectorField(newDomain, newPhi _)
    })

    val newEigenpairs = lambdas.zip(newPhis)


    new LowRankGaussianProcess[D, D](VectorField(newDomain, newMean _), newEigenpairs)
  }


}
