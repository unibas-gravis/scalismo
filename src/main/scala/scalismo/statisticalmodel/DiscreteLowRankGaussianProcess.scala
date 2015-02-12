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
import breeze.linalg.{ *, DenseMatrix, DenseVector }
import breeze.stats.distributions.Gaussian
import scalismo.common.FiniteDiscreteDomain.CanBound
import scalismo.common.{VectorField, DiscreteVectorField, FiniteDiscreteDomain}
import scalismo.geometry._
import scalismo.kernels.MatrixValuedPDKernel
import scalismo.mesh.kdtree.KDTreeMap
import scalismo.numerics.Sampler
import scalismo.registration.Transformation

/**
 * Represents a low-rank gaussian process, that is only defined at a finite, discrete set of points.
 * It supports the same operations as the LowRankGaussianProcess class, but always returns instead a
 * discrete representation. Furthermore, most operations are much more efficient, as they are implemented
 * using fast matrix/vector operations.
 *
 * Where the modeled functions in a LowRankGaussianProcess are of type Point[D]=>Vector[D], this discretized version is of type VectorPointData.
 *
 * It is possible to convert a DiscreteLowRankGaussianProcess to a LowRankGaussianProcess by calling the interpolation method.
 *
 * @see [[scalismo.common.DiscreteVectorField]]
 * @see [[DiscreteLowRankGaussianProcess]]
 */
case class DiscreteLowRankGaussianProcess[D <: Dim: NDSpace, DO <: Dim: NDSpace] private[scalismo] (val domain: FiniteDiscreteDomain[D],
                                                                                                val meanVector: DenseVector[Float],
                                                                                                val variance: DenseVector[Float],
                                                                                                val basisMatrix: DenseMatrix[Float]) { self =>


  /** See [[DiscreteLowRankGaussianProcess.rank]] */
  val rank: Int = basisMatrix.cols

  /** Discrete version of [[DiscreteLowRankGaussianProcess.mean]]*/
  def mean: DiscreteVectorField[D, DO] = vectorToVectorPoindData(meanVector)

  /**
   * A function, which returns for two given points, defined by their pointId, the covariance.
   */
  val cov: (Int, Int) => SquareMatrix[DO] = (ptId1, ptId2) => {
    val eigenMatrixForPtId1 = basisMatrix(ptId1 * outputDimensionality until (ptId1 + 1) * outputDimensionality, ::)
    val eigenMatrixForPtId2 = basisMatrix(ptId2 * outputDimensionality until (ptId2 + 1) * outputDimensionality, ::)
    //val covValue = eigenMatrixForPtId1 * breeze.linalg.diag(stddev :* stddev) * eigenMatrixForPtId2.t

    // same as commented line above, but just much more efficient (as breeze does not have diag matrix,
    // the upper command does a lot of  unnecessary computations
    val covValue = DenseMatrix.zeros[Float](outputDimensionality, outputDimensionality)

    for (i <- (0 until outputDimensionality).par) {
      val ind1 = ptId1 * outputDimensionality + i
      var j = 0
      while (j < outputDimensionality) {
        val ind2 = ptId2 * outputDimensionality + j
        var k = 0
        var valueIJ = 0f
        while (k < basisMatrix.cols) {
          valueIJ += basisMatrix(ind1, k) * basisMatrix(ind2, k) * variance(k)
          k += 1
        }
        covValue(i, j) = valueIJ
        j += 1
      }
    }

    SquareMatrix[DO](covValue.data)
  }

  /**
   * Discrete version of [[DiscreteLowRankGaussianProcess.instance]]
   */
  def instance(c: DenseVector[Float]): DiscreteVectorField[D, DO] = {
    require(rank == c.size)
    val instVal = instanceVector(c)
    vectorToVectorPoindData(instVal)
  }

  /**
   * Discrete version of [[DiscreteLowRankGaussianProcess.sample]]
   */
  def sample: DiscreteVectorField[D, DO] = {
    val coeffs = for (_ <- 0 until rank) yield Gaussian(0, 1).draw().toFloat
    instance(DenseVector(coeffs.toArray))
  }

  /**
   * Returns the variance and associated basis function that defines the process.
   * The basis is the (discretized) Karhunen Loeve basis (e.g. it is obtained from a Mercer's decomposition of the covariance function
   */
  def klBasis: IndexedSeq[(Float, DiscreteVectorField[D, DO])] = {
    for (i <- 0 until rank) yield (variance(i), vectorToVectorPoindData(basisMatrix(::, i).toDenseVector))
  }

  /**
   * Discrete version of [[LowRankGaussianProcess.project(IndexedSeq[(Point[D], Vector[DO])], Double)]]
   */
  def project(trainingData: IndexedSeq[(Int, Vector[DO])], sigma2: Double = 1e-6): DiscreteVectorField[D, DO] = {
    val newtd = trainingData.map { case (pt, df) => (pt, df, sigma2) }
    project(newtd)
  }

  /**
   * Discrete version of [[LowRankGaussianProcess.project(IndexedSeq[(Point[D], Vector[DO], Double)])]]
   */
  def project(trainingData: IndexedSeq[(Int, Vector[DO], Double)]): DiscreteVectorField[D, DO] = {
    val c = coefficients(trainingData)
    instance(c)
  }

  /**
   * Discrete version of [[DiscreteLowRankGaussianProcess.coefficients(IndexedSeq[(Point[D], Vector[DO], Double)])]]
   */
  def coefficients(trainingData: IndexedSeq[(Int, Vector[DO], Double)]): DenseVector[Float] = {

    val (minv, qtL, yVec, mVec) = DiscreteLowRankGaussianProcess.genericRegressionComputations(this, trainingData)
    val mean_coeffs = (minv * qtL).map(_.toFloat) * (yVec - mVec)
    mean_coeffs
  }

  /**
   * Discrete version of [[DiscreteLowRankGaussianProcess.coefficients(IndexedSeq[(Point[D], Vector[DO])], Double)]]
   */
  def coefficients(trainingData: IndexedSeq[(Int, Vector[DO])], sigma2: Double): DenseVector[Float] = {
    val newtd = trainingData.map { case (pt, df) => (pt, df, sigma2) }
    coefficients(newtd)
  }

  /**
   * @see [[DiscreteLowRankGaussianProcess.marginal]]
   */
  def marginal(pointId: Int) = {
    val meanAtPoint = meanVector(pointId * outputDimensionality until (pointId + 1) * outputDimensionality).copy
    NDimensionalNormalDistribution(Vector[DO](meanAtPoint.data), cov(pointId, pointId))
  }

  /**
   * Discrete version of [[DiscreteLowRankGaussianProcess.posterior(IndexedSeq[(Point[D], Vector[DO])], sigma2: Double]]. In contrast to this method, the points for the training
   * data are defined by the pointId. The returned posterior process is defined at the same points.
   *
   */
  def posterior(trainingData: IndexedSeq[(Int, Vector[DO])], sigma2: Double): DiscreteLowRankGaussianProcess[D, DO] = {
    val newtd = trainingData.map { case (ptId, df) => (ptId, df, sigma2) }
    posterior(newtd)
  }

  /**
   * Discrete version of [[DiscreteLowRankGaussianProcess.posterior(IndexedSeq[(Point[D], Vector[DO], Double)])]]. In contrast to this method, the points for the training
   * data are defined by the pointId. The returned posterior process is defined at the same points.
   *
   */
  def posterior(trainingData: IndexedSeq[(Int, Vector[DO], Double)]): DiscreteLowRankGaussianProcess[D, DO] = {
    DiscreteLowRankGaussianProcess.regression(this, trainingData, false)
  }

  /**
   * Interpolates discrete Gaussian process to have a new, continuous representation as a [[DiscreteLowRankGaussianProcess]].
   * This is achieved by using a nearest neigbor interpolation of the mean function and a Nystrom for computing the kl basis
   * @param nNystromPoints determines how many points of the domain are used to estimate the full
   *                       kl basis.
   */

  def interpolate(nNystromPoints: Int = 2 * rank)(implicit e : CanBound[D]): LowRankGaussianProcess[D, DO] = {

    val sampler = new Sampler[D] {
      override def volumeOfSampleRegion = numberOfPoints.toDouble
      override val numberOfPoints = nNystromPoints
      val p = volumeOfSampleRegion / numberOfPoints
      val randGen = new util.Random()
      val domainPoints = domain.points.toIndexedSeq
      override def sample = {
        val sampledPtIds = for (_ <- 0 until domain.numberOfPoints) yield randGen.nextInt(domain.numberOfPoints)
        sampledPtIds.toIndexedSeq.map(ptId => (domainPoints(ptId), p))
      }
    }

    // TODO, here we could do something smarter, such as e.g. b-spline interpolation
    val meanPD = this.mean
    val kdTreeMap = KDTreeMap.fromSeq(domain.pointsWithId.toIndexedSeq)

    def meanFun(pt : Point[D]) : Vector[DO] = {
      val closestPts = (kdTreeMap.findNearest(pt, n = 1))
      val (closestPt, closestPtId) = closestPts(0)
      meanPD(closestPtId)
    }

    val covFun: MatrixValuedPDKernel[D, DO] = new MatrixValuedPDKernel[D, DO] {
      override val domain = self.domain.boundingBox
      override def k(x: Point[D], y: Point[D]): SquareMatrix[DO] = {
        val closestPtsX = kdTreeMap.findNearest(x, n = 1)
        val (closestX, xId) = closestPtsX(0)
        val closestPtsY = kdTreeMap.findNearest(y, n = 1)
        val (closestY, yId) = closestPtsY(0)
        cov(xId, yId)
      }
    }
    val gp = GaussianProcess(VectorField(domain.boundingBox, meanFun _), covFun)
    LowRankGaussianProcess.approximateGP[D, DO](gp, sampler, rank)
  }

  protected[statisticalmodel] def instanceVector(alpha: DenseVector[Float]): DenseVector[Float] = {
    require(rank == alpha.size)

    basisMatrix * (stddev :* alpha) + meanVector
  }

  protected def vectorToVectorPoindData(vec: DenseVector[Float]): DiscreteVectorField[D, DO] = {
    val vectors =
      for (v <- vec.toArray.grouped(3))
        yield Vector[DO](v)

    DiscreteVectorField[D, DO](domain, vectors.toIndexedSeq)
  }

  private[this] val outputDimensionality = implicitly[NDSpace[DO]].dimensionality

  private[this] val stddev = variance.map(x => math.sqrt(x).toFloat)

}

object DiscreteLowRankGaussianProcess {

  /**
   * Creates a new DiscreteLowRankGaussianProcess by discretizing the given gaussian process at the domain points.
   */
  def apply[D <: Dim: NDSpace, DO <: Dim: NDSpace](domain: FiniteDiscreteDomain[D], gp: LowRankGaussianProcess[D, DO]): DiscreteLowRankGaussianProcess[D, DO] = {
    val points = domain.points.toSeq

    val outputDimensionality = implicitly[NDSpace[DO]].dimensionality

    // precompute all the at the given points
    val (gpLambdas, gpPhis) = gp.klBasis.unzip
    val m = DenseVector.zeros[Float](points.size * outputDimensionality)
    for (xWithIndex <- points.zipWithIndex.par) {
      val (x, i) = xWithIndex
      m(i * outputDimensionality until (i + 1) * outputDimensionality) := gp.mean(x).toBreezeVector
    }

    val U = DenseMatrix.zeros[Float](points.size * outputDimensionality, gp.rank)
    for (xWithIndex <- points.zipWithIndex.par; (phi_j, j) <- gpPhis.zipWithIndex) {
      val (x, i) = xWithIndex
      val v = phi_j(x)
      U(i * outputDimensionality until (i + 1) * outputDimensionality, j) := phi_j(x).toBreezeVector
    }

    val lambdas = new DenseVector[Float](gpLambdas.toArray)
    new DiscreteLowRankGaussianProcess[D, DO](domain, m, lambdas, U)
  }

  /**
   * Discrete implementation of [[DiscreteLowRankGaussianProcess.regression]]
   */
  def regression[D <: Dim: NDSpace, DO <: Dim: NDSpace](gp: DiscreteLowRankGaussianProcess[D, DO],
                                                        trainingData: IndexedSeq[(Int, Vector[DO])],
                                                        sigma2: Double,
                                                        meanOnly: Boolean): DiscreteLowRankGaussianProcess[D, DO] = {
    val tdWithWithPointwiseSigma2 = trainingData.map { case (ptId, v) => (ptId, v, sigma2) }
    regression(gp, tdWithWithPointwiseSigma2, meanOnly)
  }

  /**
   * Discrete implementation of [[DiscreteLowRankGaussianProcess.regression]]
   */
  def regression[D <: Dim: NDSpace, DO <: Dim: NDSpace](gp: DiscreteLowRankGaussianProcess[D, DO],
                                                        trainingData: IndexedSeq[(Int, Vector[DO])],
                                                        sigma2: Double): DiscreteLowRankGaussianProcess[D, DO] = {
    val tdWithWithPointwiseSigma2 = trainingData.map { case (ptId, v) => (ptId, v, sigma2) }
    regression(gp, tdWithWithPointwiseSigma2, false)
  }


  /**
   * Discrete implementation of [[DiscreteLowRankGaussianProcess.regression]]
   */
  def regression[D <: Dim: NDSpace, DO <: Dim: NDSpace](gp: DiscreteLowRankGaussianProcess[D, DO],
                                                        trainingData: IndexedSeq[(Int, Vector[DO], Double)],
                                                        meanOnly: Boolean = false): DiscreteLowRankGaussianProcess[D, DO] = {

    val dim = implicitly[NDSpace[DO]].dimensionality

    val (_Minv, _QtL, yVec, mVec) = genericRegressionComputations(gp, trainingData)
    val mean_coeffs = (_Minv * _QtL).map(_.toFloat) * (yVec - mVec)

    //val mean_p = gp.instance(mean_coeffs)
    val mean_pVector = gp.instanceVector(mean_coeffs)

    if (meanOnly == true) {
      // create an empty gaussian process (not specialized), which is needed in order to be able to construct
      // the specialized one
      val emptyEigenPairs = IndexedSeq[(Float, Point[D] => Vector[DO])]()
      new DiscreteLowRankGaussianProcess[D, DO](gp.domain, mean_pVector, DenseVector.zeros[Float](0), DenseMatrix.zeros[Float](mean_pVector.size, 0))
    } else {
      val D = breeze.linalg.diag(DenseVector(gp.variance.map(math.sqrt(_)).toArray))
      val Sigma = D * _Minv * D
      val SVD(innerUDbl, innerD2, _) = breeze.linalg.svd(Sigma)
      val innerU = innerUDbl.map(_.toFloat)

      val lambdas_p = DenseVector[Float](innerD2.toArray.map(_.toFloat))

      // we do the follwoing computation
      // val eigenMatrix_p = gp.eigenMatrix * innerU // IS this correct?
      // but in parallel
      val eigenMatrix_p = DenseMatrix.zeros[Float](gp.basisMatrix.rows, innerU.cols)
      for (rowInd <- (0 until gp.basisMatrix.rows).par) {

        // TODO maybe this strange transposing can be alleviated? It seems breeze does not support
        // row-vector matrix multiplication
        eigenMatrix_p(rowInd, ::) := (innerU.t * gp.basisMatrix(rowInd, ::).t).t
      }

      new DiscreteLowRankGaussianProcess(gp.domain, mean_pVector, lambdas_p, eigenMatrix_p)
    }
  }

  /**
   * Creates a new DiscreteLowRankGaussianProcess, where the mean and covariance matrix are estimated from the given transformations.
   *
   */
  def createDiscreteLowRankGPFromTransformations[D <: Dim: NDSpace](domain: FiniteDiscreteDomain[D], transformations: Seq[Transformation[D]]): DiscreteLowRankGaussianProcess[D, D] = {
    val dim = implicitly[NDSpace[D]].dimensionality

    val n = transformations.size
    val p = domain.numberOfPoints

    // create the data matrix
    val X = DenseMatrix.zeros[Float](n, p * dim)
    for (p1 <- transformations.zipWithIndex.par; p2 <- domain.pointsWithId) {
      val (t, i) = p1
      val (x, j) = p2
      val ux = t(x) - x
      X(i, j * dim until (j + 1) * dim) := ux.toBreezeVector.t
    }

    def demean(X: DenseMatrix[Float]): (DenseMatrix[Double], DenseVector[Double]) = {
      val X0 = X.map(_.toDouble) // will be the demeaned result matrix
      val m: DenseVector[Double] = breeze.stats.mean(X0(::, *)).toDenseVector
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
    val U: DenseMatrix[Float] = x0.map(_.toFloat).t * vt.map(_.toFloat).t * breeze.linalg.diag(Dinv).map(_.toFloat) / Math.sqrt(n - 1).toFloat

    new DiscreteLowRankGaussianProcess(domain, meanVec.map(_.toFloat), d2.map(_.toFloat), U)

  }

  private def genericRegressionComputations[D <: Dim: NDSpace, DO <: Dim: NDSpace](gp: DiscreteLowRankGaussianProcess[D, DO],
                                                                                   trainingData: IndexedSeq[(Int, Vector[DO], Double)]) = {
    val dim = implicitly[NDSpace[DO]].dimensionality
    val (ptIds, ys, sigma2s) = trainingData.unzip3

    def flatten(v: IndexedSeq[Vector[DO]]) = DenseVector(v.flatten(_.data).toArray)

    val yVec = flatten(ys)
    val meanValues = ptIds.map { ptId =>
      {
        val v = gp.meanVector(ptId * dim until (ptId + 1) * dim).copy
        Vector[DO](v.data)
      }
    }
    val mVec = flatten(meanValues)

    val Q = DenseMatrix.zeros[Double](trainingData.size * dim, gp.rank)
    for ((ptId, i) <- ptIds.zipWithIndex; j <- 0 until gp.rank) {
      val eigenVecAtPoint = gp.basisMatrix((ptId * dim) until ((ptId + 1) * dim), j).map(_.toDouble)
      Q(i * dim until i * dim + dim, j) := eigenVecAtPoint * math.sqrt(gp.variance(j))
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

    val M = QtL * Q + DenseMatrix.eye[Double](gp.rank)
    val Minv = breeze.linalg.pinv(M)

    (Minv, QtL, yVec, mVec)
  }

}

