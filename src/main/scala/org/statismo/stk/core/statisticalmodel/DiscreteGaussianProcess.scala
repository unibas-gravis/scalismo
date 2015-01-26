package org.statismo.stk.core.statisticalmodel

import breeze.linalg.svd.SVD
import breeze.linalg.{*, DenseMatrix, DenseVector}
import breeze.stats.distributions.Gaussian
import org.statismo.stk.core.common._
import org.statismo.stk.core.geometry._
import org.statismo.stk.core.kernels.MatrixValuedPDKernel
import org.statismo.stk.core.mesh.kdtree.KDTreeMap
import org.statismo.stk.core.numerics.Sampler
import org.statismo.stk.core.registration.{Transformation, RigidTransformation}


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
 * @see [[VectorPointData[D, DO]]
 * @see [[LowRankGaussianProcess[D, DO]]
 */
case class DiscreteLowRankGaussianProcess[D <: Dim : NDSpace, DO <: Dim : NDSpace] private[core] (val domain : FiniteDiscreteDomain[D],
                                                                                       val meanVector : DenseVector[Float],
                                                                                       val variance : DenseVector[Float],
                                                                                       val basisMatrix : DenseMatrix[Float]) {

  /** the number of basis functions that define this Gaussian process */
  val rank: Int = basisMatrix.cols

  /** The mean functions, defined on the given discrete domain */
  def mean: VectorPointData[D, DO] = vectorToVectorPoindData(meanVector)

  /**
   * A function, which returns for two given points, defined by their pointId, the covariance.
   */
  val cov: (Int, Int) => SquareMatrix[DO] =  (ptId1, ptId2) => {
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
    * Discrete version of [[LowRankGaussianProcess.instance]]
    */
  def instance(c: DenseVector[Float]): VectorPointData[D, DO] = {
    require(rank == c.size)
    val instVal = instanceVector(c)
    vectorToVectorPoindData(instVal)
  }

  /**
   * Discrete version of [[LowRankGaussianProcess.sample]]
   */
  def sample: VectorPointData[D, DO] = {
    val coeffs = for (_ <- 0 until rank) yield Gaussian(0, 1).draw().toFloat
    instance(DenseVector(coeffs.toArray))
  }

  /**
   * Returns the variance and associated basis function that defines the process.
   * The basis is the (discretized) Karhunen Loeve basis (e.g. it is obtained from a Mercer's decomposition of the covariance function
   */
  def klBasis : IndexedSeq[(Float, VectorPointData[D, DO])] = {
    for (i <- 0 until rank) yield
      (variance(i), vectorToVectorPoindData(basisMatrix(::,i).toDenseVector))
  }


  /**
   * Discrete version of [[LowRankGaussianProcess.coefficients(IndexedSeq[(Point[D], Vector[DO], Double)])]]
   */
  def coefficients(trainingData: IndexedSeq[(Int, Vector[DO], Double)]): DenseVector[Float] = {

    val (minv, qtL, yVec, mVec) = DiscreteLowRankGaussianProcess.genericRegressionComputations(this, trainingData)
    val mean_coeffs = (minv * qtL).map(_.toFloat) * (yVec - mVec)
    mean_coeffs
  }


  /**
   * Discrete version of [[LowRankGaussianProcess.coefficients(IndexedSeq[(Point[D], Vector[DO])], Double)]]
   */
  def coefficients(trainingData: IndexedSeq[(Int, Vector[DO])], sigma2: Double): DenseVector[Float] = {
    val newtd = trainingData.map { case (pt, df) => (pt, df, sigma2)}
    coefficients(newtd)
  }


  /**
   * The marginal distribution for the point with the given pointId 
   */
  def marginal(pointId: Int) = {
    val meanAtPoint = meanVector(pointId * outputDimensionality until (pointId + 1) * outputDimensionality).copy
    NDimensionalNormalDistribution(Vector[DO](meanAtPoint.data), cov(pointId, pointId))
  }

  /**
   * Discrete version of [[LowRankGaussianProcess.posterior(IndexedSeq[(Point[D], Vector[DO])], sigma2: Double]]. In contrast to this method, the points for the training
   * data are defined by the pointId. The returned posterior process is defined at the same points.
   *
   */
  def posterior(trainingData: IndexedSeq[(Int, Vector[DO])], sigma2: Double) : DiscreteLowRankGaussianProcess[D, DO] = {
    val newtd = trainingData.map { case (ptId, df) => (ptId, df, sigma2)}
    posterior(newtd)
  }

  /**
   * Discrete version of [[LowRankGaussianProcess.posterior(IndexedSeq[(Point[D], Vector[DO], Double)])]]. In contrast to this method, the points for the training
   * data are defined by the pointId. The returned posterior process is defined at the same points.
   *
   */
  def posterior(trainingData: IndexedSeq[(Int, Vector[DO], Double)]) : DiscreteLowRankGaussianProcess[D, DO] = {
    DiscreteLowRankGaussianProcess.regression(this, trainingData, false)
  }

  /**
   *
   * @param nNystromPoints
   * @return
   */
  def interpolate(nNystromPoints : Int = 2 * rank) : LowRankGaussianProcess[D, DO] = {

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

    val covFun : MatrixValuedPDKernel[D, DO] = new MatrixValuedPDKernel[D, DO] {
      override def apply(x: Point[D], y: Point[D]): SquareMatrix[DO] = {
        val closestPtsX = kdTreeMap.findNearest(x, n = 1)
        val (closestX, xId) = closestPtsX(0)
        val closestPtsY = kdTreeMap.findNearest(y, n = 1)
        val (closestY, yId) = closestPtsY(0)
        cov(xId, yId)
      }
    }
    val gp = GaussianProcess(domain.boundingBox, meanFun _, covFun)
    LowRankGaussianProcess.approximateGP[D, DO](gp, sampler, rank)
  }


  protected[statisticalmodel] def instanceVector(alpha: DenseVector[Float]): DenseVector[Float] = {
    require(rank == alpha.size)

    basisMatrix * (stddev :* alpha) + meanVector
  }


  protected def vectorToVectorPoindData(vec: DenseVector[Float]): VectorPointData[D, DO] = {
    val vectors =
      for (v <- vec.toArray.grouped(3))
      yield Vector[DO](v)

    VectorPointData[D, DO](domain, vectors.toIndexedSeq)
  }

  private[this] val outputDimensionality = implicitly[NDSpace[DO]].dimensionality

  private[this] val stddev = variance.map(x => math.sqrt(x).toFloat)


}

object DiscreteLowRankGaussianProcess {

  def apply[D <: Dim : NDSpace, DO <: Dim : NDSpace](domain : FiniteDiscreteDomain[D], gp : LowRankGaussianProcess[D, DO]) : DiscreteLowRankGaussianProcess[D, DO] = {
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

  def regression[D <: Dim : NDSpace, DO <: Dim : NDSpace](gp: DiscreteLowRankGaussianProcess[D, DO],
                                     trainingData: IndexedSeq[(Int, Vector[DO])],
                                     sigma2 : Double,
                                     meanOnly: Boolean = false): DiscreteLowRankGaussianProcess[D, DO] = {
    val tdWithWithPointwiseSigma2 = trainingData.map{case(ptId, v) => (ptId, v, sigma2)}
    regression(gp, tdWithWithPointwiseSigma2, meanOnly)
  }

  /**
   * TODO - Check if these computations assume orthogonality
   */
  def regression[D <: Dim : NDSpace, DO <: Dim : NDSpace](gp: DiscreteLowRankGaussianProcess[D, DO],
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
  def createLowRankGPFromTransformations[D <: Dim : NDSpace](domain: Domain[D], transformations: Seq[Transformation[D]], sampler: Sampler[D]): LowRankGaussianProcess[D, D] = ??? //{
//    val dim = implicitly[NDSpace[D]].dimensionality
//
//    val samplePts = sampler.sample.map(_._1)
//
//    val kdTreeMap = KDTreeMap.fromSeq(samplePts.zipWithIndex.toIndexedSeq)
//
//    def findClosestPoint(pt: Point[D]): (Point[D], Int) = {
//      val nearestPtsAndIndices = (kdTreeMap.findNearest(pt, n = 1))
//      nearestPtsAndIndices(0)
//    }
//    def findClosestPoints(pt: Point[D], n: Int): Seq[(Point[D], Int)] = {
//      val nearestPtsAndIndices = (kdTreeMap.findNearest(pt, n))
//      nearestPtsAndIndices
//    }
//
//    val n = transformations.size
//    val p = samplePts.size
//
//    // create the data matrix
//    val X = DenseMatrix.zeros[Float](n, p * dim)
//    for (p1 <- transformations.zipWithIndex.par; p2 <- samplePts.zipWithIndex) {
//      val (t, i) = p1
//      val (x, j) = p2
//      val ux = t(x) - x
//      X(i, j * dim until (j + 1) * dim) := ux.toBreezeVector.t
//    }
//
//    def demean(X: DenseMatrix[Float]): (DenseMatrix[Double], DenseVector[Double]) = {
//      val X0 = X.map(_.toDouble) // will be the demeaned result matrix
//      val m: DenseVector[Double] = mean(X0(::, *)).toDenseVector
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
//    val U = x0.t * vt.t * breeze.linalg.diag(Dinv) / Math.sqrt(n - 1)
//
//    // to compensate for the numerical approximation using the sampled poitns
//    val normFactor = sampler.volumeOfSampleRegion / sampler.numberOfPoints
//
//    def interpolateAtPoint(x: Point[D], dataVec: DenseVector[Double]): Vector[D] = {
//      val nNbrPts = Math.pow(2, dim).toInt
//      val ptAndIds = findClosestPoints(x, nNbrPts)
//
//      var v = Vector.zeros[D]
//      var sumW = 0.0
//      for ((pt, id) <- ptAndIds) {
//        val w = 1.0 / math.max((pt - x).norm, 1e-5)
//        v += Vector[D](dataVec(id * dim until (id + 1) * dim).map(_.toFloat).data) * w
//        sumW += w
//      }
//      v * (1.0 / sumW)
//    }
//    def mu(x: Point[D]): Vector[D] = {
//      interpolateAtPoint(x, meanVec)
//    }
//    def phi(i: Int)(x: Point[D]): Vector[D] = {
//      interpolateAtPoint(x, U(::, i)) * Math.sqrt(1.0 / normFactor)
//    }
//
//    val lambdas = d2.toArray.toIndexedSeq.map(l => (l * normFactor).toFloat)
//    val phis = (0 until n).map(i => phi(i) _)
//    val eigenPairs = lambdas zip phis
//    new LowRankGaussianProcess[D, D](domain, mu _, eigenPairs)
//  }




  private def genericRegressionComputations[D <: Dim : NDSpace, DO <: Dim : NDSpace](gp: DiscreteLowRankGaussianProcess[D, DO],
                                                                                     trainingData: IndexedSeq[(Int, Vector[DO], Double)])  = {
    val dim = implicitly[NDSpace[DO]].dimensionality
    val (ptIds, ys, sigma2s) = trainingData.unzip3


    def flatten(v: IndexedSeq[Vector[DO]]) = DenseVector(v.flatten(_.data).toArray)


    val yVec = flatten(ys)
    val meanValues = ptIds.map { ptId => {
      val v = gp.meanVector(ptId * dim until (ptId+ 1) * dim).copy
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

