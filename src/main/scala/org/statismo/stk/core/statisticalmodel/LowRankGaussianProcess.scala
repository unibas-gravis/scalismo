package org.statismo.stk.core.statisticalmodel

import breeze.linalg.svd.SVD
import org.statismo.stk.core.geometry._
import org.statismo.stk.core.kernels.{MatrixValuedPDKernel, Kernel}
import org.statismo.stk.core.common.{ImmutableLRU, FiniteDiscreteDomain, DiscreteDomain, Domain}
import org.statismo.stk.core.registration.{RigidTransformation, Transformation}
import org.statismo.stk.core.numerics.Sampler
import org.statismo.stk.core.mesh.kdtree.KDTreeMap
import breeze.linalg.{*, Axis, DenseVector, DenseMatrix}
import breeze.stats.distributions.Gaussian
import breeze.stats.mean


class LowRankGaussianProcess[DI <: Dim : NDSpace, DO <: Dim : NDSpace](domain: Domain[DI],
                                                                       mean: Point[DI] => Vector[DO],
                                                                       val eigenPairs: IndexedSeq[(Float, Point[DI] => Vector[DO])])
  extends GaussianProcess[DI, DO](domain, mean, LowRankGaussianProcess.covFromEigenpairs(eigenPairs)) {

  def rank = eigenPairs.size

  def instance(alpha: DenseVector[Float]): Point[DI] => Vector[DO] = {
    require(eigenPairs.size == alpha.size)
    x => {
      val deformationsAtX = (0 until eigenPairs.size).map(i => {
        val (lambda_i, phi_i) = eigenPairs(i)
        phi_i(x) * alpha(i) * math.sqrt(lambda_i).toFloat
      })
      deformationsAtX.foldLeft(mean(x))(_ + _)
    }
  }

  def sample: Point[DI] => Vector[DO] = {
    val coeffs = for (_ <- 0 until eigenPairs.size) yield Gaussian(0, 1).draw().toFloat
    instance(DenseVector(coeffs.toArray))
  }


  override def sampleAtPoints(pts: Seq[Point[DI]]): Seq[(Point[DI], Vector[DO])] = {
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

  def project(trainingData: IndexedSeq[(Point[DI], Vector[DO])], sigma2: Double = 1e-6): (Point[DI] => Vector[DO]) = {
    val newtd = trainingData.map { case (pt, df) => (pt, df, sigma2)}
    project(newtd)
  }

  def project(trainingData: IndexedSeq[(Point[DI], Vector[DO], Double)]): Point[DI] => Vector[DO] = {
    val c = coefficients(trainingData)
    instance(c)
  }

  /**
   * Compute the coefficients alpha, that represent the given trainingData u best under this gp (in the least squares sense)
   * e.g. \sum_i alpha_i \lambda_i \phi_i(x) = u(x)
   */
  def coefficients(trainingData: IndexedSeq[(Point[DI], Vector[DO], Double)]): DenseVector[Float] =
  {
     val (minv, qtL, yVec, mVec) = LowRankGaussianProcess.genericRegressionComputations(this, trainingData)
     val mean_coeffs = (minv * qtL).map(_.toFloat) * (yVec - mVec)
     mean_coeffs
   }

  def coefficients(trainingData: IndexedSeq[(Point[DI], Vector[DO])], sigma2: Double): DenseVector[Float] = {
    val newtd = trainingData.map { case (pt, df) => (pt, df, sigma2)}
    coefficients(newtd)
  }


  def posterior(trainingData: IndexedSeq[(Point[DI], Vector[DO])], sigma2: Double) : LowRankGaussianProcess[DI, DO] = {
    val newtd = trainingData.map { case (pt, df) => (pt, df, sigma2)}
    posterior(newtd)
  }

  def posterior(trainingData: IndexedSeq[(Point[DI], Vector[DO], Double)]) : LowRankGaussianProcess[DI, DO] = {
    LowRankGaussianProcess.regression(this, trainingData)
  }
}


object LowRankGaussianProcess {
  def createLowRankGaussianProcess[D <: Dim : NDSpace](
                                                        domain: Domain[D],
                                                        sampler: Sampler[D],
                                                        mean: Point[D] => Vector[D],
                                                        cov: MatrixValuedPDKernel[D, D],
                                                        numBasisFunctions: Int) = {
    val eigenPairs = Kernel.computeNystromApproximation(cov, numBasisFunctions, sampler)
    new LowRankGaussianProcess[D, D](domain, mean, eigenPairs)
  }


  private def covFromEigenpairs[D <: Dim : NDSpace, DO <: Dim : NDSpace](eigenPairs: IndexedSeq[(Float, Point[D] => Vector[DO])]): MatrixValuedPDKernel[D, DO] = {
    val dimOps = implicitly[NDSpace[DO]]
    val cov: MatrixValuedPDKernel[D, DO] = new MatrixValuedPDKernel[D, DO] {
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
  def createLowRankGPFromTransformations[D <: Dim : NDSpace](domain: Domain[D], transformations: Seq[Transformation[D]], sampler: Sampler[D]): LowRankGaussianProcess[D, D] = {
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


  // Gaussian process regression for a low rank gaussian process
  // Note that this implementation is literally the same as the one for the specializedLowRankGaussian process. The difference is just the return type.
  // TODO maybe the implementations can be joined.
  def regression[D <: Dim : NDSpace, DO <: Dim : NDSpace](gp: LowRankGaussianProcess[D, DO],
                                                          trainingData: IndexedSeq[(Point[D], Vector[DO])],
                                                          sigma2: Double,
                                                          meanOnly: Boolean = false)
  : LowRankGaussianProcess[D, DO] = {

    val trainingDataWithNoise = trainingData.map { case (x, y) => (x, y, sigma2)}
    regression(gp, trainingDataWithNoise, meanOnly)
  }

  def regression[D <: Dim : NDSpace, DO <: Dim : NDSpace](gp: LowRankGaussianProcess[D, DO],
                                                          trainingData: IndexedSeq[(Point[D], Vector[DO], Double)],
                                                          meanOnly: Boolean = false)
  : LowRankGaussianProcess[D, DO] = {
    val outputDim = implicitly[NDSpace[DO]].dimensionality

    val (lambdas, phis) = gp.eigenPairs.unzip
    val (_Minv, _QtL, yVec, mVec) = genericRegressionComputations(gp, trainingData)
    val mean_coeffs = (_Minv * _QtL).map(_.toFloat) * (yVec - mVec)

    val mean_p = gp.instance(mean_coeffs)

    if (meanOnly == true) {
      val emptyEigenPairs = IndexedSeq[(Float, Point[D] => Vector[DO])]()
      new LowRankGaussianProcess(gp.domain, mean_p, emptyEigenPairs)

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

      val phis_p = for (i <- 0 until phis.size) yield ((x: Point[D]) => phip(i)(x))
      val lambdas_p = innerD2.toArray.map(_.toFloat).toIndexedSeq
      new LowRankGaussianProcess[D, DO](gp.domain, mean_p, lambdas_p.zip(phis_p))
    }
  }

  private def genericRegressionComputations[D <: Dim : NDSpace, DO <: Dim : NDSpace](gp: LowRankGaussianProcess[D, DO],
                                                                                     trainingData: IndexedSeq[(Point[D], Vector[DO], Double)],
                                                                                     meanOnly: Boolean = false) = {

    val (lambdas, phis) = gp.eigenPairs.unzip
    val outputDim = gp.outputDimensionality


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
   * create a statisticalMeshModel which is transformed by the given rigid transform
   * TODO - Part of this functionality should be moved into the GP. But for this we would have to define
   * a proper domain-warp concept!
   */


  def transform[D <: Dim : NDSpace](gp: LowRankGaussianProcess[D, D], rigidTransform: RigidTransformation[D]): LowRankGaussianProcess[D, D] = {
    val invTransform = rigidTransform.inverse

    val newDomain = gp.domain.warp(rigidTransform)
    val (lambdas, phis) = gp.eigenPairs.unzip

    def newMean(pt: Point[D]): Vector[D] = {
      val ptOrigGp = invTransform(pt)
      rigidTransform(ptOrigGp + gp.mean(ptOrigGp)) - rigidTransform(ptOrigGp)
    }

    val newPhis = phis.map(phi => {
      def newPhi(pt: Point[D]): Vector[D] = {
        val ptOrigGp = invTransform(pt)
        rigidTransform(ptOrigGp + phi(ptOrigGp)) - pt
      }
      newPhi _
    })

    val newEigenpairs = lambdas.zip(newPhis)


    new LowRankGaussianProcess[D, D](newDomain, newMean _, newEigenpairs)
  }


}
