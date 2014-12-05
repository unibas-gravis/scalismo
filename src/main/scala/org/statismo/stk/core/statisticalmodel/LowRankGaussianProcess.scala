package org.statismo.stk.core.statisticalmodel

import org.statismo.stk.core.geometry._
import org.statismo.stk.core.kernels.{MatrixValuedPDKernel, Kernel}
import org.statismo.stk.core.common.Domain
import org.statismo.stk.core.registration.Transformation
import org.statismo.stk.core.numerics.Sampler
import org.statismo.stk.core.mesh.kdtree.KDTreeMap
import breeze.linalg.{*, Axis, DenseVector, DenseMatrix}
import scala.Some
import breeze.stats.distributions.Gaussian
import breeze.stats.mean

case class LowRankGaussianProcessConfiguration[D <: Dim](
                                                          val domain: Domain[D],
                                                          val sampler: Sampler[D, Point[D]],
                                                          val mean: Point[D] => Vector[D],
                                                          val cov: MatrixValuedPDKernel[D, D],
                                                          val numBasisFunctions: Int)

class LowRankGaussianProcess[D <: Dim: DimTraits](domain: Domain[D],
                                                  mean: Point[D] => Vector[D],
                                                  val eigenPairs: IndexedSeq[(Float, Point[D] => Vector[D])])
  extends GaussianProcess[D](domain, mean, LowRankGaussianProcess.covFromEigenpairs(eigenPairs)) {

  def rank = eigenPairs.size

  def instance(alpha: DenseVector[Float]): Point[D] => Vector[D] = {
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

  def sample: Point[D] => Vector[D] = {
    val coeffs = for (_ <- 0 until eigenPairs.size) yield Gaussian(0, 1).draw().toFloat
    instance(DenseVector(coeffs.toArray))
  }



  override def sampleAtPoints(pts : Seq[Point[D]]) : Seq[(Point[D], Vector[D])] = {
    val aSample = sample
    pts.map(pt => (pt, aSample(pt)))
  }

  def jacobian(p: DenseVector[Float]) = { x: Point[D] =>
    val dim = x.dimensionality
    val J = DenseMatrix.zeros[Float](dim, eigenPairs.size)
    (0 until eigenPairs.size).map(i => {
      val (lambda_i, phi_i) = eigenPairs(i)
      J(::, i) := (phi_i(x) * math.sqrt(lambda_i).toFloat).toBreezeVector
    })
    J
  }

  /**
   * returns a new gaussian process, where the values for the points are
   * pre-computed and hence fast to obtain
   */
  def specializeForPoints(points: IndexedSeq[Point[D]]) = {
    SpecializedLowRankGaussianProcess(this, points)
  }



  def project(trainingData : IndexedSeq[(Point[D], Vector[D])], sigma2 : Double = 1e-6) : (Point[D] => Vector[D])  = {
    val newtd = trainingData.map { case (pt, df) => (pt, df, sigma2) }
    project(newtd)
  }

  def project(trainingData : IndexedSeq[(Point[D], Vector[D], Double)]) : Point[D] => Vector[D] = {
    val c = coefficients(trainingData)
    instance(c)
  }

  /**
   * Compute the coefficients alpha, that represent the given trainingData u best under this gp (in the least squares sense)
   * e.g. \sum_i alpha_i \lambda_i \phi_i(x) = u(x)
   */
  def coefficients(trainingData : IndexedSeq[(Point[D], Vector[D], Double)]) : DenseVector[Float] = {
    val (minv, qtL, yVec, mVec) = GaussianProcess.genericRegressionComputations(this, trainingData)
    val mean_coeffs = (minv * qtL).map(_.toFloat) * (yVec - mVec)
    mean_coeffs
  }

  def coefficients(trainingData : IndexedSeq[(Point[D], Vector[D])], sigma2 : Double) : DenseVector[Float] = {
    val newtd = trainingData.map { case (pt, df) => (pt, df, sigma2) }
    coefficients(newtd)
  }
}

class SpecializedLowRankGaussianProcess[D <: Dim: DimTraits](gp: LowRankGaussianProcess[D], val points: IndexedSeq[Point[D]], val meanVector: DenseVector[Float], val lambdas: IndexedSeq[Float], val eigenMatrix: DenseMatrix[Float])
  extends LowRankGaussianProcess[D](gp.domain, gp.mean, gp.eigenPairs) {

  private val (gpLambdas, gpPhis) = gp.eigenPairs.unzip
  private val pointToIdxMap = points.zipWithIndex.toMap
  private val stddev = DenseVector(lambdas.map(x => math.sqrt(x).toFloat).toArray)
  private val phis = (0 until lambdas.size).map(i => phiAtPoint(i)_)

  // define all fields required to qualify as a "ordinary" LowRankGaussianProcess
  override val domain = gp.domain
  override val eigenPairs = lambdas.zip(phis)
  override val mean = meanAtPoint _

  override val cov: MatrixValuedPDKernel[D, D] = {
    new MatrixValuedPDKernel[D, D] {
      def apply(x: Point[D], y: Point[D]): MatrixNxN[D] = {

        // if pt is in our map, we compute it efficiently, otherwise we
        // fall back to generic version
        val cov: Option[MatrixNxN[D]] = for {
          ptId1 <- pointToIdxMap.get(x)
          ptId2 <- pointToIdxMap.get(y)
        } yield computeCov(ptId1, ptId2)

        cov.getOrElse(gp.cov(x, y))
      }
    }
  }

  def instanceAtPoints(alpha: DenseVector[Float]): IndexedSeq[(Point[D], Vector[D])] = {
    require(eigenPairs.size == alpha.size)
    val instVal = instanceVector(alpha)
    val ptVals = for (v <- instVal.toArray.grouped(outputDimensionality)) yield dimTraits.createVector(v)
    points.zip(ptVals.toIndexedSeq)
  }

  def instanceVector(alpha: DenseVector[Float]): DenseVector[Float] = {
    require(eigenPairs.size == alpha.size)

    eigenMatrix * (stddev :* alpha) + meanVector
  }

  def sampleAtPoints: IndexedSeq[(Point[D], Vector[D])] = {
    val coeffs = for (_ <- 0 until eigenPairs.size) yield Gaussian(0, 1).draw().toFloat
    instanceAtPoints(DenseVector(coeffs.toArray))
  }

  private def meanAtPoint(pt: Point[D]): Vector[D] = {
    pointToIdxMap.get(pt) match {
      case Some(ptId) => {
        // we need the copy here, as otherwise vec.data will be the array of the
        // original vector (from which we extracted a slice)
        val vec = meanVector(ptId * outputDimensionality until (ptId + 1) * outputDimensionality).copy
        dimTraits.createVector(vec.data)
      }
      case None => gp.mean(pt)
    }
  }

  private def phiAtPoint(i: Int)(pt: Point[D]): Vector[D] = {
    val dimTraits = implicitly[DimTraits[D]]
    pointToIdxMap.get(pt) match {
      case Some(ptId) => {
        // we need the copy here, as otherwise vec.data will be the array of the
        // original vector (from which we extracted a slice)
        val df = eigenMatrix(ptId * gp.outputDimensionality until (ptId + 1) * gp.outputDimensionality, i).copy
        dimTraits.createVector(df.data)
      }
      case None => gpPhis(i)(pt)
    }
  }

  //compute covariance for two points with given ptIds
  private def computeCov(ptId1: Int, ptId2: Int) : MatrixNxN[D]= {
    val eigenMatrixForPtId1 = eigenMatrix(ptId1 * outputDimensionality until (ptId1 + 1) * outputDimensionality, ::)
    val eigenMatrixForPtId2 = eigenMatrix(ptId2 * outputDimensionality until (ptId2 + 1) * outputDimensionality, ::)
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
        while (k < eigenMatrix.cols) {
          valueIJ += eigenMatrix(ind1, k) * eigenMatrix(ind2, k) * lambdas(k)
          k += 1
        }
        covValue(i, j) = valueIJ
        j += 1
      }
    }

    dimTraits.createMatrixNxN(covValue.data)
  }

}

object SpecializedLowRankGaussianProcess {
  def apply[D <: Dim: DimTraits](gp: LowRankGaussianProcess[D], points: IndexedSeq[Point[D]]) = {

    // precompute all the at the given points
    val (gpLambdas, gpPhis) = gp.eigenPairs.unzip
    val m = DenseVector.zeros[Float](points.size * gp.outputDimensionality)
    for (xWithIndex <- points.zipWithIndex.par) {
      val (x, i) = xWithIndex
      m(i * gp.outputDimensionality until (i + 1) * gp.outputDimensionality) := gp.mean(x).toBreezeVector
    }

    val U = DenseMatrix.zeros[Float](points.size * gp.outputDimensionality, gp.rank)
    for (xWithIndex <- points.zipWithIndex.par; (phi_j, j) <- gpPhis.zipWithIndex) {
      val (x, i) = xWithIndex
      val v = phi_j(x)
      U(i * gp.outputDimensionality until (i + 1) * gp.outputDimensionality, j) := phi_j(x).toBreezeVector
    }

    new SpecializedLowRankGaussianProcess(gp, points, m, gpLambdas, U)
  }

}

class LowRankGaussianProcess1D(
                                domain: Domain[OneD],
                                mean: Point[OneD] => Vector[OneD],
                                eigenPairs: IndexedSeq[(Float, Point[OneD] => Vector[OneD])])
  extends LowRankGaussianProcess[OneD](domain, mean, eigenPairs) {}

class LowRankGaussianProcess2D(
                                domain: Domain[TwoD],
                                mean: Point[TwoD] => Vector[TwoD],
                                eigenPairs: IndexedSeq[(Float, Point[TwoD] => Vector[TwoD])])
  extends LowRankGaussianProcess[TwoD](domain, mean, eigenPairs) { }

class LowRankGaussianProcess3D(
                                domain: Domain[ThreeD],
                                mean: Point[ThreeD] => Vector[ThreeD],
                                eigenPairs: IndexedSeq[(Float, Point[ThreeD] => Vector[ThreeD])])
  extends LowRankGaussianProcess[ThreeD](domain, mean, eigenPairs) {}



object LowRankGaussianProcess {
  def createLowRankGaussianProcess1D(configuration: LowRankGaussianProcessConfiguration[OneD]) = {
    val eigenPairs = Kernel.computeNystromApproximation(configuration.cov, configuration.numBasisFunctions, configuration.sampler)
    new LowRankGaussianProcess1D(configuration.domain, configuration.mean, eigenPairs)
  }

  def createLowRankGaussianProcess2D(configuration: LowRankGaussianProcessConfiguration[TwoD]) = {
    val eigenPairs = Kernel.computeNystromApproximation(configuration.cov, configuration.numBasisFunctions, configuration.sampler)
    new LowRankGaussianProcess2D(configuration.domain, configuration.mean, eigenPairs)
  }

  def createLowRankGaussianProcess3D(configuration: LowRankGaussianProcessConfiguration[ThreeD]) = {
    val eigenPairs = Kernel.computeNystromApproximation(configuration.cov, configuration.numBasisFunctions, configuration.sampler)
    new LowRankGaussianProcess3D(configuration.domain, configuration.mean, eigenPairs)
  }


  private def covFromEigenpairs[D <: Dim : DimTraits](eigenPairs : IndexedSeq[(Float, Point[D] => Vector[D])]) : MatrixValuedPDKernel[D, D]= {
    val dimTraits = implicitly[DimTraits[D]]
    val cov: MatrixValuedPDKernel[D, D] = new MatrixValuedPDKernel[D, D] {
      def apply(x: Point[D], y: Point[D]): MatrixNxN[D] = {
        val ptDim = dimTraits.dimensionality
        val phis = eigenPairs.map(_._2)

        var outer = MatrixNxN.zeros[D]
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
  def createLowRankGPFromTransformations[D <: Dim: DimTraits](domain: Domain[D], transformations: Seq[Transformation[D]], sampler: Sampler[D, Point[D]]): LowRankGaussianProcess[D] = {
    val dimTraits = implicitly[DimTraits[D]]
    val dim = dimTraits.dimensionality

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
    for ((t, i) <- transformations.zipWithIndex.par; (x, j) <- samplePts.zipWithIndex) {
      val ux = t(x) - x
      X(i, j * dim until (j + 1) * dim) := ux.toBreezeVector.t
    }

    def demean(X: DenseMatrix[Float]): (DenseMatrix[Double], DenseVector[Double]) = {
      val X0 = X.map(_.toDouble) // will be the demeaned result matrix
      val m : DenseVector[Double] = mean(X0(::, *)).toDenseVector
      for (i <- 0 until X0.rows) {
        X0(i, ::) := X0(i, ::) - m.t
      }
      (X0, m)
    }

    val (x0, meanVec) = demean(X)
    val (u, d2, vt) = breeze.linalg.svd(x0 * x0.t * (1.0 / (n - 1)))

    val D = d2.map(v => Math.sqrt(v))
    val Dinv = D.map(d => if (d > 1e-6) 1.0 / d else 0.0)

    // a Matrix with the eigenvectors
    val U = x0.t * vt.t * breeze.linalg.diag(Dinv) / Math.sqrt(n - 1)

    // to compensate for the numerical approximation using the sampled poitns
    val normFactor = sampler.volumeOfSampleRegion / sampler.numberOfPoints

    def interpolateAtPoint(x: Point[D], dataVec: DenseVector[Double]): Vector[D] = {
      val nNbrPts = Math.pow(2, dim).toInt
      val ptAndIds = findClosestPoints(x, nNbrPts)

      var v = dimTraits.zeroVector
      var sumW = 0.0
      for ((pt, id) <- ptAndIds) {
        val w = 1.0 / math.max((pt - x).norm, 1e-5)
        v += dimTraits.createVector(dataVec(id * dim until (id + 1) * dim).map(_.toFloat).data) * w
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
    new LowRankGaussianProcess[D](domain, mu _, eigenPairs)
  }

}
