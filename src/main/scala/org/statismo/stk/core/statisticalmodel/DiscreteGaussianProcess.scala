package org.statismo.stk.core.statisticalmodel

import org.statismo.stk.core.geometry.Vector.VectorFactory
import org.statismo.stk.core.geometry._
import org.statismo.stk.core.common.Domain
import org.statismo.stk.core.numerics.Sampler
import org.statismo.stk.core.kernels._
import breeze.linalg.{DenseMatrix, DenseVector}
import breeze.stats.distributions.Gaussian
import org.statismo.stk.core.geometry.Vector
import org.statismo.stk.core.kernels.LandmarkKernel
import org.statismo.stk.core.kernels.LandmarkKernelNonRepeatingPoints

case class DiscreteGaussianProcessConfiguration[D <: Dim] (

                                                            val domain: Domain[D],
                                                            val sampler: Sampler[D],
                                                            val mean: Point[D] => Vector[D],
                                                            val points: IndexedSeq[Point[D]],
                                                            val cov: MatrixValuedPDKernel[D,D])

case class DiscreteGaussianProcess[D <: Dim: VectorFactory : ToInt](val domain: Domain[D],
                                                        val mean: Point[D] => Vector[D], val points: IndexedSeq[Point[D]], val cov: MatrixValuedPDKernel[D,D]) extends GaussianProcess[D] {


  def outputDim = cov.outputDim
  private def N = points.size
  override val rank = points.size*outputDim

  def instance(alpha: DenseVector[Float]): Point[D] => Vector[D] = {

    require(rank == alpha.size)

    val coefficients = alpha.valuesIterator.grouped(outputDim).toIndexedSeq.map(x => Vector(x.toArray))

    // Reshape alpha 1x(NxD) Vector to NxD Vector

    def kalpha(x: Point[D]) : Vector[D] = {

      val zeroVector = Vector.zeros[D]
      (0 until points.size).map { i =>  cov(x,points(i)) * coefficients(i) }.fold(zeroVector)((a, b) => { a + b })

    }

    x =>
    {
      mean(x) + kalpha(x)
    }
  }

  def sample: Point[D] => Vector[D] = {
    val coeffs = for (_ <- 0 until rank) yield Gaussian(0, 1).draw().toFloat
    instance(DenseVector(coeffs.toArray))
  }

  def jacobian(p: DenseVector[Float]) = { x: Point[D] =>
    val dim = x.dimensionality
    val J = DenseMatrix.zeros[Float](dim, points.size*dim)
    (0 until points.size).map(i => {
      J(::,(i*dim) until (i*dim + dim)) := cov(points(i),x).toBreezeMatrix
    })
    J
  }

  /**
   * Compute the marginal distribution for the given point
   */

  def marginal(pt: Point[D]): MVNormalForPoint[D] = {
    MVNormalForPoint(pt, mean(pt), cov(pt, pt))
  }


}


object DiscreteGaussianProcess {

  def createDiscreteGaussianProcess1D(configuration: DiscreteGaussianProcessConfiguration[OneD]) = {
    new DiscreteGaussianProcess[OneD](configuration.domain, configuration.mean, configuration.points, configuration.cov)
  }

  def createDiscreteGaussianProcess2D(configuration: DiscreteGaussianProcessConfiguration[TwoD]) = {
    new DiscreteGaussianProcess[TwoD](configuration.domain, configuration.mean, configuration.points, configuration.cov)
  }

  def createDiscreteGaussianProcess3D(configuration: DiscreteGaussianProcessConfiguration[ThreeD]) = {
    new DiscreteGaussianProcess[ThreeD](configuration.domain, configuration.mean, configuration.points, configuration.cov)
  }

  def createLandmarkDiscreteGaussianProcess1D(configuration: DiscreteGaussianProcessConfiguration[OneD], trainingData: IndexedSeq[(Point[OneD], Vector[OneD])], sigma2: Double) = {

    val trainingDataWithNoise = trainingData.map {
      case (x, y) => (x, y, sigma2)
    }
    val nPoints = configuration.points.length
    val mean: Point[OneD] => Vector[OneD] = createLandmarkMean(trainingDataWithNoise, configuration.cov, configuration.mean)
    val landmarkCov: MatrixValuedPDKernel[OneD, OneD] = LandmarkKernel[OneD](configuration.cov, trainingDataWithNoise, nPoints * nPoints)

    new DiscreteGaussianProcess[OneD](configuration.domain, mean, configuration.sampler.sample.map(_._1), landmarkCov)
  }

  def createLandmarkDiscreteGaussianProcess2D(configuration: DiscreteGaussianProcessConfiguration[TwoD], trainingData: IndexedSeq[(Point[TwoD], Vector[TwoD])], sigma2: Double) = {

    val trainingDataWithNoise = trainingData.map {
      case (x, y) => (x, y, sigma2)
    }
    val nPoints = configuration.points.length
    val mean: Point[TwoD] => Vector[TwoD] = createLandmarkMean(trainingDataWithNoise, configuration.cov, configuration.mean)
    val landmarkCov = LandmarkKernel[TwoD](configuration.cov, trainingDataWithNoise, nPoints * nPoints)
    new DiscreteGaussianProcess[TwoD](configuration.domain, mean, configuration.points, landmarkCov)
  }

  def createLandmarkBaseGaussianProcess3D(configuration: DiscreteGaussianProcessConfiguration[ThreeD], trainingData: IndexedSeq[(Point[ThreeD], Vector[ThreeD])], sigma2: Double) = {

    val trainingDataWithNoise = trainingData.map {
      case (x, y) => (x, y, sigma2)
    }
    val nPoints = configuration.points.length
    val mean: Point[ThreeD] => Vector[ThreeD] = createLandmarkMean(trainingDataWithNoise, configuration.cov, configuration.mean)
    val landmarkCov = LandmarkKernel[ThreeD](configuration.cov, trainingDataWithNoise, nPoints * nPoints)
    new DiscreteGaussianProcess[ThreeD](configuration.domain, mean, configuration.points, landmarkCov)
  }

  def createCacheOptimizedLandmarkBaseGaussianProcess3D(configuration: DiscreteGaussianProcessConfiguration[ThreeD], trainingData: IndexedSeq[(Point[ThreeD], Vector[ThreeD])], sigma2: Double, meshSize: Int) = {

    val trainingDataWithNoise = trainingData.map {
      case (x, y) => (x, y, sigma2)
    }
    val nPoints = configuration.points.size
    val mean: Point[ThreeD] => Vector[ThreeD] = createLandmarkMean(trainingDataWithNoise, configuration.cov, configuration.mean)
    val landmarkCov = LandmarkKernelNonRepeatingPoints[ThreeD](configuration.cov, trainingDataWithNoise, meshSize)
    new DiscreteGaussianProcess[ThreeD](configuration.domain, mean, configuration.points, landmarkCov)
  }

  // TODO this is pretty much the same code as the landmark kernel! refactor?
  private def createLandmarkMean[D <: Dim: VectorFactory](trainingData: IndexedSeq[(Point[D], Vector[D], Double)], kernel: MatrixValuedPDKernel[D, D], mean: (Point[D]) => Vector[D]): (Point[D]) => Vector[D] = {

    val dim = kernel.outputDim
    val N = trainingData.size*dim
    def flatten(v: IndexedSeq[Vector[D]]) = DenseVector(v.flatten(_.data).toArray)

    val (xs, ys, sigma2s) = trainingData.unzip3

    val meanvec = xs.map(mean)
    val mVec = flatten(meanvec)
    val yVec = flatten(ys)
    val fVec = yVec-mVec

    val noise = breeze.linalg.diag(DenseVector(sigma2s.map(x => List.fill(dim)(x)).flatten.toArray))

    val K_inv = breeze.linalg.pinv(Kernel.computeKernelMatrix[D](xs,kernel).map(_.toDouble) + noise)

    def f(x: Point[D]) : Vector[D] = {
      val kstar = Kernel.computeKernelVectorFor[D](x,xs,kernel)
      Vector[D](((kstar * K_inv).map(_.toFloat) * fVec).toArray)
    }

    f
  }


}