package org.statismo.stk.core.statisticalmodel

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


case class DiscreteGaussianProcess[D <: Dim: DimOps](_domain: Domain[D],
                                                        _mean: Point[D] => Vector[D], val points: IndexedSeq[Point[D]], _cov: MatrixValuedPDKernel[D,D])
  extends GaussianProcess[D](_domain, _mean, _cov) {



  def outputDim = cov.outputDim
  private def N = points.size
  val rank = points.size*outputDim

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


}


object DiscreteGaussianProcess {

  def createDiscreteGaussianProcess1D(configuration: DiscreteGaussianProcessConfiguration[_1D]) = {
    new DiscreteGaussianProcess[_1D](configuration.domain, configuration.mean, configuration.points, configuration.cov)
  }

  def createDiscreteGaussianProcess2D(configuration: DiscreteGaussianProcessConfiguration[_2D]) = {
    new DiscreteGaussianProcess[_2D](configuration.domain, configuration.mean, configuration.points, configuration.cov)
  }

  def createDiscreteGaussianProcess3D(configuration: DiscreteGaussianProcessConfiguration[_3D]) = {
    new DiscreteGaussianProcess[_3D](configuration.domain, configuration.mean, configuration.points, configuration.cov)
  }

  def createLandmarkDiscreteGaussianProcess1D(configuration: DiscreteGaussianProcessConfiguration[_1D], trainingData: IndexedSeq[(Point[_1D], Vector[_1D])], sigma2: Double) = {

    val trainingDataWithNoise = trainingData.map {
      case (x, y) => (x, y, sigma2)
    }
    val nPoints = configuration.points.length
    val mean: Point[_1D] => Vector[_1D] = createLandmarkMean(trainingDataWithNoise, configuration.cov, configuration.mean)
    val landmarkCov: MatrixValuedPDKernel[_1D, _1D] = LandmarkKernel[_1D](configuration.cov, trainingDataWithNoise, nPoints * nPoints)

    new DiscreteGaussianProcess[_1D](configuration.domain, mean, configuration.sampler.sample.map(_._1), landmarkCov)
  }

  def createLandmarkDiscreteGaussianProcess2D(configuration: DiscreteGaussianProcessConfiguration[_2D], trainingData: IndexedSeq[(Point[_2D], Vector[_2D])], sigma2: Double) = {

    val trainingDataWithNoise = trainingData.map {
      case (x, y) => (x, y, sigma2)
    }
    val nPoints = configuration.points.length
    val mean: Point[_2D] => Vector[_2D] = createLandmarkMean(trainingDataWithNoise, configuration.cov, configuration.mean)
    val landmarkCov = LandmarkKernel[_2D](configuration.cov, trainingDataWithNoise, nPoints * nPoints)
    new DiscreteGaussianProcess[_2D](configuration.domain, mean, configuration.points, landmarkCov)
  }

  def createLandmarkBaseGaussianProcess3D(configuration: DiscreteGaussianProcessConfiguration[_3D], trainingData: IndexedSeq[(Point[_3D], Vector[_3D])], sigma2: Double) = {

    val trainingDataWithNoise = trainingData.map {
      case (x, y) => (x, y, sigma2)
    }
    val nPoints = configuration.points.length
    val mean: Point[_3D] => Vector[_3D] = createLandmarkMean(trainingDataWithNoise, configuration.cov, configuration.mean)
    val landmarkCov = LandmarkKernel[_3D](configuration.cov, trainingDataWithNoise, nPoints * nPoints)
    new DiscreteGaussianProcess[_3D](configuration.domain, mean, configuration.points, landmarkCov)
  }

  def createCacheOptimizedLandmarkBaseGaussianProcess3D(configuration: DiscreteGaussianProcessConfiguration[_3D], trainingData: IndexedSeq[(Point[_3D], Vector[_3D])], sigma2: Double, meshSize: Int) = {

    val trainingDataWithNoise = trainingData.map {
      case (x, y) => (x, y, sigma2)
    }
    val nPoints = configuration.points.size
    val mean: Point[_3D] => Vector[_3D] = createLandmarkMean(trainingDataWithNoise, configuration.cov, configuration.mean)
    val landmarkCov = LandmarkKernelNonRepeatingPoints[_3D](configuration.cov, trainingDataWithNoise, meshSize)
    new DiscreteGaussianProcess[_3D](configuration.domain, mean, configuration.points, landmarkCov)
  }

  // TODO this is pretty much the same code as the landmark kernel! refactor?
  private def createLandmarkMean[D <: Dim: DimOps](trainingData: IndexedSeq[(Point[D], Vector[D], Double)], kernel: MatrixValuedPDKernel[D, D], mean: (Point[D]) => Vector[D]): (Point[D]) => Vector[D] = {

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