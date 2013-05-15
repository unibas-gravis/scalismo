package smptk
package registration

import image.Geometry._
import image.CoordVector
import breeze.linalg.{ pinv, diag, DenseMatrix }
import smptk.image.DiscreteImageDomain1D
import breeze.linalg.DenseVector
import smptk.numerics.RandomSVD
import smptk.image.DiscreteImageDomain
import breeze.plot.Figure
import smptk.numerics.Sampler
import smptk.common.DiscreteDomain
import smptk.common.BoxedRegion
import smptk.numerics.UniformSampler
import smptk.common.ImmutableLRU

trait PDKernel[CV[A] <: CoordVector[A]] {
  def apply(x: CV[Double], y: CV[Double]): DenseMatrix[Double]
  def outputDim: Int
}

case class UncorrelatedKernelND[CV[A] <: CoordVector[A]](k: PDKernel[CV], val outputDim: Int) extends PDKernel[CV] {
  if (k.outputDim != 1) throw new IllegalArgumentException("Can only build Uncorrelated kernels from scalar valued kernels")
  val I = DenseMatrix.eye[Double](outputDim)
  def apply(x: CV[Double], y: CV[Double]) = I * (k(x, y)(0, 0)) // k is scalar valued

}

case class GaussianKernel3D(val sigma: Double, val scale : Double = 1.) extends PDKernel[CoordVector3D] {
  val sigma2 = sigma * sigma
  def outputDim = 1
  def apply(x: CoordVector3D[Double], y: CoordVector3D[Double]) = {

    val r0 = (x(0) - y(0))
    val r1 = (x(1) - y(1))
    val r2 = (x(2) - y(2))
    val normr2 = r0 * r0 + r1 * r1 + r2 * r2 // ||x -y ||^2
    DenseMatrix(scala.math.exp(-normr2 / sigma2)) * scale
  }
}

case class GaussianKernel2D(val sigma: Double) extends PDKernel[CoordVector2D] {
  val sigma2 = sigma * sigma
  def outputDim = 1
  def apply(x: CoordVector2D[Double], y: CoordVector2D[Double]) = {

    val r0 = (x(0) - y(0))
    val r1 = (x(1) - y(1))
    val normr2 = r0 * r0 + r1 * r1 // ||x -y ||^2
    DenseMatrix(scala.math.exp(-normr2 / sigma2))
  }
}

case class GaussianKernel1D(val sigma: Double) extends PDKernel[CoordVector1D] {

  val sigma2 = sigma * sigma

  def outputDim = 1
  def apply(x: Point1D, y: Point1D) = {

    val r = (x(0) - y(0))
    DenseMatrix(scala.math.exp(-(r * r) / sigma2))
  }
}

case class PolynomialKernel3D(degree: Int) extends PDKernel[CoordVector3D] {
  def outputDim = 1
  def apply(x: Point3D, y: Point3D) = {
    DenseMatrix(math.pow(x(0) * y(0) + x(1) * y(1) + x(2) * y(2) + 1, degree))
  }
}

case class PolynomialKernel2D(degree: Int) extends PDKernel[CoordVector2D] {
  def outputDim = 1
  def apply(x: Point2D, y: Point2D) = {
    DenseMatrix(math.pow(x(0) * y(0) + x(1) * y(1) + 1, degree))
  }
}

case class PolynomialKernel1D(degree: Int) extends PDKernel[CoordVector1D] {
  def outputDim = 1
  def apply(x: Point1D, y: Point1D) = {
    DenseMatrix(math.pow(x(0) * y(0) + 1, degree))
  }
}

object Kernel {

  def computeKernelMatrix[CV[A] <: CoordVector[A]](xs: IndexedSeq[CV[Double]], k: PDKernel[CV]): DenseMatrix[Double] = {
    val d = k.outputDim

    val K = DenseMatrix.zeros[Double](xs.size * d, xs.size * d)
    for { (xi, i) <- xs.zipWithIndex; (xj, j) <- xs.zipWithIndex; di <- 0 until d; dj <- 0 until d } {
      K(i * d + di, j * d + dj) = k(xi, xj)(di, dj)
      K(j * d + dj, i * d + di) = K(i * d + di, j * d + dj)
    }
    K
  }

  /**
   * for every domain point x in the list, we compute the kernel vector
   * kx = (k(x, x1), ... k(x, xm))
   * since the kernel is matrix valued, kx is actually a matrix
   */
  def computeKernelVectorFor[CV[A] <: CoordVector[A]](x: CV[Double], xs: IndexedSeq[CV[Double]], k: PDKernel[CV]): DenseMatrix[Double] = {
    val d = k.outputDim

    val kxs = DenseMatrix.zeros[Double](d, xs.size * d)

    var j = 0
    while (j < xs.size) {
      var di = 0
      while (di < d) {
        var dj = 0
        while (dj < d) {
          kxs(di, j * d + dj) = k(xs(j), x)(di, dj)
          dj += 1
        }
        di += 1
      }
      j += 1
    }

    kxs
  }


  def computeNystromApproximation[CV[A] <: CoordVector[A]](k: PDKernel[CV], domain: BoxedRegion[CV], numBasisFunctions: Int, numPointsForNystrom: Int, sampler: Sampler[CV]): IndexedSeq[(Double, CV[Double] => DenseVector[Double])] = {


    // procedure for the nystrom approximation as described in 
    // Gaussian Processes for machine Learning (Rasmussen and Williamson), Chapter 4, Page 99

    val ptsForNystrom = sampler.sample(domain, numPointsForNystrom)
    val ndVolume = domain.volume

    val kernelMatrix = computeKernelMatrix(ptsForNystrom, k)
    val (uMat, lambdaMat, _) = RandomSVD.computeSVD(kernelMatrix, numBasisFunctions)
    val lambda = lambdaMat.map(lmbda => (ndVolume.toDouble / numPointsForNystrom.toDouble) * lmbda)
    val numParams = (for (i <- (0 until lambda.size) if lambda(i) >= 1e-8) yield 1).size

    val W = uMat(::, 0 until numParams) * math.sqrt(numPointsForNystrom / ndVolume.toDouble) * pinv(diag(lambdaMat(0 until numParams)))

    @volatile
    var cache = ImmutableLRU[CV[Double], DenseMatrix[Double]](1000)
    def phi(i : Int)(x: CV[Double]) = {
      // check the cache. if value is not there exit
      // TODO make it nicer using scalaz Memo class
      // TODO make cache size configurable
      val (maybeKx, _) = cache.get(x)
      val value = maybeKx.getOrElse {
        val newValue = computeKernelVectorFor(x, ptsForNystrom, k) * W
        cache = (cache + (x, newValue))._2 // ignore evicted key
        newValue
      }
      // return an indexed seq containing with elements corresponding to the i deformations 
       value(::, i)
    }

    val lambdaISeq = lambda(0 until numParams).toArray.toIndexedSeq
    val phis = (0 until numParams).map(i => phi(i)_)
    lambdaISeq.zip(phis)
  }

}
