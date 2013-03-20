package smptk
package registration

import image.Geometry._
import image.CoordVector
import breeze.linalg.DenseMatrix
import smptk.image.DiscreteImageDomain1D
import breeze.linalg.DenseVector

trait PDKernel[CV[A] <: CoordVector[A]] extends ((CV[Double], CV[Double]) => Double)

case class GaussianKernel1D(val sigma2: Double) extends PDKernel[CoordVector1D] {
  def apply(x: Point1D, y: Point1D) = {

    val r = (x(0) - y(0))
    scala.math.exp(-(r * r) / sigma2)
  }
}

case class PolynomialKernel1D(degree: Int) extends PDKernel[CoordVector1D] {
  def apply(x: Point1D, y: Point1D) = {
    math.pow(x(0) * y(0) + 1, degree)
  }
}

object Kernel {
  type Sample1D = IndexedSeq[CoordVector1D[Double]]
  val numPointsForNystrom = 500

  def computeKernelMatrix(xs: Sample1D, k: PDKernel[CoordVector1D]): DenseMatrix[Double] = {
    val K = DenseMatrix.zeros[Double](xs.size, xs.size)
    for { (xi, i) <- xs.zipWithIndex; (xj, j) <- xs.zipWithIndex } {
      K(i, j) = k(xi, xj)
      K(j, i) = K(i, j)
    }
    K
  }

  def computeKernelVectorFor(x: Point1D, xs: IndexedSeq[Point1D], k: PDKernel[CoordVector1D]): DenseVector[Double] = {
    val kxs = DenseVector.zeros[Double](xs.size)
    for { (xi, i) <- xs.zipWithIndex } {
      kxs(i) = k(xi, x)
    }
    kxs
  }

  def computeNystromApproximation(k: PDKernel[CoordVector1D], domain: DiscreteImageDomain1D, numBasisFunctions: Int): IndexedSeq[(Double, (Point1D => Double))] = {

    // procedure for the nystrom approximation as described in 
    // Gaussian Processes for machine Learning (Rasmussen and Williamson), Chapter 4, Page 99

    val step = (domain.extent(0) - domain.origin(0)) / numPointsForNystrom
    val ptsForNystrom = for (i <- 0 until numPointsForNystrom) yield CoordVector1D(domain.origin(0) + i * step)
    val kernelMatrix = computeKernelMatrix(ptsForNystrom, k)
    val (uMat, lambdaMat, _) = breeze.linalg.svd(kernelMatrix) //TODO replace with rand SVD
    val lambda = lambdaMat.map(_ / numPointsForNystrom)


    
    val phiCache = scala.collection.mutable.HashMap.empty[(Int, Point1D), Double]
    
    // TODO think about size hint
    // TODO think if we should replace it with a weak HashMap
    phiCache.sizeHint(domain.numberOfPoints * numBasisFunctions)
    
    def phi(i: Int)(x: Point1D) = {

      def phiInternal(x: Point1D) = {
        val kVec = computeKernelVectorFor(x, ptsForNystrom, k)
        val value = math.sqrt(numPointsForNystrom) / lambdaMat(i) * (kVec dot uMat(::, i))
        value
      }

      phiCache.getOrElseUpdate((i, x), phiInternal(x))
    }
    for (i <- (0 until numBasisFunctions)) yield (lambda(i), phi(i)_)

  }

}
