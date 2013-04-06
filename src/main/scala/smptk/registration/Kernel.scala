package smptk
package registration

import image.Geometry._
import image.CoordVector
import breeze.linalg.DenseMatrix
import smptk.image.DiscreteImageDomain1D
import breeze.linalg.DenseVector
import smptk.numerics.RandomSVD
import smptk.image.DiscreteImageDomain
import breeze.plot.Figure

trait PDKernel[CV[A] <: CoordVector[A]] {
  def apply(x : CV[Double], y : CV[Double]) : DenseMatrix[Double]
  def outputDim: Int
}

case class UncorrelatedKernelND[CV[A] <: CoordVector[A]](k: PDKernel[CV], val outputDim: Int) extends PDKernel[CV] {
  if (k.outputDim != 1) throw new IllegalArgumentException("Can only build Uncorrelated kernels from scalar valued kernels")
  val I = DenseMatrix.eye[Double](outputDim)
  def apply(x: CV[Double], y: CV[Double]) = I * (k(x, y)(0, 0)) // k is scalar valued

}

case class GaussianKernel2D(val sigma: Double) extends PDKernel[CoordVector2D] {
  val sigma2 = sigma * sigma
  def outputDim = 1
  def apply(x: CoordVector2D[Double], y:CoordVector2D[Double]) = {

    val r0 = (x(0) - y(0)) 
    val r1 = (x(1) - y(1))    
    val normr2 = r0*r0 + r1 * r1 // ||x -y ||^2
    if (normr2/ sigma2 > 10) DenseMatrix(0.)
    else DenseMatrix(scala.math.exp(-normr2 / sigma2))
  }
}

case class GaussianKernel1D(val sigma: Double) extends PDKernel[CoordVector1D] {

  val sigma2 = sigma * sigma
  
  def outputDim = 1
  def apply(x: Point1D, y: Point1D) = {

    val r = (x(0) - y(0))
    if (r * 5 / sigma2 > 10) DenseMatrix(0.)
    else DenseMatrix(scala.math.exp(-(r * r) / sigma2))
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
  val nystromSpacing = 10 // the spacing in mm between two grid points for the nystrom approximation

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

  def computeNystromApproximation[CV[A] <: CoordVector[A]](k: PDKernel[CV], domain: DiscreteImageDomain[CV], numBasisFunctions: Int, numPointsForNystrom : Int): (IndexedSeq[(Double, (CV[Double] => DenseVector[Double]))], Int) = {

    // procedure for the nystrom approximation as described in 
    // Gaussian Processes for machine Learning (Rasmussen and Williamson), Chapter 4, Page 99
	val ndVolume : Double = (0 until domain.dimensionality).foldLeft(1.)((p, d) => (domain.extent(d) - domain.origin(d)) * p)
    val ptsForNystrom = domain.uniformSamples(numPointsForNystrom)
    val kernelMatrix = computeKernelMatrix(ptsForNystrom, k)
//    val f = Figure()
//    val p = f.subplot(0)    
//    p += breeze.plot.image(kernelMatrix)
    val (uMat, lambdaMat, _) = RandomSVD.computeSVD(kernelMatrix , numBasisFunctions)
    val lambda = lambdaMat.map(lmbda => ndVolume.toDouble /  numPointsForNystrom * lmbda  )
    val numParams = (for (i <- (0 until lambda.size) if lambda(i) >= 1e-8) yield 1).size
    val phiCache = (0 until numParams).map(_ => scala.collection.mutable.HashMap.empty[CV[Double], DenseVector[Double]])

    // TODO think about size hint
    // TODO think if we should replace it with a weak HashMap
    for (i <- 0 until numParams) {phiCache(i).sizeHint(domain.numberOfPoints)}

    def phi(i: Int)(x: CV[Double]) = {
      def phiInternal(x: CV[Double]) = {
        val kx = computeKernelVectorFor(x , ptsForNystrom, k)
        val value = kx * uMat(::, i) * math.sqrt(numPointsForNystrom / ndVolume.toDouble) / lambdaMat(i)
        value
      }

      phiCache(i).getOrElseUpdate(x, phiInternal(x))
      
    }

    val pairs = for (i <- (0 until numParams) if lambda(i) >= 1e-8) yield (lambda(i), phi(i)_)
    (pairs, numParams)
  }

}
