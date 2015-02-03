package scalismo.kernels

import breeze.linalg.{ DenseVector, pinv, diag, DenseMatrix }
import scalismo.common.{VectorField, Domain}
import scalismo.geometry._
import scalismo.numerics.{RandomSVD, Sampler}
import scalismo.utils.Memoize

abstract class PDKernel[D <: Dim] { self =>
  
  protected def k(x : Point[D], y : Point[D]) : Double

  def apply(x: Point[D], y: Point[D]): Double = {
    if (this.domain.isDefinedAt(x) && this.domain.isDefinedAt(y))
      k(x,y)
    else {
      if (!this.domain.isDefinedAt(x)) {
        throw new IllegalArgumentException((s"$x is outside of the domain"))
      } else {
        throw new IllegalArgumentException((s"$y is outside of the domain"))
      }
    }
  }

  def domain : Domain[D]

  def +(that: PDKernel[D]): PDKernel[D] = new PDKernel[D] {
    override def k(x: Point[D], y: Point[D]) = self.k(x, y) + that.k(x, y)
    override def domain = Domain.intersection(domain, that.domain)
  }

  def *(that: PDKernel[D]): PDKernel[D] = new PDKernel[D] {
    override def k(x: Point[D], y: Point[D]) = self.k(x, y) * that.k(x, y)
    override def domain = Domain.intersection(domain, that.domain)
  }

  def *(s: Double): PDKernel[D] = new PDKernel[D] {
    override def k(x: Point[D], y: Point[D]) = self.k(x, y) * s
    override def domain = self.domain
  }

  // TODO this could be made more generic by allowing the input of phi to be any type A
  def compose(phi: Point[D] => Point[D]) = new PDKernel[D] {
    override def k(x: Point[D], y: Point[D]) = self.k(phi(x), phi(y))
    override def domain = self.domain
  }

}

abstract class MatrixValuedPDKernel[D <: Dim: NDSpace, DO <: Dim: NDSpace] { self =>

  def apply(x: Point[D], y: Point[D]): SquareMatrix[DO] = {
    if (this.domain.isDefinedAt(x) && this.domain.isDefinedAt(y))
      k(x,y)
    else {
      if (!this.domain.isDefinedAt(x)) {
        throw new IllegalArgumentException((s"$x is outside of the domain"))
      } else {
        throw new IllegalArgumentException((s"$y is outside of the domain"))
      }
    }
  }

  protected def k(x: Point[D], y: Point[D]): SquareMatrix[DO]

  def outputDim = implicitly[NDSpace[DO]].dimensionality

  def domain : Domain[D]


  def +(that: MatrixValuedPDKernel[D, DO]): MatrixValuedPDKernel[D, DO] = new MatrixValuedPDKernel[D, DO] {
    override def k(x: Point[D], y: Point[D]) = self.k(x, y) + that.k(x, y)
    override def domain = Domain.intersection(domain, that.domain)
  }

  def *(that: MatrixValuedPDKernel[D, DO]): MatrixValuedPDKernel[D, DO] = new MatrixValuedPDKernel[D, DO] {
    override def k(x: Point[D], y: Point[D]) = self.k(x, y) :* that.k(x, y)
    override def domain = Domain.intersection(domain, that.domain)
  }

  def *(s: Double): MatrixValuedPDKernel[D, DO] = new MatrixValuedPDKernel[D, DO] {
    override def k(x: Point[D], y: Point[D]) = self.k(x, y) * s
    override def domain = self.domain
  }

  // TODO this could be made more generic by allowing the input of phi to be any type A
  def compose(phi: Point[D] => Point[D]) = new MatrixValuedPDKernel[D, DO] {
    override def k(x: Point[D], y: Point[D]) = self.k(phi(x), phi(y))
    override def domain = self.domain
  }

}

case class UncorrelatedKernel[D <: Dim : NDSpace](kernel: PDKernel[D]) extends MatrixValuedPDKernel[D, D] {
  val I = SquareMatrix.eye[D]
  def k(x: Point[D], y: Point[D]) = I * (kernel(x, y)) // k is scalar valued
  override def domain = kernel.domain
}



case class MultiScaleKernel[D <: Dim : NDSpace](kernel : MatrixValuedPDKernel[D, D],
                                                min: Int,
                                                max: Int,
                                                scale : Int => Double = i => scala.math.pow(2.0, -2.0 * i)) extends MatrixValuedPDKernel[D, D] {

  def k(x: Point[D], y: Point[D]): SquareMatrix[D] = {
    var sum = SquareMatrix.zeros[D]
    for (i <- min until max) {
      sum += kernel((x.toVector * Math.pow(2, -i)).toPoint, (y.toVector * Math.pow(2, -i)).toPoint) * scale(i)
    }
    sum
  }

  // TODO check that the domain is correct
  override def domain = kernel.domain
}


object Kernel {

  def computeKernelMatrix[D <: Dim, DO <: Dim](xs: Seq[Point[D]], k: MatrixValuedPDKernel[D, DO]): DenseMatrix[Float] = {
    val d = k.outputDim

    val K = DenseMatrix.zeros[Float](xs.size * d, xs.size * d)
    val xiWithIndex = xs.zipWithIndex.par
    val xjWithIndex = xs.zipWithIndex
    for { p1 <- xiWithIndex;  (xi, i) = p1; p2 <- xjWithIndex.drop(i) } {
      val (xj, j) = p2
      val kxixj = k(xi, xj);
      var di = 0;
      while (di < d) {
        var dj = 0;
        while (dj < d) {
          K(i * d + di, j * d + dj) = kxixj(di, dj)
          K(j * d + dj, i * d + di) = K(i * d + di, j * d + dj)
          dj += 1
        }
        di += 1
      }
    }
    K
  }

  /**
   * for every domain point x in the list, we compute the kernel vector
   * kx = (k(x, x1), ... k(x, xm))
   * since the kernel is matrix valued, kx is actually a matrix
   *
   * !! Hack - We currently return a double matrix, with the only reason that matrix multiplication (further down) is
   * faster (breeze implementation detail). This should be replaced at some point
   */
  def computeKernelVectorFor[D <: Dim, DO <: Dim](x: Point[D], xs: IndexedSeq[Point[D]], k: MatrixValuedPDKernel[D, DO]): DenseMatrix[Double] = {
    val d = k.outputDim

    val kxs = DenseMatrix.zeros[Double](d, xs.size * d)

    var j = 0
    while (j < xs.size) {
      var di = 0
      val kxxj = k(x, xs(j))
      while (di < d) {
        var dj = 0
        while (dj < d) {
          kxs(di, j * d + dj) = kxxj(di, dj)
          dj += 1
        }
        di += 1
      }
      j += 1
    }

    kxs
  }

  def computeNystromApproximation[D <: Dim: NDSpace, DO <: Dim : NDSpace](k: MatrixValuedPDKernel[D, DO],
                                                                          numBasisFunctions: Int,
                                                                          sampler: Sampler[D])
  : IndexedSeq[(Float, VectorField[D, DO])] = {

    // procedure for the nystrom approximation as described in 
    // Gaussian Processes for machine Learning (Rasmussen and Williamson), Chapter 4, Page 99

    val (ptsForNystrom, _) = sampler.sample.unzip

    // deppending on the sampler, it may happen that we did not sample all the points we wanted 
    val effectiveNumberOfPointsSampled = ptsForNystrom.size

    val kernelMatrix = computeKernelMatrix(ptsForNystrom, k).map(_.toDouble)
    val (uMat, lambdaMat, _) = RandomSVD.computeSVD(kernelMatrix, numBasisFunctions)


    val lambda = lambdaMat.map(lmbda => (lmbda / effectiveNumberOfPointsSampled.toDouble) )
    val numParams = (for (i <- (0 until lambda.size) if lambda(i) >= 1e-8) yield 1).size

    val W = uMat(::, 0 until numParams) * math.sqrt(effectiveNumberOfPointsSampled) * pinv(diag(lambdaMat(0 until numParams)))

    def computePhis(x: Point[D]): DenseMatrix[Double] = computeKernelVectorFor(x, ptsForNystrom, k) * W
    val computePhisMemoized = Memoize(computePhis, 1000)

    def phi(i: Int)(x: Point[D]) = {
      val value = computePhisMemoized(x)
      // extract the right entry for the i-th phi function
      Vector[DO](value(::, i).toArray.map(_.toFloat))

    }

    val lambdaISeq = lambda(0 until numParams).map(_.toFloat).toArray.toIndexedSeq
    val phis = (0 until numParams).map(i => VectorField(k.domain, phi(i)_))
    lambdaISeq.zip(phis)
  }

}
