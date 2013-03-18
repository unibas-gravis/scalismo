package smptk.registration

import smptk.image.Geometry._
import TransformationSpace.ParameterVector
import breeze.linalg.DenseMatrix
import scala.NotImplementedError
import breeze.linalg.DenseVector
import smptk.image.CoordVector
import smptk.image.DiscreteImageDomain1D
import breeze.plot._

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

trait GaussianProcess[CV[A] <: CoordVector[A]] {
  def m(x : CV[Double]) : Double
  def k(x : CV[Double], y : CV[Double]) : Double
   
  def sample(xs : IndexedSeq[CV[Double]]) : DenseVector[Double]
}

case class GaussianProcess1D(val _m: CoordVector1D[Double] => Double, val _k: PDKernel[CoordVector1D]) extends GaussianProcess[CoordVector1D]{

  type Sample1D = IndexedSeq[CoordVector1D[Double]]

  def m(x : Point1D) = _m(x)
  def k(x : Point1D, y : Point1D) = _k(x,y)
  
  
  def posterior(trainingData: IndexedSeq[(CoordVector1D[Double], Double)], sigma2 : Double): GaussianProcess1D = {

    val (xs, ys) = trainingData.unzip
    val yVec = DenseVector(ys.toArray)
    val kxx = computeKernelMatrix(xs)
    
    val kinv = breeze.linalg.inv(kxx + DenseMatrix.eye[Double](xs.size) * sigma2)
    
    def mp(x: CoordVector1D[Double]): Double = {
      val kxs = computeKernelVectorFor(x, xs)
      (kxs  dot kinv *  yVec) 
  
    }
    
    val kp = new PDKernel[CoordVector1D] {
      def apply(x1: Point1D, x2: Point1D) = { 
       val kx1xs = computeKernelVectorFor(x1, xs)
       val kx2xs = computeKernelVectorFor(x2, xs)
       k(x1, x2) - (kx1xs dot  (kinv * kx2xs))
      }
    }
    GaussianProcess1D(mp, kp)
  }

  def sample(xs: Sample1D): DenseVector[Double] = {
    val meanVec = DenseVector(xs.map(cv => m(cv)).toArray)
    val covMatrix = computeKernelMatrix(xs)
    val noise = breeze.linalg.diag(DenseVector.ones[Double](xs.size)) * 1e-6 // gaussian noise for stability 
    val lMat = breeze.linalg.cholesky(covMatrix + noise)
    val u = for (_ <- 0 until xs.size) yield breeze.stats.distributions.Gaussian(0, 1).draw()
    val uVec = DenseVector(u.toArray)
    meanVec + lMat * uVec
  }

  private def computeKernelMatrix(xs : Sample1D): DenseMatrix[Double] = {
    val K = DenseMatrix.zeros[Double](xs.size, xs.size)
    for { (xi, i) <- xs.zipWithIndex; (xj, j) <- xs.zipWithIndex } {
      K(i, j) = k(xi, xj)
      K(j, i) = K(i, j)
    }
    K
  }
  
  private def computeKernelVectorFor(x : Point1D,  xs: IndexedSeq[Point1D]): DenseVector[Double] = {
    val kxs = DenseVector.zeros[Double](xs.size)
    for { (xi, i) <- xs.zipWithIndex} {
      kxs(i) = k(xi, x)
    }
    kxs
  }

  
}

case class KernelTransformationSpace1D(val domain: DiscreteImageDomain1D, 
    val numParameters: Int, gp: GaussianProcess[CoordVector1D]) extends TransformationSpace[CoordVector1D] {

  def parametersDimensionality = numParameters

  val numPointsForNystrom = 500
  val (lambda, phi) = computeNystromApproximation()

  def apply(p: ParameterVector) = KernelTransformation1D(p)
  def inverseTransform(p: ParameterVector) = None
  def takeDerivativeWRTParameters(p: ParameterVector) = { x: Point1D =>
    val terms = for (i <- (0 until p.size)) yield (lambda(i) * phi(i)(x))
    DenseMatrix.create(1, numParameters, terms.toArray)
  }

  private def computeNystromApproximation(): (DenseVector[Double], (Int => Point1D => Double)) = {

    val step = domain.extent(0) / numPointsForNystrom
    val ptsForNystrom = for (i <- 0 until numPointsForNystrom) yield CoordVector1D(domain.origin(0) + i * step)
    val kernelMatrix = computeKernelMatrix(ptsForNystrom)
    val (uMat, lambda) = computeKernelMatrixDecomposition(kernelMatrix)

    def phi(i: Int)(x: Point1D) = {
      val kVec = computeKernelVectorFor(x, ptsForNystrom)
      val value = math.sqrt(numPointsForNystrom) / lambda(i) * (kVec dot uMat(::, i))
      value
    }

    (lambda, phi)

  }

  private def computeKernelVectorFor(x: Point1D, pts: IndexedSeq[Point1D]): DenseVector[Double] = {
    val k = DenseVector.zeros[Double](pts.size)
    for ((pt_i, i) <- pts.zipWithIndex) {
      k(i) = gp.k(x, pt_i)
    }
    k
  }

  private def computeKernelMatrixDecomposition(kernelMatrix: DenseMatrix[Double]) = {
    val (uMat, dVec, vMat) = breeze.linalg.svd(kernelMatrix)
    (uMat, dVec)
  }

  private def computeKernelMatrix(pts: IndexedSeq[Point1D]): DenseMatrix[Double] = {

    val k = DenseMatrix.zeros[Double](pts.size, pts.size)
    for ((pt_i, i) <- pts.zipWithIndex; (pt_j, j) <- pts.zipWithIndex) {
      k(i, j) = gp.k(pt_i, pt_j)
      k(j, i) = k(i, j)
    }
    k
  }

  // the actual kernel transform
  case class KernelTransformation1D(alpha: ParameterVector) extends Transformation[CoordVector1D] {

    def apply(x: Point1D) = {
      val terms = for (i <- (0 until numParameters) view) yield (alpha(i) * lambda(i) * phi(i)(x))
      CoordVector1D(terms.sum + gp.m(x)) 
    }

    def takeDerivative(x: Point1D) = { throw new NotImplementedError("take derivative of kernel") }
  }

}

object KernelTransformationSpace {
  def main(args: Array[String]) {

    val kernel = GaussianKernel1D(2)
    
    //val kernel = PolynomialKernel1D(4)
    
    val gp = GaussianProcess1D((_: Point1D) => 0., kernel)
    
    val domain = DiscreteImageDomain1D(CoordVector1D(0f), CoordVector1D(0.01f), CoordVector1D(500))
    val ts = KernelTransformationSpace1D(domain, 20, gp)

    val f = Figure()
    val p = f.subplot(0)

    val xs = domain.points

    val trainingData = IndexedSeq((CoordVector1D(0.5), 1.), (CoordVector1D(2.), -1.), (CoordVector1D(3.5), -1.5))
    val gpp = gp.posterior(trainingData, 0.000001)
    for (i <- 0 until 5) {
      val sample = gpp.sample(xs)
      p += plot(xs.map(_(0)).toArray, sample)
    }
    val meanSample =  xs.map(gpp.m)
    p += plot(xs.map(_(0)).toArray, DenseVector(meanSample.toArray) , '.')
    f.saveas("/tmp/plot.png")
  }
}

