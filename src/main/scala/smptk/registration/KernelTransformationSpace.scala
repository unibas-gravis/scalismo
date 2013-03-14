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
    val r = (x - y)
    scala.math.exp(-(r * r) / sigma2)
  }
}

case class KernelTransformationSpace1D(val domain: DiscreteImageDomain1D, val numParameters: Int, kernel: PDKernel[CoordVector1D]) extends TransformationSpace[CoordVector1D] {

  def parametersDimensionality = numParameters


  val numPointsForNystrom = 500

  val (lambda, phi) = computeNystromApproximation()

  def apply(p: ParameterVector) = KernelTransformation1D(p)
  def inverseTransform(p:ParameterVector) = None
  def takeDerivativeWRTParameters(p: ParameterVector) = { x: Point1D =>
    val terms = for (i <- (0 until p.size)) yield (lambda(i) * phi(i)(x))
    DenseMatrix.create(1, numParameters, terms.toArray)
  }

  private def computeNystromApproximation(): (DenseVector[Double], (Int => Point1D => Double)) = {

    
    val step = domain.extent(0) / numPointsForNystrom
    val ptsForNystrom = for (i <- 0 until numPointsForNystrom) yield CoordVector1D(domain.origin(0) + i * step)
    val kernelMatrix = computeKernelMatrix(ptsForNystrom)
    val (uMat, lambda) = computeKernelMatrixDecomposition(kernelMatrix)
    
    def phi(i : Int)(x : Point1D) = { 
      val kVec = computeKernelVectorFor(x, ptsForNystrom)
      val value = math.sqrt(numPointsForNystrom)/lambda(i) *  (kVec dot uMat(::, i))
      value
    }
    

    (lambda, phi)

  }

  
  private def computeKernelVectorFor(x : Point1D, pts : IndexedSeq[Point1D]) : DenseVector[Double] = { 
    val k = DenseVector.zeros[Double](pts.size)
    for ((pt_i, i) <- pts.zipWithIndex) {
      k(i) = kernel(x, pt_i)
    }
    k
  }
  
  private def computeKernelMatrixDecomposition(kernelMatrix : DenseMatrix[Double]) = { 
    val (uMat, dVec, vMat) = breeze.linalg.svd(kernelMatrix)
    (uMat, dVec)
  }
  
  private def computeKernelMatrix(pts: IndexedSeq[Point1D]): DenseMatrix[Double] = {

    val k = DenseMatrix.zeros[Double](pts.size, pts.size)
    for ((pt_i, i) <- pts.zipWithIndex; (pt_j, j) <- pts.zipWithIndex) {
      k(i, j) = kernel(pt_i, pt_j)
      k(j, i) = k(i, j)
    }
    k
  }

  // the actual kernel transform
  case class KernelTransformation1D(alpha: ParameterVector) extends Transformation[CoordVector1D] {

    def apply(x: Point1D) = {
      val terms = for (i <- (0 until numParameters) view) yield (alpha(i) * lambda(i) * phi(i)(x))
      CoordVector1D(terms.sum)
    }

    def takeDerivative(x: Point1D) = { throw new NotImplementedError("take derivative of kernel") }
  }

}

object KernelTransformationSpace { 
  def main(args : Array[String]) { 
	val domain = DiscreteImageDomain1D(0f, 0.01f, 3000)
    val ts = KernelTransformationSpace1D(domain, 20, GaussianKernel1D(10f))
    
    val f = Figure()
    val p = f.subplot(0)

    //val xs = domain.points.map(_.toDouble)
//    val phi = ts.phi
//    p += plot(xs, xs.map(x => phi(0)(x)))
    val lambda = ts.lambda
    print ("lambda .size " +lambda.size)
    val is = (1 to lambda.size).map(_.toDouble)
    p += plot(is, is.map(i => lambda(i.toInt -1).toDouble))
  }
}

