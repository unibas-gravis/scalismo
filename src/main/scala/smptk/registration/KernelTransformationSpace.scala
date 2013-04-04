package smptk.registration

import smptk.image.Geometry._
import TransformationSpace.ParameterVector
import breeze.linalg.DenseMatrix
import scala.NotImplementedError
import breeze.linalg.DenseVector
import smptk.image.CoordVector
import smptk.image.DiscreteImageDomain1D
import breeze.plot._
import smptk.image.DiscreteImageDomain1D
import smptk.image.DiscreteImageDomain2D
import smptk.image.Geometry.implicits._
import smptk.image.Image._
import smptk.image.DiscreteScalarImage1D
import smptk.image.Interpolation
import smptk.image.Utils
import smptk.io.ImageIO
import java.io.File

//trait GaussianProcess[CV[A] <: CoordVector[A]] {
//  val  m : (CV[Double] => DenseVector[Double])
//  val k : PDKernel[CV]
//
//  def sample: IndexedSeq[CV[Double]] => DenseVector[Double]
//}

case class GaussianProcess[CV[A] <: CoordVector[A]](val m: CV[Double] => DenseVector[Double], val k: PDKernel[CV]) {

  type PointSample = IndexedSeq[CV[Double]]

  /*
  def posterior(trainingData: IndexedSeq[(CoordVector1D[Double], Double)], sigma2: Double): GaussianProcess1D = {

    val (xs, ys) = trainingData.unzip
    val yVec = DenseVector(ys.toArray)
    val kxx = Kernel.computeKernelMatrix(xs, k)

    val kinv = breeze.linalg.inv(kxx + DenseMatrix.eye[Double](xs.size) * sigma2)

    def mp(x: CoordVector1D[Double]): Double = {
      val kxs = Kernel.computeKernelVectorFor(x, xs,k)
      (kxs dot kinv * yVec)

    }

    val kp = new PDKernel[CoordVector1D] {
      def apply(x1: Point1D, x2: Point1D) = {
        val kx1xs = Kernel.computeKernelVectorFor(x1, xs, k)
        val kx2xs = Kernel.computeKernelVectorFor(x2, xs, k)
        k(x1, x2) - (kx1xs dot (kinv * kx2xs))
      }
    }
    GaussianProcess1D(mp, kp)
  }
*/
  def sample: (PointSample => DenseVector[Double]) = { (xs: PointSample) =>
    {
      val n = xs.size
      val d = k.outputDim
      val meanVec = DenseVector.zeros[Double](n * d)
      for (i <- 0 until n; di <- 0 until d) meanVec(i * d + di) = m(xs(i))(di)
      val covMatrix = Kernel.computeKernelMatrix(xs, k)
      val noise = breeze.linalg.diag(DenseVector.ones[Double](xs.size)) * 1e-6 // gaussian noise for stability 
      val lMat = breeze.linalg.cholesky(covMatrix + noise)
      val u = for (_ <- 0 until xs.size) yield breeze.stats.distributions.Gaussian(0, 1).draw()
      val uVec = DenseVector(u.toArray)
      meanVec + lMat * uVec
    }
  }
}

case class KernelTransformationSpace1D(val domain: DiscreteImageDomain1D,
  val numParameters: Int, gp: GaussianProcess[CoordVector1D]) extends TransformationSpace[CoordVector1D] {

  def parametersDimensionality = numParameters

  val (eigenPairs, effectiveNumParameters) = Kernel.computeNystromApproximation(gp.k, domain, numParameters)

  def apply(p: ParameterVector) = KernelTransformation1D(p)
  def inverseTransform(p: ParameterVector) = None
  def takeDerivativeWRTParameters(p: ParameterVector) = { x: Point1D =>
    val J = DenseMatrix.zeros[Double](1, effectiveNumParameters)
    for (((lambda, phi), j) <- eigenPairs.zipWithIndex) (J(0, j) = math.sqrt(lambda) * phi(x)(0))
    J

  }

  // the actual kernel transform
  case class KernelTransformation1D(alpha: ParameterVector) extends Transformation[CoordVector1D] {
    require(alpha.size == eigenPairs.size)

    def apply(x: Point1D) = {

      val defValue = eigenPairs.zipWithIndex.par.map(eigPairWithIndex => {
        val ((lambda, phi), i) = eigPairWithIndex
        phi(x)(0) * alpha(i) * math.sqrt(lambda)
      }).foldLeft(0.)(_ + _)

      CoordVector1D(x(0) + defValue + gp.m(x)(0))
    }

    def takeDerivative(x: Point1D) = { throw new NotImplementedError("take derivative of kernel") }
  }

}

case class KernelTransformationSpace2D(val domain: DiscreteImageDomain2D,
  val numParameters: Int, gp: GaussianProcess[CoordVector2D]) extends TransformationSpace[CoordVector2D] {

  def parametersDimensionality = numParameters

  val (eigenPairs, effectiveNumParameters) = Kernel.computeNystromApproximation(gp.k, domain, numParameters)

  def apply(p: ParameterVector) = KernelTransformation2D(p)
  def inverseTransform(p: ParameterVector) = None
  def takeDerivativeWRTParameters(p: ParameterVector) = { x: Point2D =>
    val J = DenseMatrix.zeros[Double](2, effectiveNumParameters)
    for (((lambda, phi), j) <- eigenPairs.zipWithIndex) { J(::, j) := phi(x) * math.sqrt(lambda) }
    J
  }

  // the actual kernel transform
  case class KernelTransformation2D(alpha: ParameterVector) extends Transformation[CoordVector2D] {
    require(alpha.size == eigenPairs.size)

    def apply(x: CoordVector2D[Double]) = {
      val zero = DenseVector(0., 0.)
      //      val defValue = eigenPairs.zipWithIndex.foldLeft(zero)((sum , eigPairWithIndex) => {
      //        val ((lambda, phi), i) = eigPairWithIndex
      //        sum + phi(x) * alpha(i) * math.sqrt(lambda)
      //      })
      val defValue = eigenPairs.zipWithIndex.par.map(eigPairWithIndex => {
        val ((lambda, phi), i) = eigPairWithIndex
        phi(x) * alpha(i) * math.sqrt(lambda)
      }).foldLeft(zero)(_ + _)

      CoordVector2D(x(0) + defValue(0) + gp.m(x)(0), x(1) + defValue(1) + gp.m(x)(1))
    }

    def takeDerivative(x: Point2D) = {throw new NotImplementedError("take derivative of kernel") }
  }

}

object KernelTransformationSpace {

  def main(args: Array[String]) {
      val domain = DiscreteImageDomain1D(-5., 0.1, 1000)
      val discreteImage = DiscreteScalarImage1D(domain, domain.points.map(x => x(0)))
      val continuousImg = Interpolation.interpolate(3)(discreteImage)

      val gk = GaussianKernel1D(0.1)
      val gp = GaussianProcess[CoordVector1D]((x: Point1D) => DenseVector(0.), gk)
      val kernelSpace = KernelTransformationSpace1D(domain, 50, gp)

      val transform = kernelSpace(DenseVector.ones[Double](50) * 1.)
      val transformedImg = continuousImg compose transform

      //      val f = Figure()
      //      val p = f.subplot(0)
      //      
      //      val xs = domain.points
      //      val eigPairs = Kernel.computeNystromApproximation(gp.k, domain, 5)
      //      for ((lmbda, phi) <- eigPairs) { 
      //    	  p += plot(xs.map(_(0)), xs.map(x => phi(x)))
      //      }
      //	  f.refresh      
      //      Utils.showGrid1D(domain, transform)

      val regResult = Registration.registration1D(transformedImg, continuousImg, kernelSpace, MeanSquaresMetric1D,
        0f, DenseVector.zeros[Double](50))

      Utils.show1D(continuousImg, domain)
      Utils.show1D(transformedImg, domain)
      Utils.show1D(continuousImg.warp(regResult(domain).transform, domain.isInside), domain)

//    val fixedImage = Utils.gridImage2D(10./64, 1./64)
//     val domain = DiscreteImageDomain2D(CoordVector2D(0.,0.), CoordVector2D(1./64, 1./64	), CoordVector2D(128, 128))
//    val testImgUrl = "/home/luethi/workspace/smptk/src/test/resources/lena256.h5"
//    val discreteFixedImage = ImageIO.read2DScalarImage[Short](new File(testImgUrl)).get
//    val fixedImage = Interpolation.interpolate2D(3)(discreteFixedImage)
//    val domain = discreteFixedImage.domain
//    Utils.show2D(fixedImage, domain)
//
//    val gk = UncorrelatedKernelND[CoordVector2D](GaussianKernel2D(100.0), 2)
//
//    //val gk = UncorrelatedKernelND[CoordVector2D](PolynomialKernel2D(2), 2)
//    val gp = GaussianProcess[CoordVector2D]((x: CoordVector2D[Double]) => DenseVector(0., 0.), gk)
//
//    val transformSpace = KernelTransformationSpace2D(domain, 5, gp)
//    val kernelTransform = transformSpace(DenseVector(100., 100., 100., 20., 20.))
//
//    println(transformSpace.eigenPairs.unzip._1)
//    val transformedLena = fixedImage compose kernelTransform
//    //val outputDomain = DiscreteImageDomain2D(CoordVector2D(0.,0.), CoordVector2D(0.5, 0.5), CoordVector2D(256, 256))
//    Utils.show2D(transformedLena, domain)
//    val registration = Registration.registration2D(transformedLena, fixedImage, transformSpace, MeanSquaresMetric2D,
//      0f, DenseVector.zeros[Double](5))
//
//    val regResult = registration(domain)
//    val backwarpedLena = transformedLena.compose(regResult.transform)
//    Utils.show2D(backwarpedLena, domain)

  }
}

