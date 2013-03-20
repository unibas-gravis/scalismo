package smptk
package registration

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import breeze.linalg.DenseMatrix
import smptk.image.DiscreteImageDomain1D
import smptk.image.Geometry.CoordVector1D
import smptk.image.Geometry.Point1D
import com.sun.org.apache.xpath.internal.operations.Plus
import image.Geometry.implicits._
import image.Image._
import smptk.image.DiscreteScalarImage1D
import smptk.image.Interpolation
import breeze.linalg.DenseVector
import smptk.image.Utils
import breeze.plot.Figure
import breeze.plot._

class KernelTransformationTests extends FunSpec with ShouldMatchers {

  // TODO add a  test for testing the posterior kernel

  describe("The Nystroem approximation of a Kernel matrix") {
    it("Is close enough to kernel matrix") {
      val kernel = GaussianKernel1D(10)
      val domain = DiscreteImageDomain1D(CoordVector1D(-5f), CoordVector1D(0.02f), CoordVector1D(500))
      val eigenPairs = Kernel.computeNystromApproximation(kernel, domain, 100)
      def approxKernel(x: Point1D, y: Point1D) = {
        eigenPairs.foldLeft(0.)((sum, eigenPair) => {
          val (lmbda, phi) = eigenPair
          sum + lmbda * phi(x) * phi(y)
        })
      }

      for (x <- domain.points; y <- domain.points) {    	  
        (approxKernel(x, y) should be(kernel(x, y) plusOrMinus 0.001f))

      }

    }
  }

  describe("A kernel transformation") {
    it("can be used to get the correct parameters in 1d (doing registration)") {

      val domain = DiscreteImageDomain1D(-5., 0.01,  1000)
      val discreteImage = DiscreteScalarImage1D(domain, domain.points.map(x => x(0)))
      val continuousImg = Interpolation.interpolate(3)(discreteImage)

      
      val gk = GaussianKernel1D(0.1)
      val gp = GaussianProcess1D((x : Point1D) => 0., gk)
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
      Utils.show1D(continuousImg.warp(regResult(domain).transform, domain.isInside ), domain)
      //Utils.show1D(continuousImg compose regResult(domain).transform , domain)
            
    }
  }
}