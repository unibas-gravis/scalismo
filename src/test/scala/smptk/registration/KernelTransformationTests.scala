package smptk
package registration

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import breeze.linalg.DenseMatrix
import smptk.image.DiscreteImageDomain1D
import smptk.image.Geometry.CoordVector1D
import smptk.image.Geometry.Point1D
import com.sun.org.apache.xpath.internal.operations.Plus


class KernelTransformationTests extends FunSpec with ShouldMatchers {

  describe("The Nystroem approximation of a Kernel matrix") {
    it("Is close enough to kernel matrix") {
      val N = 100
      val kernel = PolynomialKernel1D(3)
      val domain = DiscreteImageDomain1D(CoordVector1D(0f), CoordVector1D(0.01f), CoordVector1D(100))
      val eigenPairs = Kernel.computeNystromApproximation(kernel, domain, 4)
      
      def approxKernel(x : Point1D, y : Point1D) = {   
        eigenPairs.foldLeft(0.)((sum, eigenPair) => {
          val (lmbda, phi) = eigenPair
          sum + lmbda * phi(x) * phi(y)
        })
      }
      
      for (x<-domain.points; y <-domain.points) {
 
        (approxKernel(x,y) should be (kernel(x,y) plusOrMinus 0.0001f))
        
      }


    }
  }
}