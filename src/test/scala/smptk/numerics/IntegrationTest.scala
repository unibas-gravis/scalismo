package smptk.numerics
import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import smptk.image.DiscreteImageDomain1D
import smptk.image.ContinuousScalarImage1D
import smptk.image.Geometry._
import breeze.linalg.DenseVector

class IntegrationTest extends FunSpec with ShouldMatchers {

  describe("An integration in 1D") {
    it("Correctly integrates x squared on interval [0,1]") {
      val img =  ContinuousScalarImage1D( (x: Point1D) => x >= 0 && x <= 1, (x: Point1D) => x * x, (x: Point1D) => DenseVector(2f) * x(0) )  
      val res = Integration.integrate(img, DiscreteImageDomain1D(0f, 0.001f, 1000))
      res should be (1f / 3f plusOrMinus 0.001f) 
    }
    it("Correctly integrates sin(x) on interval [0, Pi]") {
      val img =  ContinuousScalarImage1D( 
          (x: Point1D) => x >= 0 && x <= math.Pi, 
          (x: Point1D) => math.sin(x.toDouble).toFloat, 
          (x: Point1D) => DenseVector( - math.cos(x.toDouble).toFloat )
          )
      val res = Integration.integrate(img, DiscreteImageDomain1D(0f, math.Pi.toFloat / 1000f, 1000))
      res should be (-math.cos(math.Pi).toFloat + math.cos(0).toFloat plusOrMinus 0.001f) 

      
    }
  }

}