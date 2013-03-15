package smptk.numerics
import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import smptk.image.DiscreteImageDomain1D
import smptk.image.ContinuousScalarImage1D
import smptk.image.Geometry._
import smptk.image.Geometry.implicits._
import breeze.linalg.DenseVector

class IntegrationTest extends FunSpec with ShouldMatchers {

  describe("An integration in 1D") {
    it("Correctly integrates x squared on interval [0,1]") {
      val img =  ContinuousScalarImage1D( (x: Point1D) => x >= 0 && x <= 1, (x: Point1D) => x * x, (x: Point1D) => DenseVector(2.) * x(0) )  
      val res = Integration.integrate(img, DiscreteImageDomain1D(0., 0.001, 1000))
      res should be (1. / 3. plusOrMinus 0.001) 
    }
    it("Correctly integrates sin(x) on interval [0, Pi]") {
      val img =  ContinuousScalarImage1D( 
          (x: Point1D) => x >= 0 && x <= math.Pi, 
          (x: Point1D) => math.sin(x.toDouble).toFloat, 
          (x: Point1D) => DenseVector( - math.cos(x.toDouble).toFloat )
          )
      val res = Integration.integrate(img, DiscreteImageDomain1D(0f, math.Pi.toFloat / 1000f, 1000))
      res should be (-math.cos(math.Pi) + math.cos(0) plusOrMinus 0.001) 

      
    }
  }

}