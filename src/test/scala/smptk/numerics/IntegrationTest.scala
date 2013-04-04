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
    it("Correctly integrates x squared on interval [-1,1]") {
      val img =  ContinuousScalarImage1D( (x: Point1D) => x >= 0 && x <= 1, (x: Point1D) => x * x, Some((x: Point1D) => DenseVector(2.) * x(0) ))  
      val res = Integration.integrate(img, DiscreteImageDomain1D(-1, 0.002, 1000))
      res should be (1. / 3. plusOrMinus 0.001) 
    }
    it("Correctly integrates sin(x) on interval [-Pi, Pi]") {
      val img =  ContinuousScalarImage1D( 
          (x: Point1D) => x >= -math.Pi && x <= math.Pi, 
          (x: Point1D) => math.sin(x.toDouble).toFloat, 
          Some((x: Point1D) => DenseVector( - math.cos(x.toDouble).toFloat ))
          )
      val res = Integration.integrate(img, DiscreteImageDomain1D(-math.Pi, math.Pi.toFloat / 500f, 1000))
      res should be (0. plusOrMinus 0.001) 

      
    }
  }

}