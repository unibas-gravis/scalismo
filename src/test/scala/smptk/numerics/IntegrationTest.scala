package smptk.numerics
import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import smptk.image.DiscreteImageDomain1D
import smptk.image.ContinuousScalarImage1D
import smptk.geometry._
import smptk.geometry.implicits._
import breeze.linalg.DenseVector
import smptk.image.DiscreteImageDomain2D
import smptk.image.Utils
import smptk.common.BoxedDomain1D

class IntegrationTest extends FunSpec with ShouldMatchers {

  describe("An integration in 1D") {
    it("Correctly integrates x squared on interval [-1,1]") {

      val img =  ContinuousScalarImage1D( BoxedDomain1D(0.0, 1.0), (x: Point[OneD]) => x * x, Some((x: Point[OneD]) => DenseVector(2.) * x(0) ))  

      val domain = DiscreteImageDomain1D(-1, 0.002, 1000)
      val integrator = Integrator[OneD](IntegratorConfiguration(UniformSampler1D(), domain.numberOfPoints))  
    
      val res = integrator.integrateScalar(img, domain)
      res should be(1. / 3. plusOrMinus 0.001)
    }
    it("Correctly integrates sin(x) on interval [-Pi, Pi]") {

      val img =  ContinuousScalarImage1D( 
          BoxedDomain1D(-math.Pi, math.Pi), 
          (x: Point[OneD]) => math.sin(x.toDouble).toFloat, 
          Some((x: Point[OneD]) => DenseVector( - math.cos(x.toDouble).toFloat ))
          )

      val domain = DiscreteImageDomain1D(-math.Pi, math.Pi.toFloat / 500f, 1000)
      val integrator = Integrator[OneD](IntegratorConfiguration(UniformSampler1D(), domain.numberOfPoints))  
        
      val res = integrator.integrateScalar(img, domain)
      res should be(0. plusOrMinus 0.001)

    }
    
    it("Correctly integrates integrates a compact function") {

      val img =  ContinuousScalarImage1D( BoxedDomain1D(-1.0, 1.0),  (x: Point[OneD]) => 1.)

      //Utils.show1D(img, DiscreteImageDomain1D(-2., 0.1, 40))
      
      val region1 = BoxedDomain1D(-1.01, 1.01) 
      val region2 = BoxedDomain1D(-8.01, 8.01)
      
      val integrator = Integrator(IntegratorConfiguration(UniformSampler1D(), 1000))  

      val res1 = integrator.integrateScalar(img, region1)
      val res2 = integrator.integrateScalar(img, region2)
      
      
      res1 should be(res2 plusOrMinus 0.001)

    }

   
  }

}