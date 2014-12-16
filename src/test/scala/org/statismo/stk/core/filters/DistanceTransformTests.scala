package org.statismo.stk.core.filters

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import org.statismo.stk.core.geometry._
import org.statismo.stk.core.image.DiscreteScalarImage1D
import org.statismo.stk.core.image.DiscreteImageDomain
import org.statismo.stk.core.image.DiscreteScalarImage2D
import scala.language.implicitConversions

class DistanceTransformTests extends FunSpec with ShouldMatchers {
  org.statismo.stk.core.initialize()

  implicit def doubleToFloat(d : Double) = d.toFloat
  
  describe("A 1D distance transform") {
    it("yields the right distance values") {
      val dom1D = DiscreteImageDomain[_1D](Point(0.0), Vector(1.0), Index(5))
      val img1D = DiscreteScalarImage1D(dom1D, Array(1.0, 1.0, 0.0, 1.0, 1.0))
      
      val dm =  DistanceTransform.euclideanDistanceTransform1D(img1D)
      dm.values should equal(Array(2.0, 1.0, 0.0, 1.0, 2.0))     
    }
  }

    describe("A 2D distance transform") {
    it("yields the right distance values") {
      val dom2D = DiscreteImageDomain[_2D](Point(0.0, 0.0), Vector(1.0, 1.0), Index(4, 3))
      val img2D = DiscreteScalarImage2D(dom2D, Array(1.0, 1.0, 1.0, 1.0, 1.0 , 0.0, 0.0, 1.0, 1.0, 1.0, 1.0, 1.0))
      
      val dm =  DistanceTransform.euclideanDistanceTransform2D(img2D)
      dm(Index(0,0)) should be(math.sqrt(2) plusOrMinus 1e-5)
      dm(Index(1,1)) should be(0.0 plusOrMinus 1e-5)
      dm(Index(1,0)) should be(1.0 plusOrMinus 1e-5)
      dm(Index(3,2)) should be(math.sqrt(2) plusOrMinus 1e-5)
    }
  }

  
  
}
