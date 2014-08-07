package org.statismo.stk.core.filters

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import org.statismo.stk.core.geometry._
import org.statismo.stk.core.image.DiscreteScalarImage1D
import org.statismo.stk.core.image.DiscreteImageDomain1D
import org.statismo.stk.core.image.DiscreteImageDomain2D
import org.statismo.stk.core.image.DiscreteScalarImage2D
import scala.language.implicitConversions

class DistanceTransformTests extends FunSpec with ShouldMatchers {
  org.statismo.stk.core.initialize()

  implicit def doubleToFloat(d : Double) = d.toFloat
  
  describe("A 1D distance transform") {
    it("yields the right distance values") {
      val dom1D = DiscreteImageDomain1D(Point1D(0.0), Vector1D(1.0), Index1D(5))
      val img1D = DiscreteScalarImage1D(dom1D, Array(1.0, 1.0, 0.0, 1.0, 1.0))
      
      val dm =  DistanceTransform.euclideanDistanceTransform1D(img1D)
      dm.values should equal(Array(2.0, 1.0, 0.0, 1.0, 2.0))     
    }
  }

    describe("A 2D distance transform") {
    it("yields the right distance values") {
      val dom2D = DiscreteImageDomain2D(Point2D(0.0, 0.0), Vector2D(1.0, 1.0), Index2D(4, 3))
      val img2D = DiscreteScalarImage2D(dom2D, Array(1.0, 1.0, 1.0, 1.0, 1. , 0.0, 0.0, 1.0, 1.0, 1.0, 1.0, 1.0))
      
      val dm =  DistanceTransform.euclideanDistanceTransform2D(img2D)
      dm(Index2D(0,0)) should be(math.sqrt(2) plusOrMinus 1e-5)      
      dm(Index2D(1,1)) should be(0. plusOrMinus 1e-5)
      dm(Index2D(1,0)) should be(1. plusOrMinus 1e-5)
      dm(Index2D(3,2)) should be(math.sqrt(2) plusOrMinus 1e-5)
    }
  }

  
  
}
