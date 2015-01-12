package org.statismo.stk.core.image.filter

/**
 * Created by luetma00 on 12.01.15.
 */

import java.io.File

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import org.statismo.stk.core.geometry._
import org.statismo.stk.core.image.{ScalarImage, DiscreteImageDomain, DiscreteScalarImage}
import org.statismo.stk.core.io.ImageIO
import scala.language.implicitConversions

class DiscreteImageFilterTests extends FunSpec with ShouldMatchers {

  org.statismo.stk.core.initialize()

  implicit def doubleToFloat(d: Double) = d.toFloat


//  describe("A 2D distance transform") {
//
//    it("yields the right distance values") {
//
//
//      val dom2D = DiscreteImageDomain[_2D](Point(0.0, 0.0), Vector(0.1,0.2), Index(100, 50))
//      val img = ScalarImage(dom2D.imageBox, (pt : Point[_2D]) => if (pt(0) <= 5 && pt(1) <= 5) 1 else 0)
//
//      val discreteImg = img.sample[Short](dom2D, 0)
//      val dm = DiscreteImageFilter.euclideanDistanceTransform(discreteImg)
//      dm(dom2D.pointToIndex(Point(0, 0))) should be(1f plusOrMinus 1e-5f)
//      dm(Index(0, 0)) should be (Math.sqrt(2).toFloat plusOrMinus 1e-5)
//      dm(Index(9, 0)) should be (9f plusOrMinus 1e-5f)
//    }
//  }
//}


