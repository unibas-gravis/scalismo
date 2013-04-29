package smptk.image

import Image._
import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import smptk.image.Interpolation._
import org.scalatest.Ignore
import smptk.image.Geometry._
import smptk.image.Geometry.implicits._
import breeze.plot.Figure
import breeze.plot._
import org.scalatest.PrivateMethodTester
import breeze.linalg.DenseVector
import smptk.io.ImageIO
import java.io.File

class InterpolationTest extends FunSpec with ShouldMatchers with PrivateMethodTester {
  describe("A 1D Interpolation with 0rd order bspline") {

    it("interpolates the values for origin 2.3 and spacing 1.5") {
      val domain = DiscreteImageDomain1D(2.3, 1.5, 7)
      val discreteImage = DiscreteScalarImage1D(domain, Array(1.4, 2.1, 7.5, 9., 8., 0., 2.1))
      val continuousImg = interpolate1D(0)(discreteImage)
      for ((pt, idx) <- discreteImage.domain.points.zipWithIndex) {
        assert(continuousImg(pt) === discreteImage(idx))
      }
    }
  }

  describe("A 1D Interpolation with 1th order bspline") {

    it("interpolates the values for origin 2.3 and spacing 1.5") {
      val domain = DiscreteImageDomain1D(2.3f, 1.5f, 7)
      val discreteImage = DiscreteScalarImage1D(domain, Array(1.4, 2.1, 7.5, 9, 8, 0, 2.1))
      val continuousImg = interpolate1D(1)(discreteImage)
      for ((pt, idx) <- discreteImage.domain.points.zipWithIndex) {
        continuousImg(pt) should be(discreteImage(idx) plusOrMinus 0.0001f)
      }
    }

    it("interpolates the values for origin 0 and spacing 1") {
      val domain = DiscreteImageDomain1D(0f, 1, 5)
      val discreteImage = DiscreteScalarImage1D(domain, Array(3., 2., 1.5, 1., 0.))
      val continuousImg = interpolate1D(0)(discreteImage)
      for ((pt, idx) <- discreteImage.domain.points.zipWithIndex) {
        assert(continuousImg(pt) === discreteImage(idx))
      }
    }

    describe("A 1D Interpolation with 3rd order bspline") {

      it("Derivative of interpolated Sine function is the Cosine") {
        val domain = DiscreteImageDomain1D(-2., 0.01, 400)

        val discreteSinImage = DiscreteScalarImage1D(domain, domain.points.map(x => math.sin(x * math.Pi)).toIndexedSeq)
        val interpolatedSinImage = interpolate1D(3)(discreteSinImage)
        val derivativeImage = interpolatedSinImage.differentiate.get

        val discreteCosImage = DiscreteScalarImage1D(domain, domain.points.map(x => math.Pi * math.cos(x * math.Pi)).toIndexedSeq)
        val interpolatedCosImage = interpolate1D(3)(discreteCosImage)

        for ((pt, idx) <- domain.points.zipWithIndex.filter(x => math.abs(x._1) < 1.90)) {
          derivativeImage(pt)(0).toDouble should be(discreteCosImage(idx) plusOrMinus 0.0001f)
        }

      }

    }

  }

  describe("A 2D interpolation  Spline") {

    describe("of degree 0") {

      it("Has coefficients equal to the image samples") {

        val domain = DiscreteImageDomain2D((1., 0.), (0.5, 1.), (2, 3))
        val discreteImage = DiscreteScalarImage2D[Float](domain, Array(1.4f, 2.1f, 7.5f, 9f, 8f, 0f))
        for (idx <- 0 until discreteImage.domain.points.size) {
          val coeffs = Interpolation.determineCoefficients(0, discreteImage)
          coeffs(idx).toFloat should be(discreteImage(idx) plusOrMinus 0.0001f)
        }
      }

      it("Interpolates the values for a simple domain") {
        val domain = DiscreteImageDomain2D((0., 0.), (1., 1.), (2, 3))
        val discreteImage = DiscreteScalarImage2D(domain, Array(1f, 2f, 3f, 4f, 5f, 6f))

        val continuousImg = interpolate2D(0)(discreteImage)

        for ((pt, idx) <- discreteImage.domain.points.zipWithIndex) {
          continuousImg(pt).toFloat should be(discreteImage(idx) plusOrMinus 0.0001f)
        }
      }

      it("Interpolates the values for origin (2,3) and spacing (1.5, 2.3)") {
        val domain = DiscreteImageDomain2D((2., 3.), (1.5, 0.1), (2, 3))
        val discreteImage = DiscreteScalarImage2D(domain, Array(1.4f, 2.1f, 7.5f, 9f, 8f, 0f))

        val continuousImg = interpolate2D(0)(discreteImage)

        for ((pt, idx) <- discreteImage.domain.points.zipWithIndex) {
          continuousImg(pt).toFloat should be(discreteImage(idx) plusOrMinus 0.0001f)
        }
      }

    }
    describe(" of degree 3") {
      it("Interpolates the values for origin (2,3) and spacing (1.5, 2.3)") {
        val domain = DiscreteImageDomain2D((2., 3.), (1.5, 1.3), (10, 10))
        val discreteImage = DiscreteScalarImage2D(domain, domain.points.map(x => x(0)).toIndexedSeq)

        val continuousImg = interpolate2D(3)(discreteImage)

        for ((pt, idx) <- discreteImage.domain.points.zipWithIndex) {
          continuousImg(pt) should be(discreteImage(idx) plusOrMinus 0.0001f)
        }
      }

      it("Interpolates the values correctly for a test dataset") {
        val testImgUrl = getClass().getResource("/lena256.h5").getPath()
        val discreteFixedImage = ImageIO.read2DScalarImage[Short](new File(testImgUrl)).get
        val interpolatedImage = Interpolation.interpolate2D(2)(discreteFixedImage)

        val diff= for((p,i) <- discreteFixedImage.domain.points.zipWithIndex) yield {          
          interpolatedImage(p) - (discreteFixedImage(i))
        } 
        //println(ImageIO.writeImage(DiscreteScalarImage2D(discreteFixedImage.domain, diff).map(_.toShort), new File("/export/zambia/tmp/lenadiff.h5")))
        
        for((p,i) <- discreteFixedImage.domain.points.zipWithIndex){          
          interpolatedImage(p) should be (discreteFixedImage(i).toDouble plusOrMinus 30)
        } 
       
      }

      it("Derivative of interpolated function is correct") {
        val domain = DiscreteImageDomain2D((-2., -2.), (0.01, 0.01), (400, 400))

        val discreteFImage = DiscreteScalarImage2D(domain, domain.points.map(x => x(0) * x(0) + x(1) * x(1)).toIndexedSeq)
        val interpolatedFImage = interpolate2D(3)(discreteFImage)
        val derivativeImage = interpolatedFImage.differentiate.get

        for ((pt, idx) <- domain.points.zipWithIndex.filter(x => math.abs(x._1(0)) < 1.90 && math.abs(x._1(1)) < 1.90)) {
          derivativeImage(pt)(0) should be((2 * pt(0)) plusOrMinus 0.0001f)
          derivativeImage(pt)(1) should be((2 * pt(1)) plusOrMinus 0.0001f)
        }

      }

    }
  }
  describe("A 3D interpolation  Spline") {
    describe("of degree 0") {

      it("Has coefficients equal to the image samples") {

        val domain = DiscreteImageDomain3D((2., 3., 0.), (1.5, 1.3, 2.0), (2, 3, 2))
        val discreteImage = DiscreteScalarImage3D[Float](domain, Array(1.4f, 2.1f, 7.5f, 9f, 8f, 0f, 1.4f, 2.1f, 7.5f, 9f, 8f, 0f))

        for (idx <- 0 until discreteImage.domain.points.size) {
          val coeffs = Interpolation.determineCoefficients(0, discreteImage)
          coeffs(idx).toFloat should be(discreteImage(idx) plusOrMinus 0.0001f)
        }
      }

      it("Interpolates the values for origin (2,3,0) and spacing (1.5, 1.3, 2)") {
        val domain = DiscreteImageDomain3D((2., 3., 0.), (1.5, 1.3, 2.0), (2, 3, 2))
        val discreteImage = DiscreteScalarImage3D[Float](domain, Array(1.4f, 2.1f, 7.5f, 9f, 8f, 0f, 1.4f, 2.1f, 7.5f, 9f, 8f, 0f))

        val continuousImg = interpolate3D(0)(discreteImage)

        for ((pt, idx) <- discreteImage.domain.points.zipWithIndex) {
          continuousImg(pt).toFloat should be(discreteImage(idx) plusOrMinus 0.0001f)
        }
      }

    }

    describe(" of degree 1") {
      it("Interpolates the values for origin (2,3,0) and spacing (1.5, 1.3, 2)") {
        val domain = DiscreteImageDomain3D((2., 3., 0.), (1.5, 1.3, 2.0), (2, 3, 2))
        val discreteImage = DiscreteScalarImage3D[Float](domain, Array(1.4f, 2.1f, 7.5f, 9f, 8f, 0f, 1.4f, 2.1f, 7.5f, 9f, 8f, 0f))

        val continuousImg = interpolate3D(1)(discreteImage)

        for ((pt, idx) <- discreteImage.domain.points.zipWithIndex) {
          continuousImg(pt).toFloat should be(discreteImage(idx) plusOrMinus 0.0001f)
        }
      }

    }

    describe(" of degree 3") {
      it("Interpolates the values for origin (2,3,0) and spacing (1.5, 1.3, 2)") {
        val domain = DiscreteImageDomain3D((2., 3., 0.), (1.5, 1.3, 2.0), (10, 10, 10))
        val discreteImage = DiscreteScalarImage3D[Float](domain, domain.points.map(x => x(0).toFloat).toIndexedSeq)

        val continuousImg = interpolate3D(3)(discreteImage)

        for ((pt, idx) <- discreteImage.domain.points.zipWithIndex) {
          continuousImg(pt).toFloat should be(discreteImage(idx) plusOrMinus 0.0001f)
        }
      }

      it("Derivative of interpolated function is correct") {
        val domain = DiscreteImageDomain3D((-2., -2., -2.), (0.1, 0.1, 0.1), (40, 40, 40))

        val discreteFImage = DiscreteScalarImage3D(domain, domain.points.map(x => x(0) * x(0) + x(1) * x(1) + x(2) * x(2)).toIndexedSeq)
        val interpolatedFImage = interpolate3D(3)(discreteFImage)
        val derivativeImage = interpolatedFImage.differentiate.get

        for ((pt, idx) <- domain.points.zipWithIndex.filter(x => math.abs(x._1(0)) < 1. && math.abs(x._1(1)) < 1. && math.abs(x._1(2)) < 1.)) {
          derivativeImage(pt)(0) should be((2 * pt(0)) plusOrMinus 0.0001)
          derivativeImage(pt)(1) should be((2 * pt(1)) plusOrMinus 0.0001)
          derivativeImage(pt)(2) should be((2 * pt(2)) plusOrMinus 0.0001)
        }

      }
      
    }
    
    it("Interpolates a real dataset correctly"){
      	val path = getClass().getResource("/chimp3D-11.h5").getPath()
    	val discreteImage = ImageIO.read3DScalarImage[Short](new File(path)).get  
    	val continuousImage = Interpolation.interpolate3D(0)(discreteImage) 
    	
    	println("before show")
    	Utils.show3D(continuousImage, discreteImage.domain)
   	    println("before map")	    	
    	for((p,i) <- discreteImage.domain.points.zipWithIndex) 
    		 discreteImage.pixelValues(i).toDouble should be (continuousImage(p) plusOrMinus 0.001 )
    	
    }
    

  }
}
