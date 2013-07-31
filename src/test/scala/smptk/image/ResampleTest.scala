package smptk
package image

import Image._
import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import Interpolation._
import org.scalatest.Ignore
import io.ImageIO
import java.io.File
import smptk.registration.RotationSpace2D
import smptk.geometry._
import smptk.geometry.implicits._
import breeze.linalg.DenseVector

class ResampleTest extends FunSpec with ShouldMatchers {
  describe("Resampling a 2D image") {

    
    // TODO make tests meaningful. currently they are only for visualization (side effect ;-) )
    
    val tmpdir = System.getProperty("java.io.tmpdir")
      val testImgUrl = getClass().getResource("/lena.h5").getPath()
      val discreteImage = ImageIO.read2DScalarImage[Short](new File(testImgUrl)).get
      val continuousImage = Interpolation.interpolate(discreteImage, 3)

    it("yields the original discrete image") {
      val resampledImage = Resample.sample[Short](continuousImage, discreteImage.domain, 0)
      ImageIO.writeImage(resampledImage, new File(tmpdir, "resampled.h5"))
      
    }

    it("yields the original discrete image for a translated domain") {

      val origin = discreteImage.domain.origin
      val translatedDomain = DiscreteImageDomain2D((origin(0) + 10, origin(1) + 10), discreteImage.domain.spacing, discreteImage.domain.size)
      val resampledImage = Resample.sample[Short](continuousImage, translatedDomain, 0)
      ImageIO.writeImage(resampledImage, new File(tmpdir, "resampled-translated.h5"))
    }

    it("represents a rotated image") {
      val domain = discreteImage.domain
      val center = ((domain.extent - domain.origin) * 0.5).toPoint 
      val rotTransform = RotationSpace2D(center)(DenseVector((math.Pi / 10).toFloat))
      val rotatedImg = continuousImage compose rotTransform
      val resampledImage = Resample.sample[Short](rotatedImg, domain, 0)
      ImageIO.writeImage(resampledImage, new File(tmpdir, "resampled-rotated.h5"))
    }
    
    
  }
  
  
  describe("Resampling a 3D image") {
    	val path = getClass().getResource("/chimp3D-11.h5").getPath()
    	val discreteImage = ImageIO.read3DScalarImage[Short](new File(path)).get  
    	val continuousImage = Interpolation.interpolate(discreteImage, 0) 
    	
    	it("yields the original discrete image"){ 	
    	  val resampledImage = Resample.sample[Short](continuousImage, discreteImage.domain, 0)    	  
    	  for(i <-0 until discreteImage.domain.numberOfPoints) { assert( resampledImage.pixelValues(i) === discreteImage.pixelValues(i) )} 
    	}
    
  } 
  
}