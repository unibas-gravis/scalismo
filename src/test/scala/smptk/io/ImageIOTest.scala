package smptk
package io

import image.Image._
import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import image.Interpolation._
import image._
import geometry._
import geometry.implicits._
import java.io.File
import scala.util.Success
import scala.util.Failure

class ImageIOTest extends FunSpec with ShouldMatchers {

  smptk.initialize()
  
  describe("A 1D scalar image") {
    it("can be stored and read again") {
      val domain = DiscreteImageDomain1D(0, 0.02, 50)
      val values = domain.points.map(x => math.sin(2 * math.Pi * x)).map(_.toFloat).toIndexedSeq
      val discreteImage = DiscreteScalarImage1D[Float](domain, values)

      val tmpImgFile = File.createTempFile("image1D", ".h5")
      
      ImageIO.writeImage(discreteImage, tmpImgFile)
      val restoredDiscreteImgOrFailure = ImageIO.read1DScalarImage[Float](tmpImgFile)

      restoredDiscreteImgOrFailure.isSuccess should be(true)
      discreteImage should equal(restoredDiscreteImgOrFailure.get)

      tmpImgFile.delete()
    }
  }

  
  describe("A 2D vector image") { 
    it ("can be stored and read again") {
    	val domain = DiscreteImageDomain2D((1.0, 0.0), (0.5, 1.0), (2, 3))
        val discreteImage = DiscreteScalarImage2D[Float](domain, IndexedSeq(1.4f, 2.1f, 7.5f, 9f, 8f, 0f))
        
         val tmpImgFile = File.createTempFile("image2D", ".h5")

        ImageIO.writeImage(discreteImage, tmpImgFile)
        val restoredDiscreteImgOrFailure = ImageIO.read2DScalarImage[Float](tmpImgFile)
        
        restoredDiscreteImgOrFailure.isSuccess should be (true)            	    	
        discreteImage should equal (restoredDiscreteImgOrFailure.get)
       
        tmpImgFile.delete()
    }    
    

  }
  
// uncommneted, cause image is not here
//  describe("A 3D scalar image") {
//    it("Can be read and written again") {
//      val path = getClass().getResource("/chimp3D-11.h5").getPath()
//      
//      val discreteImage = ImageIO.read3DScalarImage[Short](new File(path)).get
//   //   Utils.show3D[Short](discreteImage)
//     
//     val f =  new File("/tmp/dummy.h5")
//     val t =  ImageIO.writeImage(discreteImage,f) 
//     
//     assert (t.isSuccess == true)
//     f.delete()
//    }    
//    
//  }
  
}