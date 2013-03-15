package smptk
package registration

import scala.collection.immutable.{ Vector => SVector }

import TransformationSpace.ParameterVector
import Registration.RegistrationResult
import image._
import image.Geometry._
import image.Geometry.implicits._
import image.Image._
import image.Interpolation._
import numerics.Integration
import io.ImageIO


import breeze.plot._
import breeze.linalg._

import java.io.File

object Test { 
// def main(args : Array[String])  { 
//        val discreteImage = ImageIO.read2DScalarImage[Short](new File("/tmp/test.h5")).get
//      println("image read")
//        val continuousImage = Interpolation.interpolate2D(0)(discreteImage)
//        println("interpolated")
//      val tSpace = TranslationSpace2D()
//      val translation = tSpace(DenseVector[Float](10,0))
//
//        val translatedImg = continuousImage.warp(translation)
//     println("warped")
//      val resampledImage = Resample.sample2D[Short](translatedImg, discreteImage.domain, 0)      
//      println("resampled")
//      ImageIO.writeImage(resampledImage, new File("/tmp/resampled.h5"))
//      println("written")
//  }
//}

  def doRegistration() = {
      val testImgUrl = "/home/luethi/workspace/smptk/src/test/resources/lena.h5"
        // getClass().getResource("/lena.h5").getPath()
      val discreteFixedImage = ImageIO.read2DScalarImage[Short](new File(testImgUrl)).get
      val fixedImage = Interpolation.interpolate2D(3)(discreteFixedImage)
      
      val domain = discreteFixedImage.domain
      val center = (domain.origin(0) + domain.extent(0) / 2, domain.origin(1) + domain.extent(1) / 2)
      
      //val rigidTransform = RigidTransformationSpace2D(center)(DenseVector(-0f,-0f, 3.14f  / 20))
      val translationTransform = TranslationSpace2D()(DenseVector(-1f, 5f))
      val transformedLena =fixedImage compose translationTransform
      
      val registration = Registration.registration2D(fixedImage, transformedLena, TranslationSpace2D(), MeanSquaresMetric2D, 
          0f, DenseVector.zeros[Float](2))
      
      val regResult = registration(domain)    
      println(regResult.parameters)
          regResult
  }

  def main(args: Array[String]) {
    val regResult = doRegistration()
    println("optimal parameters: " + regResult.parameters.toString)

  }

}