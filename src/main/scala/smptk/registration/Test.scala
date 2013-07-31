package smptk
package registration

import scala.collection.immutable.{ Vector => SVector }
import TransformationSpace.ParameterVector
import image._
import image.Image._
import image.Interpolation._
import io.ImageIO
import java.io.File
import breeze.linalg.DenseVector

object Test {
  smptk.initialize()
  def resample3D() {
    val path = "/home/bouabene/workspace/smptk/src/test/resources/chimp3D-11.h5"
    val discreteImage = ImageIO.read3DScalarImage[Short](new File(path)).get
    val continuousImage = Interpolation.interpolate(discreteImage, 0)

    println("before resampling, number of domain points = " + discreteImage.domain.numberOfPoints)
    val resampledImage = Resample.sample[Short](continuousImage, discreteImage.domain, 0)
    println("finished resampling")
  }
   def resample2D() {
    val path = "/home/bouabene/femur-xraySS.h5"
    val discreteImage = ImageIO.read2DScalarImage[Short](new File(path)).get
    val continuousImage = Interpolation.interpolate(discreteImage, 3)

    println("before resampling, number of domain points = " + discreteImage.domain.numberOfPoints)
    val resampledImage = Resample.sample[Short](continuousImage, discreteImage.domain, 0.0)
    println("finished resampling")
  } 
  
  def main(args: Array[String]) {
    resample3D
  }
}