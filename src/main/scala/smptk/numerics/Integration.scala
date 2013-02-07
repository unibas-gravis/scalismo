package smptk.numerics

import scala.language.higherKinds
import smptk.image._
import breeze.linalg.DenseVector
import smptk.image.Image._

object Integration {
     implicit val numSamples : Int = 100
  
	def integrate[CV[A] <: CoordVector[A]](img : ContinuousScalarImage[CV])(implicit numSamples : Int) : Float = { 
  	    val domain = img.domain
  	    // TODO this is terribly innefficient as we are looping twice
  	    val sampleValues : IndexedSeq[Option[Float]] = domain.uniformSamples(numSamples).map(img.liftPixelValue)
	   
	    val sum = sampleValues.map(_.getOrElse(0f)).sum
	    var ndVolume = 1f;  	    
	    for (d <- 0 until domain.dimensionality) {
	      ndVolume = domain.extent(d) * ndVolume
	    }
	   	    
	    sum * ndVolume / numSamples.toFloat
	}
     
       
	def integrate[CV[A] <: CoordVector[A]](img : ContinuousVectorImage[CV])(implicit numSamples : Int) : DenseVector[Float] = { 
  	    val domain = img.domain
	    val sampleValues : IndexedSeq[Option[DenseVector[Float]]] = domain.uniformSamples(numSamples).map(img.liftPixelValue)
	    
	    var ndVolume = 1f;
	     for (d <- 0 until domain.dimensionality) {
	      ndVolume = domain.extent(d) * ndVolume
	    }
	    
	    val zeroVector = DenseVector.zeros[Float](img.pixelDimensionality)
	    val sum : DenseVector[Float] = sampleValues.map(_.getOrElse(zeroVector)).foldLeft(zeroVector)(_ + _)
	    sum * ndVolume / numSamples.toFloat
	    
	}
}