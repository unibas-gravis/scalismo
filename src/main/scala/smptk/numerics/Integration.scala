package smptk.numerics

import scala.language.higherKinds
import smptk.image._
import breeze.linalg.DenseVector
import smptk.image.Image._

object Integration {
  
	def integrate[CV[A] <: CoordVector[A], Repr](img : ContinuousScalarImage[CV], integrationRegion : DiscreteImageDomain[CV]) : Float = { 

  	    // TODO this is terribly innefficient as we are looping twice
  	    val sampleValues : IndexedSeq[Option[Float]] = integrationRegion.points.map(img.liftPixelValue)
	   
	    val sum = sampleValues.map(_.getOrElse(0f)).sum
	    var ndVolume = 1f;  	    
	    for (d <- 0 until integrationRegion.dimensionality) {
	      ndVolume = integrationRegion.extent(d) * ndVolume
	    }
	   	    
	    sum * ndVolume / integrationRegion.numberOfPoints.toFloat
	}
     
       
	def integrate[CV[A] <: CoordVector[A]](img : ContinuousVectorImage[CV], integrationRegion : DiscreteImageDomain[CV]) : DenseVector[Float] = { 

	    val sampleValues : IndexedSeq[Option[DenseVector[Float]]] = integrationRegion.points.map(img.liftPixelValue)
	    
	    var ndVolume = 1f;
	     for (d <- 0 until integrationRegion.dimensionality) {
	      ndVolume = integrationRegion.extent(d) * ndVolume
	    }
	    
	    val zeroVector = DenseVector.zeros[Float](img.pixelDimensionality)
	    val sum : DenseVector[Float] = sampleValues.map(_.getOrElse(zeroVector)).foldLeft(zeroVector)(_ + _)
	    sum * ndVolume / integrationRegion.numberOfPoints.toFloat
	    
	}
}