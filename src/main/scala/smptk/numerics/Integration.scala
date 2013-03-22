package smptk.numerics

import scala.language.higherKinds
import smptk.image._
import breeze.linalg.DenseVector
import smptk.image.Image._
import smptk.image.DiscreteImageDomain

object Integration {
  
	def integrate[CV[A] <: CoordVector[A]](img : ContinuousScalarImage[CV], integrationRegion : DiscreteImageDomain[CV]) : Double = { 

  	    // TODO this is terribly innefficient as we are looping twice
  	    val sampleValues : IndexedSeq[Option[Double]] = integrationRegion.points.map(img.liftPixelValue)
	   
	    val sum = sampleValues.map(_.getOrElse(0.)).sum
	    var ndVolume = 1.;  	    
	    for (d <- 0 until integrationRegion.dimensionality) {
	      ndVolume = (integrationRegion.extent(d) - integrationRegion.origin(d)) * ndVolume
	    }
	   	    
	    sum * ndVolume / integrationRegion.numberOfPoints
	}
     
       
	def integrate[CV[A] <: CoordVector[A]](img : ContinuousVectorImage[CV], integrationRegion : DiscreteImageDomain[CV]) : DenseVector[Double] = { 

	    val sampleValues : IndexedSeq[Option[DenseVector[Double]]] = integrationRegion.points.map(img.liftPixelValue)
	    
	    var ndVolume = 1.;
	     for (d <- 0 until integrationRegion.dimensionality) {
	      ndVolume = (integrationRegion.extent(d) - integrationRegion.origin(d))* ndVolume
	    }
	    
	    val zeroVector = DenseVector.zeros[Double](img.pixelDimensionality)
	    val sum : DenseVector[Double] = sampleValues.map(_.getOrElse(zeroVector)).foldLeft(zeroVector)(_ + _)
	    sum * ndVolume / integrationRegion.numberOfPoints.toDouble
	    
	}
}