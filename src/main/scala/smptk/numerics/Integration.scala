package smptk.numerics

import scala.language.higherKinds
import smptk.image._
import breeze.linalg.DenseVector
import smptk.image.Image._
import smptk.image.DiscreteImageDomain
import smptk.common.BoxedRegion


sealed trait IntegrationConfiguration

trait Integrator[CV[A] <: CoordVector[A]] {
  
  def integrateScalar(img : ContinuousScalarImage[CV], integrationRegion : BoxedRegion[CV]) : Double  
  def integrateVector(img : ContinuousVectorImage[CV], integrationRegion : BoxedRegion[CV]) : DenseVector[Double]
  
}

case class UniformIntegratorConfiguration(nbPoints : Int) extends IntegrationConfiguration

case class UniformIntegrator[CV[A] <: CoordVector[A]](configuration : UniformIntegratorConfiguration  =  UniformIntegratorConfiguration(3000)) extends Integrator[CV] {
		
	def integrateScalar(img : ContinuousScalarImage[CV], integrationRegion : BoxedRegion[CV]) : Double = { 
	  val sampleValues : IndexedSeq[Option[Double]] = integrationRegion.uniformSamples(configuration.nbPoints).map(img.liftPixelValue)
	   
	    val sum = sampleValues.map(_.getOrElse(0.)).sum
	    var ndVolume = 1.;  	    
	    for (d <- 0 until integrationRegion.dimensionality) {
	      ndVolume = (integrationRegion.extent(d) - integrationRegion.origin(d)) * ndVolume
	    }
	   	    
	    sum * ndVolume / configuration.nbPoints.toDouble
	}
	
	def integrateVector(img : ContinuousVectorImage[CV], integrationRegion : BoxedRegion[CV]) : DenseVector[Double] = { 

	    val sampleValues : IndexedSeq[Option[DenseVector[Double]]] = integrationRegion.uniformSamples(configuration.nbPoints).map(img.liftPixelValue)	    
	    var ndVolume = 1.;
	     for (d <- 0 until integrationRegion.dimensionality) {
	      ndVolume = (integrationRegion.extent(d) - integrationRegion.origin(d))* ndVolume
	    }
	    
	    val zeroVector = DenseVector.zeros[Double](img.pixelDimensionality)
	    val sum : DenseVector[Double] = sampleValues.map(_.getOrElse(zeroVector)).foldLeft(zeroVector)(_ + _)
	    sum * ndVolume / configuration.nbPoints.toDouble
	    
	}
}


