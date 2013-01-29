package smptk.image

import breeze.plot._
import breeze.linalg._
import scala.util._
import java.io.IOException


object Interpolation {

  

  def splineInterpolate[Point](p: Point): (Point => Float) = p => 0
    
  def cubicSpline(x:Float) : Float = {
	val absX = scala.math.abs(x)
	
    if(absX>=0 && absX<1)
		(0.66666666-scala.math.pow(absX, 2)+scala.math.pow(absX, 3)).toFloat
	else if (absX >= 1 && absX < 2)
		(scala.math.pow((2-absX),3)/6).toFloat
	else 0
  
  }
  
  def interpolationScalar1D(image:DiscreteScalarImage1D) : ContinuousScalarImage1D ={
    val ck = DenseVector.ones[Float](image.domain.size(0))
    
    new ContinuousScalarImage1D(
    		ImageDomain1D()
    ) 
    	
    	def apply(x:Space#Point) : Float = {
    	   val xUnit = (spaceOperations.getComponentPoint(x,0) - spaceOperations.getComponentPoint(image.domain.origin,0)) / spaceOperations.getComponentVector(image.domain.spacing,0)
    	  	val k = scala.math.ceil(xUnit - 2).toInt 
    	  	val K = 4  
    	  	   	 
    	  	
    		var result = 0f	
    		var i = k 
    		while(i <= K){
    		  result = result + cubicSpline(spaceOperations.getComponentPoint(x,0)-k)*ck(k)
    		  i=i+1
    		}
    	   result
    	}
    	def pixelDimensionality = 1
    	
    	def domain = new ImageDomain[Space]{
    		def dimensionality = 1
    		def spaceOperations: SpaceOperations[Space] = spaceOperations
    		def origin: Space#Point = image.domain.origin
    		def extent: DenseVector[Float] = image.domain.spacing * image.domain.size(0)
    		def isInside(pt : Space#Point) : Boolean = {}
    def uniformSamples(n : Int) : IndexedSeq[Space#Point]
    	}
    } 

  }
  
  
  def interpolation2D[Space <: SpaceType, Pixel](image:DiscreteImage[Space, Pixel]):ContinuousImage[Space,Pixel] ={
   new ContinuousImage[Space, Pixel]{
      def apply(x:Space#Point) : Float = {1f}
    }
  }
  
  def interpolation[Space <: SpaceType,  Pixel](image: DiscreteImage[Space, Pixel])(implicit spaceOperations:SpaceOperations[Space]): ContinuousImage[Space, Pixel] = {

    spaceOperations.dimensionality match {
      case 	1 =>  interpolation1D(image)
      case _=> interpolation2D(image) 
    
    } 
 	
  }
    
  
  
  def main(args: Array[String]) {
	  val a : Try[Int]= Failure(new IOException("abc"))
	  val b : Try[Int]= Success(5)
    val f = Figure()
    val p = f.subplot(0)
    val x = linspace(0.0, 1.0)
    println("here: " +a.getOrElse("0"))
    
    val c = for {
      value <- a
      value2 <- b
    }
      yield(value + value2)
    
    p += plot(x, x :^ 2.0)
    p += plot(x, x :^ 3.0, '.')
    p.xlabel = "x axis"
    p.ylabel = "y axis"
    f.saveas("lines.png") // save current figure as a .png, eps and pdf also supported
    println("hello world")
  }
}