package smptk.image

import breeze.plot._
import breeze.linalg._
import scala.util._
import java.io.IOException
import smptk.image.Geometry.CoordVector1D
import smptk.image.Geometry.CoordVector2D


object Interpolation {
 

  def splineInterpolate[Point](p: Point): (Point => Float) = p => 0
    
  
  def bSpline(n :Int)(x:Float) : Float = {
    val absX = scala.math.abs(x)
    n match {
      case 0 => {
        if (-0.5 < x && x < 0.5) 1f
        else if (absX == 0.5)  0.5f
        else 0

      }
       case 1 => {
         if( -1 <= x && x <= 0)  1f+x
         else if (0<x && x<=1)  1f-x
         else 0
       }
       case 2 => {
          if ( -1.5 <= x && x < -0.5) 0.5f * scala.math.pow(x + 1.5f, 2).toFloat
          else if ( -0.5 <= x && x < 0.5) - scala.math.pow(x + 0.5, 2).toFloat + (x - 0.5f) + 1.5f 
          else if (x >= 0.5 && x < 1.5) 0.5f * scala.math.pow(1 - (x - 0.5f), 2).toFloat
          else 0
              
       }
       case 3 => {

        if (absX >= 0 && absX < 1)
          (0.66666666 - scala.math.pow(absX, 2) + 0.5f * scala.math.pow(absX, 3)).toFloat
        else if (absX >= 1 && absX < 2)
          (scala.math.pow((2 - absX), 3) / 6).toFloat
        else 0
      }
       case _ => throw new NotImplementedError("Bspline of order " +n +" is not implemented yet")
     }
  }
  

  def interpolate(degree:Int)(image:DiscreteScalarImage1D) : ContinuousScalarImage1D ={
    val ck = image.pixelValues //DenseVector.ones[Float](image.domain.size(0))

    val splineBasis : (Float => Float) = bSpline(degree)
    new ContinuousScalarImage1D(
      ContinuousImageDomain1D(image.domain.origin, image.domain.extent), //new domain
      (x: CoordVector1D) => { // apply function
        val xUnit = (x(0) - image.domain.origin(0)) / image.domain.spacing(0)

        val k1 = scala.math.max(scala.math.ceil(xUnit - 0.5f*(degree+1)).toInt ,0)
        val K = scala.math.min(degree+1, ck.size-1)

        var result = 0f
        var k = k1
        while (k <= scala.math.min(k1 + K - 1, ck.size - 1)) {
          result = result + splineBasis(xUnit - k) * ck(k)
          k = k + 1
        }
        result
      }, 

      (x:CoordVector1D) => { //derivative
        //TODO derivative
        
        DenseVector(1f) 
      })
    	
  }
  
  
  def interpolationScalar2D(image:DiscreteScalarImage2D):ContinuousScalarImage2D = {
   new ContinuousScalarImage2D(
		   ContinuousImageDomain2D(image.domain.origin, image.domain.extent), //new domain
    		(x:CoordVector2D) => (1f), 
    		
    		(x:CoordVector2D) => DenseVector(1f) ) 
 
  }
  
  

  
  
  def main(args: Array[String]) {
	  val a : Try[Int]= Failure(new IOException("abc"))
	  val b : Try[Int]= Success(5)
    val f = Figure()
    val p = f.subplot(0)
    val xs = linspace(0, 5, 100).map(_.toFloat)
    val ps = DiscreteScalarImage1D(DiscreteImageDomain1D(0f, 1f, Tuple1(5)),  IndexedSeq(3f, 2f, 1.5f, 1f))
    val continuousImg = interpolate(2)(ps)
//    p += plot(x, x.map(bSpline(0) ))    
//    p += plot(x, x.map(bSpline(1) ))  
    
    p += plot(xs, xs.map( x => continuousImg(x) ))  
//    p += plot(x, x.map(bSpline(2) ))  
//    p += plot(x, x.map(bSpline(3) ))  
    p.xlabel = "x axis"
    p.ylabel = "y axis"
    f.saveas("lines.png") // save current figure as a .png, eps and pdf also supported
    println("hello world")
  }
}