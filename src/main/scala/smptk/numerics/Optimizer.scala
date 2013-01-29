package smptk.numerics

import smptk.registration.TransformationSpace.ParameterVector
import breeze.linalg.DenseVector

trait CostFunction extends Function1[ParameterVector, (Float, DenseVector[Float])] {}

trait Optimizer extends Function2[ParameterVector, CostFunction, ParameterVector]{
  
}

case class GradientDescentOptimizer(val numIterations : Int) extends Optimizer {
  
  val stepLength  : Float = 0.00001f
  
  def apply(x0 : ParameterVector, c : CostFunction) : ParameterVector = {
     optimize(x0, c, 0)
  }
  
  private def optimize(x : ParameterVector, c : CostFunction, it : Int) : ParameterVector = {    
    val (newValue, gradient) = c(x)
    println("iteration " +it +" x " +x)
    println("iteration " +it +" value " +newValue)
    println("iteration " +it +" gradinet " +gradient)
    if (it > numIterations) x 
    else {
      //val normalizedGradient = gradient / gradient.norm(2).toFloat
      optimize(x - gradient * stepLength, c, it + 1) 
    }
  }
  
}

object Optimizer { 
  

  
  
}