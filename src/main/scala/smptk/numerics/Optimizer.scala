package smptk.numerics

import smptk.registration.TransformationSpace.ParameterVector
import breeze.linalg.DenseVector
import breeze.optimize.LBFGS
import breeze.optimize.DiffFunction
import breeze.util.logging.Logger.Level

trait CostFunction extends Function1[ParameterVector, (Double, DenseVector[Double])] {}


sealed trait OptimizationConfiguration 

trait Optimizer extends Function2[ParameterVector, CostFunction, ParameterVector]{}


case class LBFGSOptimizerConfiguration( 
  val numIterations : Int,
  val m : Int = 10,
  val tolerance : Double = 1e-5
  )  extends OptimizationConfiguration 


case class LBFGSOptimizer(configuration: LBFGSOptimizerConfiguration) extends Optimizer { 
  def apply(x0 : ParameterVector, c : CostFunction) : ParameterVector = { 
    optimize(x0, c)    
  }
  
  private def optimize(x0 : ParameterVector, c : CostFunction) : ParameterVector = {
    val f = new DiffFunction[DenseVector[Double]] { 
      def calculate(x : DenseVector[Double]) =  {
        val (v, g) = c(x)
        (v, g)
      }
    }
    val lbfgs = new LBFGS[DenseVector[Double]](maxIter = configuration.numIterations, m = configuration.m, tolerance = configuration.tolerance)
    val optParams = lbfgs.minimize(f, x0)
        optParams
  }
}


case class GradientDescentConfiguration(val numIterations : Int, val stepLength : Double) extends OptimizationConfiguration 

case class GradientDescentOptimizer(configuration : GradientDescentConfiguration) extends Optimizer {
  
  val stepLength = configuration.stepLength
  val numIterations = configuration.numIterations
  
  def apply(x0 : ParameterVector, c : CostFunction) : ParameterVector = {
     optimize(x0, c, 0)
  }
  
  private def optimize(x : ParameterVector, c : CostFunction, it : Int) : ParameterVector = {    
    val (newValue, gradient) = c(x)
    println("iteration " +it +" parameterVector " +x)
    println("iteration " +it +" Error value " +newValue)
    println("iteration " +it +" gradient " +gradient)
    if (it > numIterations) x 
    else {
      //val normalizedGradient = gradient / gradient.norm(2).toFloat
      optimize(x - gradient * stepLength, c, it + 1) 
    }
  }
  
}

object Optimizer { 
  

  
  
}