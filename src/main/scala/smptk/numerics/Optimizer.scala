package smptk.numerics

import smptk.registration.TransformationSpace.ParameterVector
import breeze.linalg.DenseVector
import breeze.optimize.LBFGS
import breeze.optimize.DiffFunction
import breeze.util.logging.Logger.Level

trait CostFunction extends Function1[ParameterVector, (Float, DenseVector[Float])] {}

trait Optimizer extends Function2[ParameterVector, CostFunction, ParameterVector]{
  
}

case class LBFGSOptimizer(val numIterations : Int) extends Optimizer { 
  def apply(x0 : ParameterVector, c : CostFunction) : ParameterVector = { 
    optimize(x0, c)    
  }
  
  private def optimize(x0 : ParameterVector, c : CostFunction) : ParameterVector = {
    val f = new DiffFunction[DenseVector[Double]] { 
      def calculate(x : DenseVector[Double]) =  {
        val xFloat = x.map(_.toFloat)
        val (v, g) = c(xFloat)
        (v, g.map(_.toDouble))
      }
    }
    val lbfgs = new LBFGS[DenseVector[Double]](maxIter = numIterations)
    val optParams = lbfgs.minimize(f, x0.map(_.toDouble))
        optParams.map(_.toFloat)
  }
}

case class GradientDescentOptimizer(val numIterations : Int) extends Optimizer {
  
  val stepLength  : Float = 0.000000000005f
  
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