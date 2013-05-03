package smptk.numerics

import smptk.registration.TransformationSpace.ParameterVector
import breeze.linalg.DenseVector
import breeze.optimize.LBFGS
import breeze.optimize.DiffFunction
import breeze.util.logging.Logger.Level
import breeze.optimize.StochasticGradientDescent

import breeze.optimize.StochasticGradientDescent.SimpleSGD

trait CostFunction extends Function1[ParameterVector, (Double, DenseVector[Double])] {
  def onlyValue(p: ParameterVector): Double

}

sealed trait OptimizationConfiguration

trait Optimizer extends Function2[ParameterVector, CostFunction, ParameterVector] {}

case class LBFGSOptimizerConfiguration(
  val numIterations: Int,
  val m: Int = 10,
  val tolerance: Double = 1e-5) extends OptimizationConfiguration

case class LBFGSOptimizer(configuration: LBFGSOptimizerConfiguration) extends Optimizer {
  def apply(x0: ParameterVector, c: CostFunction): ParameterVector = {
    optimize(x0, c)
  }

  private def optimize(x0: ParameterVector, c: CostFunction): ParameterVector = {
    val f = new DiffFunction[DenseVector[Double]] {
      def calculate(x: DenseVector[Double]) = {
        val (v, g) = c(x)
        (v, g)
      }
    }
    val lbfgs = new LBFGS[DenseVector[Double]](maxIter = configuration.numIterations, m = configuration.m, tolerance = configuration.tolerance)
    val optParams = lbfgs.minimize(f, x0)
    optParams
  }
}

case class BreezeStochGradOptimizerConfiguration(
  val numIterations: Int,
  val stepLength: Double,
  val tolerance: Double = 1e-5) extends OptimizationConfiguration

case class BreezeStochGradOptimizer(configuration: BreezeStochGradOptimizerConfiguration) extends Optimizer {
  def apply(x0: ParameterVector, c: CostFunction): ParameterVector = {
    optimize(x0, c)
  }

  private def optimize(x0: ParameterVector, c: CostFunction): ParameterVector = {
    val f = new DiffFunction[DenseVector[Double]] {
      def calculate(x: DenseVector[Double]) = {
        val (v, g) = c(x)
        (v, g)
      }
    }
    val stochOpti = new SimpleSGD[DenseVector[Double]](configuration.stepLength, configuration.numIterations)
    val optParams = stochOpti.minimize(f, x0)
    optParams
  }
}

case class GradientDescentConfiguration(val numIterations: Int, val stepLength: Double, val withLineSearch: Boolean = false, val robinsMonroe: Boolean = false, val stepDecreaseCoeff: Double = 0.) extends OptimizationConfiguration

case class GradientDescentOptimizer(configuration: GradientDescentConfiguration) extends Optimizer {

  private def goldenSectionLineSearch(nbPoints: Int, xk: ParameterVector, lowerLimit: Double, upperLimit: Double, normalizedGradient: DenseVector[Double], f: CostFunction): Double = {
    val r = 0.618

    var ll = lowerLimit
    var ul = upperLimit
    var b = ll + (1 - r) * (ul - ll)
    var c = ll + r * (ul - ll)

    val xs = new Array[Double](nbPoints)
    val fs = new Array[Double](nbPoints)

    var fb = f.onlyValue(xk + normalizedGradient * b)
    var fc = f.onlyValue(xk + normalizedGradient * c)
    xs(0) = b
    xs(1) = c
    fs(0) = fb
    fs(1) = fc

    for (i <- 2 until nbPoints) {
      if (fb > fc) {
        ll = b
        b = c
        fb = fc
        c = ll + r * (ul - ll)
        fc = f.onlyValue(xk + normalizedGradient * c)
        xs(i) = c
        fs(i) = fc
      } else {
        ul = c
        c = b
        fc = fb
        b = ll + (1 - r) * (ul - ll)
        fb = f.onlyValue(xk + normalizedGradient * b)
        xs(i) = b
        fs(i) = fb
      }
    }
    println("xs =" + xs.deep)
    println("fs =" + fs.deep)
    val insideVals = xs.zip(fs).filter(f => f._2 != 0)
    val ixs = insideVals.map(t => t._1)
    val ifs = insideVals.map(t => t._2)

    if(ifs.size > 0){
      val t = ifs.zipWithIndex.min
      ixs(t._2)
    }
    else // all f values are 0, means we most probably mapped the image out ! then simply return the smallest step size
      xs.min
  }

  val stepLength = configuration.stepLength
  val numIterations = configuration.numIterations

  def apply(x0: ParameterVector, c: CostFunction): ParameterVector = {
    println("Starting optimization")
    optimize(x0, c, 0)
  }

  private def optimize(x: ParameterVector, c: CostFunction, it: Int): ParameterVector = {
    val (newValue, gradient) = c(x)
    println("iteration " + it + " parameterVector " + x)
    println("iteration " + it + " Error value " + newValue)
    println("iteration " + it + " gradient " + gradient)

    if (it > numIterations) x
    else {
      if (configuration.withLineSearch) {
        val step = goldenSectionLineSearch(8, x, 0, stepLength, gradient, c)
        println(s"Step size at iteration $it=$step")
        optimize(x - gradient * step, c, it + 1)

      } else
        if(configuration.robinsMonroe){
          val step = configuration.stepLength/Math.pow(it+(configuration.numIterations*0.1), configuration.stepDecreaseCoeff) 
          println(s"Step size at iteration $it=$step")
          optimize(x - gradient * step, c, it + 1)
        }
        else
        optimize(x - gradient * stepLength, c, it + 1)
    }

  }

}

object Optimizer {

}