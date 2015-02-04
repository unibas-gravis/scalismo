package scalismo.numerics

import breeze.linalg.DenseVector
import breeze.optimize.LBFGS
import breeze.optimize.DiffFunction
import scalismo.registration.TransformationSpace.ParameterVector
import scala.collection.Iterator

trait CostFunction extends (ParameterVector => (Float, DenseVector[Float])) {
  def onlyValue(p: ParameterVector): Double
}

trait Optimizer {

  case class State(iteration: Int, value: Double, gradient: DenseVector[Float], parameters: ParameterVector, stepLength: Double)

  def iterations(x0: ParameterVector, c: CostFunction): Iterator[State]
  def minimize(x0: ParameterVector, c: CostFunction): ParameterVector
}

case class LBFGSOptimizer(numIterations: Int, m: Int = 10, tolerance: Double = 1e-5) extends Optimizer {
  def iterations(x0: ParameterVector, c: CostFunction): Iterator[State] = {
    optimize(x0, c)
  }
  def minimize(x0: ParameterVector, c: CostFunction): ParameterVector = {
    val it = iterations(x0, c)
    it.toSeq.last.parameters
  }

  private def optimize(x0: ParameterVector, c: CostFunction): Iterator[State] = {
    val f = new DiffFunction[DenseVector[Double]] {
      def calculate(x: DenseVector[Double]) = {
        val (v, g) = c(x.map(_.toFloat))
        (v.toDouble, g.map(_.toDouble))
      }
    }
    val lbfgs = new LBFGS[DenseVector[Double]](maxIter = numIterations, m = m, tolerance = tolerance)
    for (it <- lbfgs.iterations(f, x0.map(_.toDouble)))
    yield State(it.iter, it.value, it.grad.map(_.toFloat), it.x.map(_.toFloat), 0)
  }
}

case class GradientDescentOptimizer(numIterations: Int,
                                    stepLength: Double,
                                    withLineSearch: Boolean = false,
                                    robinsMonroe: Boolean = false,
                                    stepDecreaseCoeff: Double = 0.0) extends Optimizer {

  private def goldenSectionLineSearch(nbPoints: Int, xk: ParameterVector, lowerLimit: Double, upperLimit: Double, normalizedGradient: DenseVector[Float], f: CostFunction): Double = {
    val r = 0.618

    var ll = lowerLimit
    var ul = upperLimit
    var b = ll + (1 - r) * (ul - ll)
    var c = ll + r * (ul - ll)

    val xs = new Array[Double](nbPoints)
    val fs = new Array[Double](nbPoints)

    var fb = f.onlyValue(xk + normalizedGradient * b.toFloat)
    var fc = f.onlyValue(xk + normalizedGradient * c.toFloat)
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
        fc = f.onlyValue(xk + normalizedGradient * c.toFloat)
        xs(i) = c
        fs(i) = fc
      } else {
        ul = c
        c = b
        fc = fb
        b = ll + (1 - r) * (ul - ll)
        fb = f.onlyValue(xk + normalizedGradient * b.toFloat)
        xs(i) = b
        fs(i) = fb
      }
    }
    val insideVals = xs.zip(fs).filter(f => f._2 != 0)
    val ixs = insideVals.map(t => t._1)
    val ifs = insideVals.map(t => t._2)

    if (ifs.size > 0) {
      val t = ifs.zipWithIndex.min
      ixs(t._2)
    } else // all file values are 0, means we most probably mapped the image out ! then simply return the smallest step size
      xs.min
  }

  def iterations(x0: ParameterVector, c: CostFunction): Iterator[State] = {
    optimize(x0, c, 0)
  }

  def minimize(x0: ParameterVector, c: CostFunction): ParameterVector = {
    iterations(x0, c).toSeq.last.parameters
  }

  private def optimize(x: ParameterVector, c: CostFunction, it: Int): Iterator[State] = {
    val (newValue, gradient) = c(x)


    if (it >= numIterations) Iterator(State(it, newValue, gradient, x, stepLength))
    else {

      if (withLineSearch) {
        val step = goldenSectionLineSearch(8, x, 0, stepLength, gradient, c)
        val newParam = x - gradient * step.toFloat

	Iterator(State(it, newValue, gradient, newParam, step))  ++ optimize(newParam, c, it + 1) 

      } else if (robinsMonroe) {
        val step = stepLength / Math.pow(it + (numIterations * 0.1), stepDecreaseCoeff)
        val newParam = x - gradient * step.toFloat
        Iterator(State(it, newValue, gradient, newParam, stepLength)) ++ optimize(x - gradient * step.toFloat, c, it + 1)
      } else {
        val newParam = x - gradient * stepLength.toFloat
        Iterator(State(it, newValue, gradient, newParam, stepLength)) ++ optimize(x - gradient * stepLength.toFloat, c, it + 1)
      }
    }
  }
}

object Optimizer {
}
