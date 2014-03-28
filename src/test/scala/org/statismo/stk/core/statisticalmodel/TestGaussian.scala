package org.statismo.stk.core.statisticalmodel

import breeze.linalg._
import breeze.numerics._
import breeze.plot._
import org.statismo.stk.core.kernels._
import org.statismo.stk.core.geometry._
import org.statismo.stk.core.common.BoxedDomain1D
import org.statismo.stk.core.numerics.UniformSampler1D

/**
 * Created by gerith00 on 3/14/14.
 */
object TestGaussian {

  def main(args: Array[String]) {


    val f = Figure()
    val random = scala.util.Random
    val i = linspace(1.0, 11.0, 100)
    val a = linspace(-6.0,6.0,1000)
    val a_points = for( i <- 0 until a.length ) yield {Point1D(a(i).toFloat)}
    def ifun(x: Double): Double = {
      -6.0 + 1.2 * (x - 1.0)
    }
    def g(x: Double) = {
      sin(x) * exp(-0.05 * x + x * 0.2 * random.nextGaussian * 0.3)
    }

    val xi = i map { ifun(_) }
    val xi_points = for( i <- 0 until xi.length ) yield {Point1D(xi(i).toFloat)}

    val yi = xi map { g(_) }

    var alpha_prev: DenseVector[Double] = DenseVector.zeros[Double](100)

    val kernel = GaussianKernel1D(20)
    val gk = UncorrelatedKernel1x1(kernel)
    val box = BoxedDomain1D(Point1D(-200.0f), Point1D(200.0f))

    val config = DiscreteGaussianProcessConfiguration[OneD](box,UniformSampler1D(box, 100), _ => Vector1D(0.0f),xi_points,UncorrelatedKernel1x1(GaussianKernel1D(5.0)))
    val gp = DiscreteGaussianProcess.createDiscreteGaussianProcess1D(config)

    val p = f.subplot(0)

//    println(a_points.map{x => gp.sample(x)(0)})

  }

}