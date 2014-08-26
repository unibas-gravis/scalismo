package org.statismo.stk.core.registration

import TransformationSpace.ParameterVector
import breeze.linalg.DenseVector
import org.statismo.stk.core.statisticalmodel.{DiscreteGaussianProcess, GaussianProcess}
import org.statismo.stk.core.geometry._

trait Regularizer extends (ParameterVector => Double) {
  
  def takeDerivative(p : ParameterVector) : DenseVector[Float]
}

object RKHSNormRegularizer extends Regularizer {
	def apply(alpha : ParameterVector) = { val t = alpha.norm(2);  t*t}
  
  def takeDerivative(alpha : ParameterVector) = alpha * 2f
}

case class DiscreteRKHSNormRegularizer[D <: Dim: DimOps](val gp : DiscreteGaussianProcess[D]) extends Regularizer {

  val points = gp.points
  val dim = gp.outputDimensionality
  val cov = gp.cov

  var sum = 0.0f
  // sum_i sum_j (alpha_i k(xi,xj) alpha_j )
  def apply(alpha: ParameterVector): Double = {

    // Create Vector2D from alphas DenseVector
    val coeffs = for (i <- 0 until points.size ) yield Vector[D](alpha(i*dim until (i*dim)+dim).toArray)

    def sumJ(idx: Int) : Double =  {

      var sum = 0.0

      var j = 0

      while (j < points.size) {

      sum += coeffs (idx).dot (cov (points (idx), points (j) ) * coeffs (j) )

      j += 1
    }
      sum
    }

    val sum = 0.0
    (0 until points.size).par.map { i => sumJ(i) }.fold(sum)((a, b) => { a + b })

  }

  def takeDerivative(alpha: ParameterVector): DenseVector[Float] = {

    val coeffs = for (i <- 0 until points.size ) yield Vector[D](alpha(i*dim until (i*dim)+dim).toArray)

    val dalpha = DenseVector.zeros[Float](alpha.length)

    def covJ(i: Int, j: Int) : Vector[D] = {
      cov(points(i),points(j))*coeffs(j)
    }

    var i = 0

    while (i < points.size) {

      var j = 0

      val zeroVector = Vector.zeros[D]
      val sum = (0 until points.size).par.map { j =>  covJ(i,j) }.fold(zeroVector)((a, b) => { a + b })

      dalpha(i*dim until (i*dim) + dim) := sum.toBreezeVector

      i += 1

    }

    dalpha
  }

}