package org.statismo.stk.core.statisticalmodel

import breeze.linalg.DenseVector
import breeze.linalg.DenseMatrix
import breeze.linalg.LinearAlgebra
import org.statismo.stk.core.geometry._

class MultivariateNormalDistribution(mean : DenseVector[Float], cov : DenseMatrix[Float] ) {
    
   require(cov.rows == cov.cols)
  require(mean.size == cov.rows)
    
  val dim = mean.size
  
  val covInv = LinearAlgebra.inv(cov)
  val covDet = LinearAlgebra.det(cov)
  
	def pdf(x : DenseVector[Float]) = { 
	  if (x.size != dim) throw new Exception(s"invalid vector dimensionality (provided ${x.size} should be $dim)")
	  val normFactor = math.pow(2.0 * math.Pi, -dim/2.0) * 1.0 / math.sqrt(covDet)

	  val x0 = (x - mean).map(_.toDouble)
	  val exponent = -0.5f * x0.dot(covInv * x0) 
	  normFactor * math.exp(exponent)
	}
   
   def logpdf(x : DenseVector[Float]) = { 
	  if (x.size != dim) throw new Exception(s"invalid vector dimensionality (provided ${x.size} should be $dim)")
	  val normFactor = math.pow(2.0 * math.Pi, -dim/2.0) * 1.0 / math.sqrt(covDet)

	  val x0 = (x - mean).map(_.toDouble)
	  val exponent = -0.5f * x0.dot(covInv * x0) 
	  math.log(normFactor) + exponent
     
   }
   
}

case class PointNormalDistribution[D <: Dim : DimTraits](val pt : Point[D], val mean : Vector[D], val cov : MatrixNxN[D]) extends MultivariateNormalDistribution(mean.toBreezeVector, cov.toBreezeMatrix) { 
  def pdf(x : Point[D]) : Double =  pdf(x.toBreezeVector)
  def logpdf(x : Point[D]) : Double = logpdf(x.toBreezeVector)
}