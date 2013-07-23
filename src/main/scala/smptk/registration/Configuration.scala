package smptk.registration

import smptk.numerics.Optimizer
import smptk.numerics.GradientDescentOptimizer
import smptk.numerics.GradientDescentOptimizer
import smptk.image.DiscreteImageDomain
import breeze.linalg.DenseVector
import smptk.numerics.GradientDescentConfiguration
import smptk.image.DiscreteImageDomain1D




object Configuration {  
//case class KernelRegistration1D(domain : DiscreteImageDomain1D) extends RegistrationConfiguration[CoordVector1D] {
//
//	val gk = GaussianKernel1D(0.1)
//	val gp = (domain : DiscreteImageDomain[CoordVector1D]) => GaussianProcess[CoordVector1D]((x: Point1D) => DenseVector(0.), gk)
//	val regularizationWeight = 0.0
//    
//  
//	  def optimizer = GradientDescentOptimizer(GradientDescentConfiguration(100, 0.001))
//	  def metric = MeanSquaresMetric1D(MeanSquaresMetricConfiguration()) 
//	  def transformationSpace = KernelTransformationSpace1D(KernelTransformationSpaceConfiguration(100, 500, gp))(domain)
//	  def regularizer = RKHSNormRegularizer
//	  def initialParameters = DenseVector.zeros[Double](100)
//}
}