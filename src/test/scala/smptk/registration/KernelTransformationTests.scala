package smptk
package registration

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import breeze.linalg.DenseMatrix
import smptk.image.DiscreteImageDomain1D
import smptk.image.ContinuousScalarImage1D
import smptk.image.Geometry.CoordVector1D
import smptk.image.Geometry.Point1D
import com.sun.org.apache.xpath.internal.operations.Plus
import image.Geometry.implicits._
import image.Image._
import smptk.image.DiscreteScalarImage1D
import smptk.image.Interpolation
import breeze.linalg.DenseVector
import smptk.image.Utils
import breeze.plot.Figure
import breeze.plot._
import smptk.io.ImageIO
import java.io.File
import smptk.image.Geometry.CoordVector2D
import smptk.image.Geometry.{ Point2D, Point3D }
import smptk.image.DiscreteImageDomain2D
import smptk.numerics.RandomSVD
import smptk.image.DiscreteImageDomain
import smptk.numerics.GradientDescentOptimizer
import smptk.numerics.GradientDescentConfiguration
import smptk.numerics.LBFGSOptimizer
import smptk.numerics.BreezeStochGradOptimizer
import smptk.numerics.BreezeStochGradOptimizerConfiguration
import smptk.numerics.LBFGSOptimizerConfiguration
import numerics.Integrator
import numerics.IntegratorConfiguration
import numerics.UniformSampler1D
import numerics.UniformSampler2D
import numerics.UniformDistributionRandomSampler1D
import numerics.UniformDistributionRandomSampler2D
import breeze.stats.distributions.Uniform
import smptk.image.ContinuousScalarImage2D
import breeze.stats.distributions.Uniform
import smptk.numerics.{ UniformSampler1D, UniformSampler3D }
import smptk.image.Geometry.CoordVector3D
import smptk.image.DiscreteImageDomain3D
import smptk.statisticalmodel.{LowRankGaussianProcessConfiguration}
import smptk.statisticalmodel.GaussianProcess._
import smptk.kernels._

class KernelTransformationTests extends FunSpec with ShouldMatchers {

  // TODO add a  test for testing the posterior kernel

  describe("The Nystroem approximation of a Kernel matrix") {
    it("Is close enough to a scalar valued kernel matrix") {
      val kernel = GaussianKernel1D(20)
      val domain = DiscreteImageDomain1D(CoordVector1D(-5f), CoordVector1D(2f), CoordVector1D(100))

      val sampler = UniformSampler1D()

      val eigPairs = Kernel.computeNystromApproximation(kernel, domain, 100, 500, sampler)

      def approxKernel(x: Point1D, y: Point1D) = {
        (0 until eigPairs.size).foldLeft(0.)((sum, i) => {
          val (lambda_i, phi_i) = eigPairs(i)
          sum + lambda_i * phi_i(x)(0) * phi_i(y)(0)
        })
      }

      for (x <- domain.points.slice(0, 10); y <- domain.points.slice(0, 10)) {
        val v1 = kernel(x, y)(0, 0)
        val v2 = approxKernel(x, y)
        (v2 should be(v1 plusOrMinus 0.001f))

      }
    }


    ignore("Its eigenvalues are close enough to the real eigenvalues for 1D") {
      val kernelDim = 1
      val scalarKernel = GaussianKernel1D(10)
      val domain = DiscreteImageDomain1D(CoordVector1D(0.), CoordVector1D(0.5), CoordVector1D(200))
      val sampler = UniformSampler1D()
      val eigPairsApprox = Kernel.computeNystromApproximation[CoordVector1D](scalarKernel, domain, 10, 500, sampler)
      val approxLambdas = eigPairsApprox.map(_._1)
      
      val realKernelMatrix = DenseMatrix.zeros[Double](domain.numberOfPoints * kernelDim, domain.numberOfPoints * kernelDim)

      for (i <- 0 until domain.numberOfPoints; j <- 0 until domain.numberOfPoints; di <- 0 until kernelDim; dj <- 0 until kernelDim) {
        realKernelMatrix(i * kernelDim + di, j * kernelDim + dj) = scalarKernel(domain.points(i), domain.points(j))(di, dj)
      }

      //val (_,realrealLambdas,_) = breeze.linalg.svd(realKernelMatrix)
      val (_, realLambdas, _) = RandomSVD.computeSVD(realKernelMatrix * (domain.volume / domain.numberOfPoints), eigPairsApprox.size)

      for (l <- approxLambdas.zipWithIndex)
        l._1 should be(realLambdas(l._2) plusOrMinus (0.1))

    }

    ignore("Its eigenvalues are close enough to the real eigenvalues in 2D") {

      val kernelDim = 2
      val scalarKernel = GaussianKernel2D(10)
      val ndKernel = UncorrelatedKernelND[CoordVector2D](scalarKernel, kernelDim)
      val domain = DiscreteImageDomain2D(CoordVector2D(0., 0.), CoordVector2D(1, 1), CoordVector2D(20, 20))
      val sampler = UniformSampler2D()


      val eigPairsApprox = Kernel.computeNystromApproximation[CoordVector2D](ndKernel, domain, 10, 500, sampler)
      val approxLambdas = eigPairsApprox.map(_._1)
  
      
      val realKernelMatrix = DenseMatrix.zeros[Double](domain.numberOfPoints * kernelDim, domain.numberOfPoints * kernelDim)

      for (i <- 0 until domain.numberOfPoints; j <- 0 until domain.numberOfPoints; di <- 0 until kernelDim; dj <- 0 until kernelDim) {
        realKernelMatrix(i * kernelDim + di, j * kernelDim + dj) = ndKernel(domain.points(i), domain.points(j))(di, dj)
      }

      //val (_,realrealLambdas,_) = breeze.linalg.svd(realKernelMatrix)

      val (_, realLambdas, _) = RandomSVD.computeSVD(realKernelMatrix * (domain.volume / domain.numberOfPoints), eigPairsApprox.size)
      println("approx lambdas " +approxLambdas)
      println("real lambdas " +realLambdas)

      for (l <- approxLambdas.zipWithIndex)
        l._1 should be(realLambdas(l._2) plusOrMinus (0.1))

    }

    ignore("The eigenValues are independent of the spacing") {
      val kernelDim = 2
      val scalarKernel = GaussianKernel2D(10)
      val ndKernel = UncorrelatedKernelND[CoordVector2D](scalarKernel, kernelDim)
      val domain = DiscreteImageDomain2D(CoordVector2D(0., 0.), CoordVector2D(1., 1.), CoordVector2D(20, 20))
      val domain2 = DiscreteImageDomain2D(CoordVector2D(0., 0.), CoordVector2D(0.5, 0.5), CoordVector2D(20, 20))
      val sampler = UniformSampler2D()

      val eigPairsDomain1 = Kernel.computeNystromApproximation[CoordVector2D](ndKernel, domain, 10, 500, sampler)
      val lambdasDomain1 = eigPairsDomain1.map(_._1)
      val eigPairsDomain2 = Kernel.computeNystromApproximation[CoordVector2D](ndKernel, domain2, 10, 500, sampler)
      val lambdasDomain2 = eigPairsDomain2.map(_._1)

      for (l <- lambdasDomain1.zipWithIndex)
        l._1 should be((lambdasDomain2(l._2)) plusOrMinus (0.001))
    }

    ignore("It leads to orthogonal basis functions on the domain (-5, 5)") {
      val kernel = GaussianKernel1D(20)
      val domain = DiscreteImageDomain1D(CoordVector1D(-5f), CoordVector1D(2f), CoordVector1D(100))
      val sampler = UniformSampler1D()

      val eigPairs = Kernel.computeNystromApproximation(kernel, domain, 100, 500, sampler)


      val integrator = Integrator[CoordVector1D](IntegratorConfiguration(UniformSampler1D(), domain.numberOfPoints))

      for (i <- 0 until 20) {

    	val (lambda_i, phi_i) = eigPairs(i)
        val phiImg = new ContinuousScalarImage1D(domain.isInside, (x: Point1D) => phi_i(x)(0) * phi_i(x)(0), Some(Point1D => DenseVector[Double](0.)))

        val v = integrator.integrateScalar(phiImg, domain)
        v should be(1. plusOrMinus 0.1)
      }
    }

    describe("A kernel transformation") {
      ignore("can be used to get the correct parameters in 1d (doing registration)") {

        val domain = DiscreteImageDomain1D(-5., 0.1, 1000)
        val discreteImage = DiscreteScalarImage1D(domain, domain.points.map(x => x(0)).toIndexedSeq)
        val continuousImg = Interpolation.interpolate1D(3)(discreteImage)

        val gk = GaussianKernel1D(0.1)

        val gp = createLowRankGaussianProcess1D(LowRankGaussianProcessConfiguration(domain, (x: CoordVector1D[Double]) => DenseVector(0.), gk, 10, 500))

        val regConf = RegistrationConfiguration[CoordVector1D](
          regularizationWeight = 0.0,
          optimizer = GradientDescentOptimizer(GradientDescentConfiguration(100, 0.3)),
          integrator = Integrator[CoordVector1D](IntegratorConfiguration(UniformSampler1D(), domain.numberOfPoints)),
          metric = MeanSquaresMetric1D(),
          transformationSpace = KernelTransformationSpace1D(KernelTransformationSpaceConfiguration(gp)),
          regularizer = RKHSNormRegularizer)

        val kernelSpace = regConf.transformationSpace
        val parameterVector = DenseVector.ones[Double](10) * 1.
        val transform = kernelSpace(parameterVector)
        val transformedImg = continuousImg compose transform

        val regResult = Registration.registration1D(regConf)(transformedImg, continuousImg)(domain)

        //        Utils.show1D(continuousImg, domain)
        //        Utils.show1D(transformedImg, domain)
        //        Utils.show1D(continuousImg.warp(regResult(domain).transform, domain.isInside), domain)
        //Utils.show1D(continuousImg compose regResult(domain).transform , domain)

        for (i <- 0 until parameterVector.size)
          regResult.parameters(i) should be(parameterVector(i) plusOrMinus 0.001)

      }

      ignore("can be used to get the correct parameters in 2d (doing registration)") {

        val testImgUrl = "/home/bouabene/dmFullBone.h5"

        val discreteFixedImage = ImageIO.read2DScalarImage[Float](new File(testImgUrl)).get

        val fixedImage = Interpolation.interpolate2D(3)(discreteFixedImage)

        val domain = discreteFixedImage.domain

        // Define a transformation    
        val gk = UncorrelatedKernelND(GaussianKernel2D(400), 2)

        val gp = createLowRankGaussianProcess2D(LowRankGaussianProcessConfiguration(domain, (x: CoordVector2D[Double]) => DenseVector(0., 0.), gk, 2, 500))

        val kernelTransformConfig = KernelTransformationSpaceConfiguration[CoordVector2D](gp, true)
        val transformSpace = KernelTransformationSpace2D(kernelTransformConfig)

        val parameterVector = DenseVector[Double](25., 35.)
        val transform = transformSpace(parameterVector)

        val warpedImage = fixedImage compose transform

        //Utils.show2D(warpedImage, domain)

        val regConf = RegistrationConfiguration[CoordVector2D](
          regularizationWeight = 0.0,
          optimizer = GradientDescentOptimizer(GradientDescentConfiguration(200, 0.000001, false, true, 0.602)),
          //optimizer = LBFGSOptimizer(LBFGSOptimizerConfiguration(100)),
          //optimizer = BreezeStochGradOptimizer(BreezeStochGradOptimizerConfiguration(100, 1.)),
          integrator = Integrator[CoordVector2D](IntegratorConfiguration(UniformDistributionRandomSampler2D(), 200)),
          //integrator = Integrator[CoordVector2D](IntegratorConfiguration(UniformDistributionRandomSampler2D(), 300)),
          //metric = MeanSquaresMetric2D(MeanSquaresMetricConfiguration()),
          metric = MeanSquaresMetric2D(),
          transformationSpace = KernelTransformationSpace2D(kernelTransformConfig),
          regularizer = RKHSNormRegularizer)

        val registration = Registration.registration2D(regConf)(warpedImage, fixedImage)
        val regResult = registration(domain)

        //        Utils.show2D(fixedImage compose regResult.transform, domain)

        (regResult.parameters(0) should be(parameterVector(0) plusOrMinus 0.0001))
        (regResult.parameters(1) should be(parameterVector(1) plusOrMinus 0.0001))

      }

      ignore("can be used to get the correct parameters in 2d (doing registration with image pyramids)") {
        val testImgUrl = getClass().getResource("/dm128.h5")

        val discreteFixedImage = ImageIO.read2DScalarImage[Float](new File(testImgUrl.getPath)).get

        val originalImage = Interpolation.interpolate2D(3)(discreteFixedImage)

        val domain = discreteFixedImage.domain

        // Define a transformation    
        val gk = UncorrelatedKernelND(GaussianKernel2D(50.), 2)

        val gp = createLowRankGaussianProcess2D(LowRankGaussianProcessConfiguration(domain, (x: CoordVector2D[Double]) => DenseVector(0., 0.), gk, 2, 500))

        val kernelTransformConfig = KernelTransformationSpaceConfiguration[CoordVector2D](gp, true)
        val transformSpace = KernelTransformationSpace2D(kernelTransformConfig)

        val parameterVector = DenseVector[Double](20., 10.)
        val transform = transformSpace(parameterVector)

        val warpedImage = originalImage compose transform

        // Utils.show2D(warpedImage, domain)

        def pyramidRegistation(fixedImage: ContinuousScalarImage2D, movingImage: ContinuousScalarImage2D,
          domain: DiscreteImageDomain2D,
          regConf: RegistrationConfiguration[CoordVector2D],
          deviations: List[Double],
          latestRegResults: Option[RegistrationResult[CoordVector2D]]): RegistrationResult[CoordVector2D] = {

          if (deviations.size > 0)
            println("Pyramid registration for deviation " + deviations(0))

          val integrator = Integrator[CoordVector2D](IntegratorConfiguration(UniformSampler2D(), 50))
          val image = if (deviations.size == 0) movingImage else Utils.gaussianSmoothing2D(movingImage, deviations(0), integrator)
          val smoothedFixedImage = if (deviations.size == 0) fixedImage else Utils.gaussianSmoothing2D(fixedImage, deviations(0), integrator)

          val lastParams = if (latestRegResults.isDefined) latestRegResults.get.parameters else regConf.initialParameters

          val newRegConf = RegistrationConfiguration[CoordVector2D](
            regularizationWeight = regConf.regularizationWeight,
            optimizer = regConf.optimizer,
            integrator = regConf.integrator,
            metric = regConf.metric,
            transformationSpace = regConf.transformationSpace,
            regularizer = RKHSNormRegularizer,
            initialParametersOrNone = Some(lastParams))

          val registration = Registration.registration2D(newRegConf)(smoothedFixedImage, image)
          val regResult = registration(domain)

          if (deviations.size > 0)
            println("parameters for smoothing " + deviations(0) + " = " + regResult.parameters)

          if (deviations.size == 0)
            regResult
          else
            pyramidRegistation(fixedImage, movingImage, domain, regConf, deviations.tail, Some(regResult))
        }

        val decreasingDeviations = List(8., 6., 4., 1.)

        val regConf = RegistrationConfiguration[CoordVector2D](
          regularizationWeight = 0.0,
          //optimizer = GradientDescentOptimizer(GradientDescentConfiguration(15, 0.00003, false, true, 0.602)),
          optimizer = LBFGSOptimizer(LBFGSOptimizerConfiguration(20)),
          //optimizer = BreezeStochGradOptimizer(BreezeStochGradOptimizerConfiguration(100, 1.)), 

          integrator = Integrator[CoordVector2D](IntegratorConfiguration(UniformDistributionRandomSampler2D(), 124)),
          //integrator = Integrator[CoordVector2D](IntegratorConfiguration(UniformSampler2D(124))),

          //metric = MeanSquaresMetric2D(MeanSquaresMetricConfiguration()),
          metric = MeanSquaresMetric2D(),
          transformationSpace = KernelTransformationSpace2D(kernelTransformConfig),
          regularizer = RKHSNormRegularizer)

        val regResult = pyramidRegistation(warpedImage, originalImage, domain, regConf, decreasingDeviations, None)

        Utils.show2D(originalImage compose regResult.transform, domain)

        (regResult.parameters(0) should be(parameterVector(0) plusOrMinus 0.0001))
        (regResult.parameters(1) should be(parameterVector(1) plusOrMinus 0.0001))
      }
    }
  }
}