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
import smptk.image.Geometry.Point2D
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
import smptk.numerics.UniformSampler1D

class KernelTransformationTests extends FunSpec with ShouldMatchers {

  // TODO add a  test for testing the posterior kernel

  describe("The Nystroem approximation of a Kernel matrix") {
    it("Is close enough to a scalar valued kernel matrix") {
      val kernel = GaussianKernel1D(20)
      val domain = DiscreteImageDomain1D(CoordVector1D(-5f), CoordVector1D(2f), CoordVector1D(100))
      
      val sampler = UniformSampler1D()
      val (eigenPairs, numParams) = Kernel.computeNystromApproximation(kernel, domain, 100, 500, sampler)
      println("numParams : " + numParams)
      println("eigParis.size : " + eigenPairs.size)
      def approxKernel(x: Point1D, y: Point1D) = {
        eigenPairs.foldLeft(0.)((sum, eigenPair) => {
          val (lmbda, phi) = eigenPair
          sum + lmbda * phi(x)(0) * phi(y)(0)
        })
      }

      for (x <- domain.points.slice(0, 10); y <- domain.points.slice(0, 10)) {
        val v1 = kernel(x, y)(0, 0)
        val v2 = approxKernel(x, y)
        (v2 should be(v1 plusOrMinus 0.001f))

      }
    }

    def approxKernel(x: Point2D, y: Point2D, kernelDim: Int, eigenPairs: IndexedSeq[(Double, CoordVector2D[Double] => DenseVector[Double])]) = {
      val zero = DenseMatrix.zeros[Double](kernelDim, kernelDim)
      eigenPairs.foldLeft(zero)((sum, eigenPair) => {
        val (lmbda, phi) = eigenPair
        sum + phi(x) * phi(y).t * lmbda
      })
    }

    it("It's eigenvalues are close enough to the real eigenvalues ") {
      val kernelDim = 2
      val scalarKernel = GaussianKernel2D(10)
      val ndKernel = UncorrelatedKernelND[CoordVector2D](scalarKernel, kernelDim)
      val domain = DiscreteImageDomain2D(CoordVector2D(0., 0.), CoordVector2D(1., 1.), CoordVector2D(20, 20))
      val sampler = UniformSampler2D()
      val (eigenPairs, numParams) = Kernel.computeNystromApproximation[CoordVector2D](ndKernel, domain, 10, 500, sampler)
      val approxLambdas = eigenPairs.map(_._1)

      val realKernelMatrix = DenseMatrix.zeros[Double](domain.numberOfPoints * kernelDim, domain.numberOfPoints * kernelDim)

      for (i <- 0 until domain.numberOfPoints; j <- 0 until domain.numberOfPoints; di <- 0 until kernelDim; dj <- 0 until kernelDim) {
        realKernelMatrix(i * kernelDim + di, j * kernelDim + dj) = ndKernel(domain.points(i), domain.points(j))(di, dj)
      }

      //val (_,realrealLambdas,_) = breeze.linalg.svd(realKernelMatrix)
      val (_, realLambdas, _) = RandomSVD.computeSVD(realKernelMatrix, numParams)

      for (l <- approxLambdas.zipWithIndex)
        l._1 should be(realLambdas(l._2) plusOrMinus (0.1))

    }

    it("Real eigenvalues are independant of the spacing ") {
      val kernelDim = 2
      val scalarKernel = GaussianKernel2D(10)
      val ndKernel = UncorrelatedKernelND[CoordVector2D](scalarKernel, kernelDim)
      val domain = DiscreteImageDomain2D(CoordVector2D(0., 0.), CoordVector2D(1., 1.), CoordVector2D(20, 20))
      val domain2 = DiscreteImageDomain2D(CoordVector2D(0., 0.), CoordVector2D(0.5, 0.5), CoordVector2D(20, 20))

      val realKernelMatrix1 = DenseMatrix.zeros[Double](domain.numberOfPoints * kernelDim, domain.numberOfPoints * kernelDim)

      for (i <- 0 until domain.numberOfPoints; j <- 0 until domain.numberOfPoints; di <- 0 until kernelDim; dj <- 0 until kernelDim) {
        realKernelMatrix1(i * kernelDim + di, j * kernelDim + dj) = ndKernel(domain.points(i), domain.points(j))(di, dj)
      }

      val (_, realLambdas1, _) = RandomSVD.computeSVD(realKernelMatrix1, 10)

      val realKernelMatrix2 = DenseMatrix.zeros[Double](domain2.numberOfPoints * kernelDim, domain2.numberOfPoints * kernelDim)

      for (i <- 0 until domain2.numberOfPoints; j <- 0 until domain2.numberOfPoints; di <- 0 until kernelDim; dj <- 0 until kernelDim) {
        realKernelMatrix2(i * kernelDim + di, j * kernelDim + dj) = ndKernel(domain2.points(i), domain2.points(j))(di, dj)
      }

      val (_, realLambdas2, _) = RandomSVD.computeSVD(realKernelMatrix2, 10)

      for (i <- 0 until realLambdas1.size)
        realLambdas1(i) should be(realLambdas2(i) plusOrMinus (0.1))

    }
    ignore("The eigenValues are independent of the spacing") {
      val kernelDim = 2
      val scalarKernel = GaussianKernel2D(10)
      val ndKernel = UncorrelatedKernelND[CoordVector2D](scalarKernel, kernelDim)
      val domain = DiscreteImageDomain2D(CoordVector2D(0., 0.), CoordVector2D(1., 1.), CoordVector2D(20, 20))
      val domain2 = DiscreteImageDomain2D(CoordVector2D(0., 0.), CoordVector2D(0.5, 0.5), CoordVector2D(20, 20))
      val sampler = UniformSampler2D()
      val (eigenPairs, numParams) = Kernel.computeNystromApproximation[CoordVector2D](ndKernel, domain, 10, 500, sampler)
      val lambdasDomain1 = eigenPairs.map(_._1)
      val (eigenPairs2, numParams2) = Kernel.computeNystromApproximation[CoordVector2D](ndKernel, domain2, 10, 500, sampler)
      val lambdasDomain2 = eigenPairs2.map(_._1)

      for (l <- lambdasDomain1.zipWithIndex)
        l._1 should be(lambdasDomain2(l._2) plusOrMinus (0.001))
    }

    it("Is close enough to a scalar valued polynomial kernel matrix") {
      val kernel = PolynomialKernel1D(1)
      val domain = DiscreteImageDomain1D(CoordVector1D(-5f), CoordVector1D(2f), CoordVector1D(100))
      val sampler = UniformSampler1D()
      val (eigenPairs, numParams) = Kernel.computeNystromApproximation(kernel, domain, 100, 500, sampler)
      println("numParams : " + numParams)
      println("eigParis.size : " + eigenPairs.size)
      def approxKernel(x: Point1D, y: Point1D) = {
        eigenPairs.foldLeft(0.)((sum, eigenPair) => {
          val (lmbda, phi) = eigenPair
          sum + lmbda * phi(x)(0) * phi(y)(0)
        })
      }

      for (x <- domain.points.slice(0, 10); y <- domain.points.slice(0, 10)) {
        val v1 = kernel(x, y)(0, 0)
        val v2 = approxKernel(x, y)
        (v2 should be(v1 plusOrMinus 0.001f))

      }
    }

    ignore("It leads to the same deformation for two different domains") {
      val kernel1 = GaussianKernel1D(100.0)
      val kernel2 = GaussianKernel1D(100.0)
      val domain1 = DiscreteImageDomain1D(CoordVector1D(-5f), CoordVector1D(2f), CoordVector1D(100))
      val domain2 = DiscreteImageDomain1D(CoordVector1D(-5f), CoordVector1D(2f), CoordVector1D(400))
      val sampler = UniformSampler1D()
      val (eigPairs1, _) = Kernel.computeNystromApproximation(kernel1, domain1, 1, 500, sampler)
      val (eigPairs2, _) = Kernel.computeNystromApproximation(kernel2, domain2, 1, 500, sampler)
      for (x <- domain1.points) {
        val (lambda1, phi1) = eigPairs1(0)
        val (lambda2, phi2) = eigPairs2(0)
        println("lambda2 : " + (lambda1, lambda2))
        println("vals: : " + (phi1(x)(0), phi2(x)(0)))
        println("normalized: " + (phi1(x)(0) * math.sqrt(lambda1), phi2(x)(0) * math.sqrt(lambda2)))
        (phi1(x)(0) * math.sqrt(lambda1)) should be((phi2(x)(0) * math.sqrt(lambda2)) plusOrMinus 0.01f)
      }
    }

    it("It leads to orthogonal basis functions on the domain (-5, 5)") {
      val kernel = GaussianKernel1D(20)
      val domain = DiscreteImageDomain1D(CoordVector1D(-5f), CoordVector1D(2f), CoordVector1D(100))
      val sampler = UniformSampler1D()
      val (eigenPairs, numParams) = Kernel.computeNystromApproximation(kernel, domain, 100, 500, sampler)

      val integrator = Integrator[CoordVector1D](IntegratorConfiguration(UniformSampler1D(), domain.numberOfPoints))

      for ((lambda, phi) <- eigenPairs.take(20)) {
        val phiImg = new ContinuousScalarImage1D(domain.isInside, (x: Point1D) => phi(x)(0) * phi(x)(0), Some(Point1D => DenseVector[Double](0.)))
        val v = integrator.integrateScalar(phiImg, domain)
        v should be(1. plusOrMinus 0.1)
      }
    }

    it("Is leads to orthogonal basis functions on the domain (-1, 3)") {
      val kernel = GaussianKernel1D(20)
      val domain = DiscreteImageDomain1D(CoordVector1D(-1f), CoordVector1D(0.2f), CoordVector1D(200))
      val sampler = UniformSampler1D()
      val (eigenPairs, numParams) = Kernel.computeNystromApproximation(kernel, domain, 100, 500, sampler)

      val integrator = Integrator[CoordVector1D](IntegratorConfiguration(UniformSampler1D(), domain.numberOfPoints))

      for ((lambda, phi) <- eigenPairs.take(20)) {
        print("lambda: " + lambda)
        val phiImg = new ContinuousScalarImage1D(domain.isInside, (x: Point1D) => phi(x)(0) * phi(x)(0), Some(Point1D => DenseVector[Double](0.)))
        val v = integrator.integrateScalar(phiImg, domain)
        v should be(1. plusOrMinus 0.1)
      }
    }

    it("Is close enough for a 2D matrix valued kernel") {
      val kernelDim = 2
      val scalarKernel = GaussianKernel1D(10)

      val ndKernel = UncorrelatedKernelND[CoordVector1D](scalarKernel, kernelDim)
      val domain = DiscreteImageDomain1D(CoordVector1D(-5f), CoordVector1D(2f), CoordVector1D(100))
      val sampler = UniformSampler1D()
      val (eigenPairs, numParams) = Kernel.computeNystromApproximation(ndKernel, domain, 100, 500, sampler)
      def approxKernel(x: Point1D, y: Point1D) = {
        val zero = DenseMatrix.zeros[Double](kernelDim, kernelDim)
        eigenPairs.foldLeft(zero)((sum, eigenPair) => {
          val (lmbda, phi) = eigenPair
          sum + phi(x) * phi(y).t * lmbda
        })
      }

      for (x <- domain.points; y <- domain.points; di <- 0 until kernelDim; dj <- 0 until kernelDim) {
        (approxKernel(x, y)(di, dj) should be(ndKernel(x, y)(di, dj) plusOrMinus 0.001f))

      }

    }

    ignore(" is independent of the spacing") {
      val kernelDim = 1
      val kernel = GaussianKernel1D(10)
      val domain1 = DiscreteImageDomain1D(CoordVector1D(5f), CoordVector1D(1f), CoordVector1D(150))
      val domain2 = DiscreteImageDomain1D(CoordVector1D(5f), CoordVector1D(2f), CoordVector1D(75))
      val sampler = UniformSampler1D()
      val (eigenPairs1, _) = Kernel.computeNystromApproximation(kernel, domain1, 100, 500, sampler)
      val (eigenPairs2, _) = Kernel.computeNystromApproximation(kernel, domain2, 100, 500, sampler)

      def approxKernel(eigenPairs: IndexedSeq[(Double, CoordVector1D[Double] => DenseVector[Double])], x: Point1D, y: Point1D) = {

        eigenPairs.foldLeft(0.)((sum, eigenPair) => {
          val (lmbda, phi) = eigenPair
          sum + lmbda * phi(x)(0) * phi(y)(0)
        })
      }

      for (x <- domain1.points; y <- domain1.points) {
        approxKernel(eigenPairs1, x, y) should be(approxKernel(eigenPairs2, x, y) plusOrMinus 0.001f)
      }

    }

  }

  describe("A kernel transformation") {
    it("can be used to get the correct parameters in 1d (doing registration)") {

      val domain = DiscreteImageDomain1D(-5., 0.1, 1000)
      val discreteImage = DiscreteScalarImage1D(domain, domain.points.map(x => x(0)).toIndexedSeq)
      val continuousImg = Interpolation.interpolate1D(3)(discreteImage)

      val gk = GaussianKernel1D(0.1)
      val gp = GaussianProcess[CoordVector1D](domain, (x: Point1D) => DenseVector(0.), gk)

      val regConf = RegistrationConfiguration[CoordVector1D](
        regularizationWeight = 0.0,
        optimizer = GradientDescentOptimizer(GradientDescentConfiguration(100, 0.001)),
        integrator = Integrator[CoordVector1D](IntegratorConfiguration(UniformSampler1D(), domain.numberOfPoints)),
        metric = MeanSquaresMetric1D(),
        transformationSpace = KernelTransformationSpace1D(KernelTransformationSpaceConfiguration(100, 500, gp)),
        regularizer = RKHSNormRegularizer)

      val kernelSpace = regConf.transformationSpace
      val transform = kernelSpace(DenseVector.ones[Double](50) * 1.)
      val transformedImg = continuousImg compose transform

      val regResult = Registration.registration1D(regConf)(transformedImg, continuousImg)

      Utils.show1D(continuousImg, domain)
      Utils.show1D(transformedImg, domain)
      Utils.show1D(continuousImg.warp(regResult(domain).transform, domain.isInside), domain)
      //Utils.show1D(continuousImg compose regResult(domain).transform , domain)

    }

    ignore("can be used to get the correct parameters in 2d (doing registration)") {

      val testImgUrl = "/home/bouabene/dmFullBone.h5"

      val discreteFixedImage = ImageIO.read2DScalarImage[Float](new File(testImgUrl)).get

      val fixedImage = Interpolation.interpolate2D(3)(discreteFixedImage)

      val domain = discreteFixedImage.domain

      // Define a transformation    
      val gk = UncorrelatedKernelND(GaussianKernel2D(400), 2)
      val gp = GaussianProcess[CoordVector2D](domain, (x: Point2D) => DenseVector(0., 0.), gk)

      val kernelTransformConfig = KernelTransformationSpaceConfiguration[CoordVector2D](2, 500, gp, true)
      val transformSpace = KernelTransformationSpace2D(kernelTransformConfig)

      val parameterVector = DenseVector[Double](400., 50.)
      val transform = transformSpace(parameterVector)

      val warpedImage = fixedImage compose transform

      //Utils.show2D(warpedImage, domain)

      val regConf = RegistrationConfiguration[CoordVector2D](
        regularizationWeight = 0.0,
        optimizer = GradientDescentOptimizer(GradientDescentConfiguration(100, 1, true)),
        //optimizer = LBFGSOptimizer(LBFGSOptimizerConfiguration(100)),
        //optimizer = BreezeStochGradOptimizer(BreezeStochGradOptimizerConfiguration(100, 1.)),
        integrator = Integrator[CoordVector2D](IntegratorConfiguration(UniformDistributionRandomSampler2D(), 300)),
        //metric = MeanSquaresMetric2D(MeanSquaresMetricConfiguration()),
        metric =  MeanSquaresMetric2D(),
        transformationSpace = KernelTransformationSpace2D(kernelTransformConfig),
        regularizer = RKHSNormRegularizer)

      val registration = Registration.registration2D(regConf)(warpedImage, fixedImage)
      val regResult = registration(domain)

      Utils.show2D(fixedImage compose regResult.transform, domain)

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
      val gp = GaussianProcess[CoordVector2D](domain, (x: Point2D) => DenseVector(0., 0.), gk)

      val kernelTransformConfig = KernelTransformationSpaceConfiguration[CoordVector2D](2, 500, gp, true)
      val transformSpace = KernelTransformationSpace2D(kernelTransformConfig)

      val parameterVector = DenseVector[Double](150., 50.)
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
        optimizer = GradientDescentOptimizer(GradientDescentConfiguration(50, 0.3, true)),
        //optimizer = LBFGSOptimizer(LBFGSOptimizerConfiguration(20)),
        //optimizer = BreezeStochGradOptimizer(BreezeStochGradOptimizerConfiguration(100, 1.)), 
        integrator = Integrator[CoordVector2D](IntegratorConfiguration(UniformDistributionRandomSampler2D(), 1000)),
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