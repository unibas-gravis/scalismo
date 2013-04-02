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
import smptk.numerics.Integration
import smptk.image.DiscreteImageDomain2D
import smptk.numerics.RandomSVD

class KernelTransformationTests extends FunSpec with ShouldMatchers {

  // TODO add a  test for testing the posterior kernel

  describe("The Nystroem approximation of a Kernel matrix") {
    ignore("Is close enough to a scalar valued kernel matrix") {
      val kernel = GaussianKernel1D(20)
      val domain = DiscreteImageDomain1D(CoordVector1D(-5f), CoordVector1D(2f), CoordVector1D(100))
      val (eigenPairs, numParams) = Kernel.computeNystromApproximation(kernel, domain, 100)
      println("numParams : " +numParams)
      println("eigParis.size : " +eigenPairs.size)
      def approxKernel(x: Point1D, y: Point1D) = {
        eigenPairs.foldLeft(0.)((sum, eigenPair) => {
          val (lmbda, phi) = eigenPair
          sum +  lmbda * phi(x)(0) * phi(y)(0)
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

      ignore("It's eigenvalues are close enough to the real eigenvalues ") {
        val kernelDim = 2
        val scalarKernel = GaussianKernel2D(10)
        val ndKernel = UncorrelatedKernelND[CoordVector2D](scalarKernel, kernelDim)
        val domain = DiscreteImageDomain2D(CoordVector2D(0., 0.), CoordVector2D(1., 1.), CoordVector2D(20, 20))
        val (eigenPairs, numParams) = Kernel.computeNystromApproximation[CoordVector2D](ndKernel, domain, 10)
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

      ignore("Real eigenvalues are independant of the spacing ") {
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
        
         for (i <- 0 until  realLambdas1.size)
          realLambdas1(i) should be(realLambdas2(i) plusOrMinus (0.1))
       
      }
      ignore("The eigenValues are independent of the spacing") {
        val kernelDim = 2
        val scalarKernel = GaussianKernel2D(10)
        val ndKernel = UncorrelatedKernelND[CoordVector2D](scalarKernel, kernelDim)
        val domain = DiscreteImageDomain2D(CoordVector2D(0., 0.), CoordVector2D(1., 1.), CoordVector2D(20, 20))
        val domain2 = DiscreteImageDomain2D(CoordVector2D(0., 0.), CoordVector2D(0.5, 0.5), CoordVector2D(20, 20))
        val (eigenPairs, numParams) = Kernel.computeNystromApproximation[CoordVector2D](ndKernel, domain, 10)
        val lambdasDomain1 = eigenPairs.map(_._1)
        val (eigenPairs2, numParams2) = Kernel.computeNystromApproximation[CoordVector2D](ndKernel, domain2, 10)
        val lambdasDomain2 = eigenPairs2.map(_._1)

        for (l <- lambdasDomain1.zipWithIndex)
          l._1 should be(lambdasDomain2(l._2) plusOrMinus (0.001))
      }

     ignore("Is close enough to a scalar valued polynomial kernel matrix") {
      val kernel = PolynomialKernel1D(1)
      val domain = DiscreteImageDomain1D(CoordVector1D(-5f), CoordVector1D(2f), CoordVector1D(100))
      val (eigenPairs, numParams) = Kernel.computeNystromApproximation(kernel, domain, 100)
      println("numParams : " +numParams)
      println("eigParis.size : " +eigenPairs.size)
      def approxKernel(x: Point1D, y: Point1D) = {
        eigenPairs.foldLeft(0.)((sum, eigenPair) => {
          val (lmbda, phi) = eigenPair
          sum +  lmbda * phi(x)(0) * phi(y)(0)
        })
      }
      
      for (x <- domain.points.slice(0, 10); y <- domain.points.slice(0, 10)) {
        val v1 = kernel(x, y)(0, 0)
        val v2 = approxKernel(x, y)
        (v2 should be(v1 plusOrMinus 0.001f))

      }
    }

     it("It leads to the same deformation for two different domains") {
      val kernel1 = GaussianKernel1D(100.0)
      val kernel2 = GaussianKernel1D(100.0)
      val domain1 = DiscreteImageDomain1D(CoordVector1D(-5f), CoordVector1D(2f), CoordVector1D(100))
      val domain2 = DiscreteImageDomain1D(CoordVector1D(-5f), CoordVector1D(2f), CoordVector1D(400))

      val (eigPairs1, _) = Kernel.computeNystromApproximation(kernel1, domain1, 1)
      val (eigPairs2, _) = Kernel.computeNystromApproximation(kernel2, domain2, 1)
      for (x <- domain1.points) { 
        val (lambda1, phi1) = eigPairs1(0)
        val (lambda2, phi2) = eigPairs2(0)
        println("lambda2 : " + (lambda1, lambda2))
        println("vals: : " +(phi1(x)(0), phi2(x)(0)))
        println("normalized: " +(phi1(x)(0) * math.sqrt(lambda1), phi2(x)(0) * math.sqrt(lambda2)))
        (phi1(x)(0) * math.sqrt(lambda1)) should be ((phi2(x)(0) * math.sqrt(lambda2)) plusOrMinus 0.01f)
      }
     }
   
    
    ignore("Is leads to orthogonal basis functions on the domain (-5, 5)") { 
      val kernel = GaussianKernel1D(20)
      val domain = DiscreteImageDomain1D(CoordVector1D(-5f), CoordVector1D(2f), CoordVector1D(100))
      val (eigenPairs, numParams) = Kernel.computeNystromApproximation(kernel, domain, 100)
      
      for ((lambda, phi) <- eigenPairs.take(20)) { 
          val phiImg = new ContinuousScalarImage1D(domain.isInside, (x : Point1D) =>  phi(x)(0) * phi(x)(0), Point1D => DenseVector[Double](0.))
      	  val v = Integration.integrate(phiImg , domain)	
	  v should be (1. plusOrMinus 0.1)
      }                 
    }

    ignore("Is leads to orthogonal basis functions on the domain (-1, 3)") {
      val kernel = GaussianKernel1D(20)
      val domain = DiscreteImageDomain1D(CoordVector1D(-1f), CoordVector1D(0.2f), CoordVector1D(200))
      val (eigenPairs, numParams) = Kernel.computeNystromApproximation(kernel, domain, 100)
      
      for ((lambda, phi) <- eigenPairs.take(20)) { 
      print("lambda: " +lambda)
          val phiImg = new ContinuousScalarImage1D(domain.isInside, (x : Point1D) =>  phi(x)(0) * phi(x)(0), Point1D => DenseVector[Double](0.))
      	  val v = Integration.integrate(phiImg , domain)	
	  v should be (1. plusOrMinus 0.1)
      }                 
    }

    ignore("Is close enough for a 2D matrix valued kernel") {
      val kernelDim = 2
      val scalarKernel = GaussianKernel1D(10)

      val ndKernel = UncorrelatedKernelND[CoordVector1D](scalarKernel, kernelDim)
      val domain = DiscreteImageDomain1D(CoordVector1D(-5f), CoordVector1D(2f), CoordVector1D(100))
      val (eigenPairs, numParams) = Kernel.computeNystromApproximation(ndKernel, domain, 100)
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

      val (eigenPairs1, _) = Kernel.computeNystromApproximation(kernel, domain1, 100)
      val (eigenPairs2, _) = Kernel.computeNystromApproximation(kernel, domain2, 100)      

      def approxKernel(eigenPairs : IndexedSeq[(Double, CoordVector1D[Double]=>DenseVector[Double])], x: Point1D, y: Point1D) = {

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
    ignore("can be used to get the correct parameters in 1d (doing registration)") {

      val domain = DiscreteImageDomain1D(-5., 0.1, 1000)
      val discreteImage = DiscreteScalarImage1D(domain, domain.points.map(x => x(0)))
      val continuousImg = Interpolation.interpolate(3)(discreteImage)

      val gk = GaussianKernel1D(0.1)
      val gp = GaussianProcess[CoordVector1D]((x: Point1D) => DenseVector(0.), gk)
      val kernelSpace = KernelTransformationSpace1D(domain, 50, gp)

      val transform = kernelSpace(DenseVector.ones[Double](50) * 1.)
      val transformedImg = continuousImg compose transform

      //      val f = Figure()
      //      val p = f.subplot(0)
      //      
      //      val xs = domain.points
      //      val eigPairs = Kernel.computeNystromApproximation(gp.k, domain, 5)
      //      for ((lmbda, phi) <- eigPairs) { 
      //    	  p += plot(xs.map(_(0)), xs.map(x => phi(x)))
      //      }
      //	  f.refresh      
      //      Utils.showGrid1D(domain, transform)

      val regResult = Registration.registration1D(transformedImg, continuousImg, kernelSpace, MeanSquaresMetric1D,
        0f, DenseVector.zeros[Double](50))

      Utils.show1D(continuousImg, domain)
      Utils.show1D(transformedImg, domain)
      Utils.show1D(continuousImg.warp(regResult(domain).transform, domain.isInside), domain)
      //Utils.show1D(continuousImg compose regResult(domain).transform , domain)

    }
  }

  ignore("can be used to get the correct parameters in 2d (doing registration)") {

    val testImgUrl = getClass().getResource("/lena.h5").getPath()
    val discreteFixedImage = ImageIO.read2DScalarImage[Short](new File(testImgUrl)).get
    val fixedImage = Interpolation.interpolate2D(3)(discreteFixedImage)

    val domain = discreteFixedImage.domain
    val center = CoordVector2D(domain.origin(0) + domain.extent(0) / 2, domain.origin(1) + domain.extent(1) / 2)

    val gk = UncorrelatedKernelND[CoordVector2D](GaussianKernel2D(10.), 2)
    val gp = GaussianProcess[CoordVector2D]((x: CoordVector2D[Double]) => DenseVector(0., 0.), gk)

    val transformSpace = KernelTransformationSpace2D(domain, 1, gp)
    val kernelTransform = transformSpace(DenseVector(15.))
    val transformedLena = fixedImage compose kernelTransform
    Utils.show2D(transformedLena, domain)
    val registration = Registration.registration2D(fixedImage, transformedLena, transformSpace, MeanSquaresMetric2D,
      0f, DenseVector(0.))

    val regResult = registration(domain)

    (regResult.parameters(0) should be(1. plusOrMinus 0.0001))
    //(regResult.parameters(0) should be (-3.14/20 plusOrMinus 0.0001))

  }

}