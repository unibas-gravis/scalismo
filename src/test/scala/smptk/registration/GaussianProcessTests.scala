package smptk
package registration

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import image.Geometry.{ CoordVector1D, CoordVector2D, CoordVector3D }
import image.CoordVector
import image.DiscreteImageDomain1D
import image.Geometry.implicits._
import breeze.linalg.DenseVector
import smptk.image.DiscreteImageDomain2D
import smptk.io.MeshIO
import smptk.image.Utils
import smptk.mesh.TriangleMesh
import smptk.mesh.TriangleMeshDomain
import smptk.numerics.UniformSampler1D
import breeze.plot.Figure
import smptk.common.BoxedRegion1D
import smptk.image.DiscreteImageDomain3D
import java.io.File
import GaussianProcess._


class GaussianProcessTests extends FunSpec with ShouldMatchers {
  describe("A Gaussian process regression") {
    ignore("keeps the landmark points fixed for a 1D case") {
      val domain = DiscreteImageDomain1D(-5., 0.1, 100)
      val config = LowRankGaussianProcessConfiguration[CoordVector1D](domain, _ => DenseVector(0.), GaussianKernel1D(5), 100, 500)
      val gp = GaussianProcess.createLowRankGaussianProcess1D(config)

      val trainingData = IndexedSeq((-3., 1.), (-1., 3.), (0., -1.), (1., -1.), (3., 0.)).map(t => (CoordVector1D(t._1), DenseVector(t._2)))
      val posteriorGP = GaussianProcess.regression(gp, trainingData, 1e-8)

      for ((x, y) <- trainingData) {
        (posteriorGP.mean(x)(0) should be(y(0) plusOrMinus 1e-1))
      }
    }
  }

  ignore("keeps the landmark points fixed for a 2D case") {
    val domain = DiscreteImageDomain2D(CoordVector2D(-5., -5.), CoordVector2D(0.1, 0.1), CoordVector2D(100, 100))
    val config = LowRankGaussianProcessConfiguration[CoordVector2D](domain, _ => DenseVector(0., 0.), UncorrelatedKernelND(GaussianKernel2D(5), 2), 100, 200)
    val gp = GaussianProcess.createLowRankGaussianProcess2D(config)

    val trainingData = IndexedSeq(((-3., -3.), (1., 1.)), ((-1., 3.), (0., -1.))).map(t => (CoordVector2D(t._1(0), t._1(1)), DenseVector(t._2(0), t._2(1))))
    val posteriorGP = GaussianProcess.regression(gp, trainingData, 1e-5)

    for ((x, y) <- trainingData) {
      (posteriorGP.mean(x)(0) should be(y(0) plusOrMinus 0.0001))
      (posteriorGP.mean(x)(1) should be(y(1) plusOrMinus 0.0001))
    }
  }


  ignore("keeps the landmark points fixed for a 3D case") {
    val domain = DiscreteImageDomain3D(CoordVector3D(-5., -5., -5), CoordVector3D(0.1, 0.1, 0.1), CoordVector3D(100, 100, 100))
    val config = LowRankGaussianProcessConfiguration[CoordVector3D](domain, _ => DenseVector(0., 0., 0.), UncorrelatedKernelND(GaussianKernel3D(5), 3), 100, 400)
    val gp = GaussianProcess.createLowRankGaussianProcess3D(config)


    val trainingData = IndexedSeq(((-3., -3., -1.), (1., 1., 2.)), ((-1., 3., 0.), (0., -1., 0.))).map(t => (CoordVector3D(t._1(0), t._1(1), t._1(2)), DenseVector(t._2(0), t._2(1), t._2(2))))
    val posteriorGP = GaussianProcess.regression(gp, trainingData, 1e-5)

    for ((x, y) <- trainingData) {
      (posteriorGP.mean(x)(0) should be(y(0) plusOrMinus 0.0001))
      (posteriorGP.mean(x)(1) should be(y(1) plusOrMinus 0.0001))
      (posteriorGP.mean(x)(2) should be(y(2) plusOrMinus 0.0001))
    }


  }
  
  describe("An optimized (Phi(x) cached) Gaussian Process"){
    
    val faceMesh = MeshIO.readHDF5(new File("src/test/resources/facemesh.h5")).get

    val domain = faceMesh.boundingBox
    val kernel = UncorrelatedKernelND(GaussianKernel3D(100,1), 3)
    val nbBasisFunctions = 100
    
    val config = LowRankGaussianProcessConfiguration(domain, (x:CoordVector3D[Double]) => DenseVector(0.,0.,0.),   kernel, nbBasisFunctions, 500)
    val gp = createLowRankGaussianProcess3D(config)
    
    val optimizedGP = gp.optimizeForPoints(faceMesh.domain.points.toIndexedSeq)
    
    it("returns the same values as the non-cached one") {
      val alphas = DenseVector(new breeze.stats.distributions.Uniform(0,100).sample(nbBasisFunctions).toArray)
      for(p <-faceMesh.domain.points.toIndexedSeq) assert(gp.instance(alphas)(p) === optimizedGP.instance(alphas)(p))
          
    } 
    
    it("Throws an exception when used on points other than the one it was optimized for ") {
      intercept[Exception]{
        val thisShouldThrowException = optimizedGP.instance(DenseVector.zeros[Double](nbBasisFunctions))(CoordVector3D(0.,0.,0.))
      }     
    }
    
  }

}
