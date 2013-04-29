package smptk
package registration


import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import image.Geometry.{CoordVector1D, CoordVector2D}
import image.CoordVector
import image.DiscreteImageDomain1D
import image.Geometry.implicits._
import breeze.linalg.DenseVector
import smptk.image.DiscreteImageDomain2D
import smptk.io.MeshIO
import smptk.image.Utils



class GaussianProcessTests extends FunSpec with ShouldMatchers {
  describe("A Gaussian process regression") { 
    it("keeps the landmark points fixed for a 1D case") {
      val domain = DiscreteImageDomain1D(-5., 0.1, 100)
      val gp = GaussianProcess[CoordVector1D](domain, _ => DenseVector(0.), GaussianKernel1D(10))
      
      val trainingData = IndexedSeq((-3., 1.), (-1., 3.), (0., -1.), (1., -1.), (3., 0.)).map(t => (CoordVector1D(t._1), DenseVector(t._2))) 
      val posteriorGP = GaussianProcess.regression(gp, trainingData, 0)
      
      for ((x,y) <- trainingData) { 
    	  (posteriorGP.mean(x)(0) should be(y(0) plusOrMinus 0.0001))
      }
    }
  }
  
  it("keeps the landmark points fixed for a 2D case") {
      val domain = DiscreteImageDomain2D(CoordVector2D(0.,0.), CoordVector2D(0.1, 0.1), CoordVector2D(100, 100))
      val cov = UncorrelatedKernelND(GaussianKernel2D(10), 2)
      val gp = GaussianProcess[CoordVector2D](domain, _ => DenseVector(0., 0.), cov)
      
      val trainingData = IndexedSeq(((-3., -3.), (1., 1.)), ((-1., 3.), (0., -1.))).map(t => (CoordVector2D(t._1(0), t._1(1)), DenseVector(t._2(0), t._2(1)))) 
      val posteriorGP = GaussianProcess.regression(gp, trainingData, 0)
      
      for ((x,y) <- trainingData) { 
    	  (posteriorGP.mean(x)(0) should be(y(0) plusOrMinus 0.0001))
    	  (posteriorGP.mean(x)(1) should be(y(1) plusOrMinus 0.0001))
      }
    }

  it("makes faces") {
      import java.io.File
         val testMeshURL = getClass().getResource("/facemesh.h5")

         val mesh = MeshIO.readHDF5(new File(testMeshURL.getPath)).get
         val vtkpd = Utils.meshToVTKMesh(mesh)
         Utils.ShowVTK(vtkpd)
  }
  
}
