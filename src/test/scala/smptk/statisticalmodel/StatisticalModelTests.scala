package smptk
package statisticalmodel

import smptk.io.MeshIO
import smptk.kernels._
import geometry._
import geometry.implicits._
import breeze.linalg.{ DenseVector, DenseMatrix }
import java.io.File
import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import smptk.numerics.UniformSampler3D

class StatisticalModelTests extends FunSpec with ShouldMatchers {

  describe("A statistical model") {
    it("yields the right mean and deformations when created from a discretized gp") {

      val path = getClass().getResource("/facemesh.h5").getPath
      val mesh = MeshIO.readHDF5(new File(path)).get
      val cov = UncorrelatedKernelND(GaussianKernel3D(100) * 100, 3)

      val meshPoints = mesh.points
      val region = mesh.boundingBox
      val gpConfiguration = LowRankGaussianProcessConfiguration[ThreeD](
        region,
        UniformSampler3D(region),
        (x: Point[ThreeD]) => DenseVector(0., 0., 0.),
        cov,
        20,
        300)
      val gp = GaussianProcess.createLowRankGaussianProcess3D(gpConfiguration)
      val (lambdas, phis) = gp.eigenPairs.unzip
      val specializedGP = gp.specializeForPoints(mesh.points.toIndexedSeq) // for convenience, to get mean and PCA components already discretized
      
      var mVec = DenseVector.zeros[Double](mesh.numberOfPoints * 3)
      var U = DenseMatrix.zeros[Double](mesh.numberOfPoints * 3, phis.size)
      for ((pt, ptId) <- mesh.points.toIndexedSeq.par.zipWithIndex) {
        val mAtPt = gp.mean(pt)
        val phisAtPt = phis.map(phi => phi(pt))
        for (d <- 0 until 3) {
        	mVec(ptId * 3 + d) = mAtPt(d) 
        	for (i <- 0 until phis.size) { 
        	  U(ptId * 3 + d, i) = phisAtPt(i)(d)
        	}
        }
      }
      val statMeshModel = StatisticalMeshModel(mesh, mVec, DenseVector(lambdas.toArray), U)

      val newGP = statMeshModel.gp
      val (newLambdas, newPhis) = newGP.eigenPairs.unzip
      lambdas should equal(newLambdas)

      // evaluating the newGP at the points of the mesh should yield the same deformations as the original gp
      for (pt <- mesh.points.par) {
        for (d <- 0 until 3) { gp.mean(pt)(d) should be(newGP.mean(pt)(d) plusOrMinus 1e-5) }

        for (i <- 0 until newLambdas.size) {
          val phi_iAtPoint = phis(i)(pt)
          val newPhi_iAtPoint = newPhis(i)(pt)
          for (d <- 0 until 3) { phi_iAtPoint(d) should equal(newPhi_iAtPoint(d)) }
        }

      }

    }
  }
}