package smptk
package statisticalmodel

import smptk.io.MeshIO
import smptk.kernels._
import smptk.image.Geometry._
import breeze.linalg.{ DenseVector, DenseMatrix }
import java.io.File
import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers

class StatisticalModelTests extends FunSpec with ShouldMatchers {

  describe("A statistical model") {
    it("yields the right mean and deformations when created from a discretized gp") {

      val path = getClass().getResource("/facemesh.h5").getPath
      val mesh = MeshIO.readHDF5(new File(path)).get
      val cov = UncorrelatedKernelND(GaussianKernel3D(100, 100), 3)

      val meshPoints = mesh.domain.points
      val region = mesh.boundingBox
      val gpConfiguration = LowRankGaussianProcessConfiguration[CoordVector3D](
        region,
        (x: CoordVector3D[Double]) => DenseVector(0., 0., 0.),
        cov,
        20,
        300)
      val gp = GaussianProcess.createLowRankGaussianProcess3D(gpConfiguration)
      val (lambdas, phis) = gp.eigenPairs.unzip
      val discreteGp = gp.discretize(mesh.domain.points.toIndexedSeq) // for convenience, to get mean and PCA components already discretized
      val statMeshModel = StatisticalMeshModel(mesh, discreteGp.m, DenseVector(lambdas.toArray), discreteGp.U)

      val newGP = statMeshModel.gp
      val (newLambdas, newPhis) = newGP.eigenPairs.unzip
      lambdas should equal(newLambdas)

      // evaluating the newGP at the points of the mesh should yield the same deformations as the original gp
      println("starting the comparison")
      for (pt <- mesh.domain.points.par) {
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