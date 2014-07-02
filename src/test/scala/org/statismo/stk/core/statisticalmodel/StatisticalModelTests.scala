package org.statismo.stk.core.statisticalmodel


import scala.language.implicitConversions
import org.statismo.stk.core.io.{StatismoIO, MeshIO}
import org.statismo.stk.core.kernels._
import org.statismo.stk.core.geometry._
import breeze.linalg.{ DenseVector, DenseMatrix }
import java.io.File
import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import org.statismo.stk.core.numerics.UniformSampler3D
import org.statismo.stk.core.registration.RigidTransformationSpace3D

class StatisticalModelTests extends FunSpec with ShouldMatchers {

  implicit def doubleToFloat(d: Double) = d.toFloat

  describe("A statistical model") {
    it("yields the right mean and deformations when created from a discretized gp") {
      org.statismo.stk.core.initialize()
      val path = getClass().getResource("/facemesh.h5").getPath
      val mesh = MeshIO.readHDF5(new File(path)).get
      val cov = UncorrelatedKernel3x3(GaussianKernel3D(100) * 100)

      val meshPoints = mesh.points
      val region = mesh.boundingBox
      val gpConfiguration = LowRankGaussianProcessConfiguration[ThreeD](
        region,
        UniformSampler3D(region, 7 * 7 * 7),
        (x: Point[ThreeD]) => Vector(0.0, 0.0, 0.0),
        cov,
        20)
      val gp =LowRankGaussianProcess.createLowRankGaussianProcess3D(gpConfiguration)
      val (lambdas, phis) = gp.eigenPairs.unzip
      val specializedGP = gp.specializeForPoints(mesh.points.toIndexedSeq) // for convenience, to get mean and PCA components already discretized

      var mVec = DenseVector.zeros[Float](mesh.numberOfPoints * 3)
      var U = DenseMatrix.zeros[Float](mesh.numberOfPoints * 3, phis.size)
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
      val statMeshModel = StatisticalMeshModel(mesh, mVec, DenseVector[Float](lambdas.toArray), U)

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

    it ("yields the same mean and deformations when transformed with a rigid transformation") {
      org.statismo.stk.core.initialize()
      val path = getClass().getResource("/facemodel.h5").getPath
      val model = StatismoIO.readStatismoMeshModel(new File(path)).get
      val rigidTransform = RigidTransformationSpace3D()


    }
  }
}