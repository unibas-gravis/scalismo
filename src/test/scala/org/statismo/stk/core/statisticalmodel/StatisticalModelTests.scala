package org.statismo.stk.core.statisticalmodel

import scala.language.implicitConversions
import org.statismo.stk.core.io.{ StatismoIO, MeshIO }
import org.statismo.stk.core.kernels._
import org.statismo.stk.core.geometry._
import breeze.linalg.{ DenseVector, DenseMatrix }
import java.io.File
import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import org.statismo.stk.core.numerics.UniformSampler3D
import org.statismo.stk.core.registration.RigidTransformationSpace3D
import org.statismo.stk.core.registration.RigidTransformation3D
import org.statismo.stk.core.mesh.TriangleMesh

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
      val gpConfiguration = LowRankGaussianProcessConfiguration[_3D](
        region,
        UniformSampler3D(region, 7 * 7 * 7),
        (x: Point[_3D]) => Vector(0.0, 0.0, 0.0),
        cov,
        20)
      val gp = LowRankGaussianProcess.createLowRankGaussianProcess3D(gpConfiguration)
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
      for (pt <- mesh.pointSeq.par) {
        for (d <- 0 until 3) { gp.mean(pt)(d) should be(newGP.mean(pt)(d) plusOrMinus 1e-5) }

        for (i <- 0 until newLambdas.size) {
          val phi_iAtPoint = phis(i)(pt)
          val newPhi_iAtPoint = newPhis(i)(pt)
          for (d <- 0 until 3) { phi_iAtPoint(d) should equal(newPhi_iAtPoint(d)) }
        }

      }

    }

    it("can be transformed forth and back and yield the same deformations") {
      val path = getClass().getResource("/facemodel.h5").getPath
      val model = StatismoIO.readStatismoMeshModel(new File(path)).get

      val parameterVector = DenseVector[Float](1.5, 1.0, 3.5, Math.PI, -Math.PI / 2.0, -Math.PI)
      val rigidTransform = RigidTransformationSpace3D().transformForParameters(parameterVector)
      val inverseTransform = rigidTransform.inverse.asInstanceOf[RigidTransformation3D]

      val newModel = StatisticalMeshModel.transform(StatisticalMeshModel.transform(model, rigidTransform), inverseTransform)
      val randParams = DenseVector.rand(model.gp.eigenPairs.size).map(_.toFloat) * 0.01f
      val instance1 = model.instance(randParams)
      val instance2 = newModel.instance(randParams)

      val diffs = instance1.points.zip(instance2.points).map { case (p1, p2) => (p1 - p2).norm }
      assert(diffs.max < 0.05f)
    }

    org.statismo.stk.core.initialize
    val path = getClass().getResource("/facemodel.h5").getPath
    val model = StatismoIO.readStatismoMeshModel(new File(path)).get
    val newMesh = model.instance(DenseVector.rand(model.gp.eigenPairs.size).map(_.toFloat) * 0.01f)
    val newModel = StatisticalMeshModel.changeMesh(model, newMesh)
    def compareModels(oldModel: StatisticalMeshModel, newModel: StatisticalMeshModel) {
      val oldMean = oldModel.mean
      val newMean = newModel.mean
      newMean.points.zip(oldMean.points).foreach { case (p1, p2) => assert((p1 - p2).norm < 0.001f) }
      val instanceCoeffs = DenseVector.rand(oldModel.gp.eigenPairs.size).map(_.toFloat) * 0.01f
      val instanceOldModel = oldModel.instance(instanceCoeffs)
      val instanceNewModel = newModel.instance(instanceCoeffs)
      instanceNewModel.points.toIndexedSeq.zip(instanceOldModel.points.toIndexedSeq).foreach { case (p1, p2) => assert((p1 - p2).norm < 0.001f) }
    }

    it("can change the mean shape and still yield the same shape space") {
      compareModels(model, newModel)
    }

    it("can write a changed mean statistical mode, read it and still yield the same space") {
      val tmpStatismoFile = File.createTempFile("statModel", ".h5")
      tmpStatismoFile.deleteOnExit()
      StatismoIO.writeStatismoMeshModel(newModel, tmpStatismoFile)
      val readModel = StatismoIO.readStatismoMeshModel(tmpStatismoFile).get
      compareModels(model, readModel)
    }

  }
}