package org.statismo.stk.core.io

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import java.io.File
import scala.util.Success
import scala.util.Failure
import org.statismo.stk.core.geometry._
import org.statismo.stk.core.image.Utils
import org.statismo.stk.core.mesh.{ TriangleMesh }
import org.statismo.stk.core.io.StatismoIO._
import breeze.linalg.DenseVector

class StatismoIOTest extends FunSpec with ShouldMatchers {

  org.statismo.stk.core.initialize()

  describe("a Statismo Mesh Model") {
    it("can be read") {
      // TODO add a model to the resource directory and change path
      val statismoFile = new File(getClass().getResource("/facemodel.h5").getPath())
      val maybeModel = StatismoIO.readStatismoMeshModel(statismoFile)
      maybeModel match {
        case Success(model) => {
          val refMesh = model.mesh
          val meshPts = refMesh.points
          // TODO fix this test
        }
        case Failure(e) => {
          println(e)
          e.printStackTrace()
          maybeModel.isSuccess should be(true)
        }
      }
    }

    it("can be written and read again") {
      val statismoFile = new File(getClass().getResource("/facemodel.h5").getPath())
      val dummyFile = File.createTempFile("dummy", "h5")
      dummyFile.deleteOnExit

      val t = for {
        model <- StatismoIO.readStatismoMeshModel(statismoFile)
        _ <- writeStatismoMeshModel(model, dummyFile)
        readModel <- StatismoIO.readStatismoMeshModel(dummyFile)
      } yield {
        val coeffs = DenseVector.ones[Float](model.gp.rank)
        val def1 = model.gp.instance(coeffs)
        val def2 = readModel.gp.instance(coeffs)

        model.mesh.points.zip(readModel.mesh.points).map {
          pair =>
            {
              def1(pair._1)(0) should be(def2(pair._2)(0) plusOrMinus 0.01f)
              def1(pair._1)(1) should be(def2(pair._2)(1) plusOrMinus 0.01f)
              def1(pair._1)(2) should be(def2(pair._2)(2) plusOrMinus 0.01f)
            }
        }
      }
      t.get

    }

    it("can be written in version 0.81 and read again") {
      import StatismoIO.StatismoVersion.v081
      
      val statismoFile = new File(getClass().getResource("/facemodel.h5").getPath())
      val dummyFile = File.createTempFile("dummy", "h5")
      dummyFile.deleteOnExit

      val t = for {
        model <- StatismoIO.readStatismoMeshModel(statismoFile)

        _ <- writeStatismoMeshModel(model, dummyFile, v081)
        readModel <- StatismoIO.readStatismoMeshModel(dummyFile)
      } yield {
        val coeffs = DenseVector.ones[Float](model.gp.rank)
        val def1 = model.gp.instance(coeffs)
        val def2 = readModel.gp.instance(coeffs)

        model.mesh.points.zip(readModel.mesh.points).map {
          pair =>
            {
              def1(pair._1)(0) should be(def2(pair._2)(0) plusOrMinus 0.01f)
              def1(pair._1)(1) should be(def2(pair._2)(1) plusOrMinus 0.01f)
              def1(pair._1)(2) should be(def2(pair._2)(2) plusOrMinus 0.01f)
            }
        }
      }
      t.get

    }

  }
}