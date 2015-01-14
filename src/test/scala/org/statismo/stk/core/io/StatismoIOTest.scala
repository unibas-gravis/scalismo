package org.statismo.stk.core.io

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import java.io.File
import org.statismo.stk.core.statisticalmodel.StatisticalMeshModel

import org.statismo.stk.core.mesh.{ TriangleMesh }
import org.statismo.stk.core.io.StatismoIO._
import breeze.linalg.DenseVector

class StatismoIOTest extends FunSpec with ShouldMatchers {

  org.statismo.stk.core.initialize()

  describe("a Statismo Mesh Model") {

    def assertModelEqual(model1 : StatisticalMeshModel, model2 : StatisticalMeshModel) : Unit = {
      val coeffs = DenseVector.ones[Float](model1.gp.rank)
      val def1 = model1.gp.instance(coeffs)
      val def2 = model2.gp.instance(coeffs)

      model1.mesh.points.zip(model2.mesh.points).map {
        pair =>
        {
          def1(pair._1)(0) should be(def2(pair._2)(0) plusOrMinus 0.01f)
          def1(pair._1)(1) should be(def2(pair._2)(1) plusOrMinus 0.01f)
          def1(pair._1)(2) should be(def2(pair._2)(2) plusOrMinus 0.01f)
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
        assertModelEqual(model, readModel)
      }
      t.get

    }



    it("can be written and read again in non-standard location") {
      val statismoFile = new File(getClass().getResource("/facemodel.h5").getPath())
      val dummyFile = File.createTempFile("dummy", "h5")
      dummyFile.deleteOnExit

      val t = for {
        model <- StatismoIO.readStatismoMeshModel(statismoFile)
        _ <- writeStatismoMeshModel(model, dummyFile, "/someLocation")
        readModel <- StatismoIO.readStatismoMeshModel(dummyFile, "/someLocation")
      } yield {
        assertModelEqual(model, readModel)
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

        _ <- writeStatismoMeshModel(model, dummyFile, statismoVersion = v081)
        readModel <- StatismoIO.readStatismoMeshModel(dummyFile)
      } yield {
        assertModelEqual(model, readModel)
      }
      t.get

    }
  }

  it("can read a catalog") {
    val statismoFile = new File(getClass().getResource("/facemodel.h5").getPath())
    val catalog = StatismoIO.readModelCatalog(statismoFile).get
    catalog.size should equal(1)
    val firstEntry = catalog.head
    firstEntry.name should equal("faceshapemodel")
    firstEntry.modelType should equal(StatismoIO.StatismoModelType.Polygon_Mesh)
    firstEntry.modelPath should equal("/")
  }

}