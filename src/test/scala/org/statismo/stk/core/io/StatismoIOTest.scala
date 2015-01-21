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
      assert(model1 == model2)
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