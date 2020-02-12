package scalismo.io.experimental

import java.io.File
import java.net.URLDecoder

import scalismo.ScalismoTestSuite
import scalismo.io.experimental
import scalismo.statisticalmodel.experimental.StatisticalVolumeMeshModel

class StatisticalVolumeMeshModelIOTest extends ScalismoTestSuite {

  describe("a Statismo Mesh volume Model") {

    def assertModelAlmostEqual(model1: StatisticalVolumeMeshModel, model2: StatisticalVolumeMeshModel): Unit = {
      assert(model1.mean == model2.mean)
      assert(breeze.linalg.norm(model1.gp.variance - model2.gp.variance) < 1e-5)
      assert(breeze.linalg.sum(model1.gp.basisMatrix - model2.gp.basisMatrix) < 1e-5)
    }

    it("can be written and read again") {
      val statismoFile = new File(URLDecoder.decode(getClass.getResource("/TetraMeshModel2.h5").getPath, "UTF-8"))
      val dummyFile = File.createTempFile("dummy", "h5")
      dummyFile.deleteOnExit()

      val t = for {
        model <- experimental.StatismoIO.readStatismoVolumeMeshModel(statismoFile)
        _ <- experimental.StatismoIO.writeStatismoVolumeMeshModel(model, dummyFile)
        readModel <- experimental.StatismoIO.readStatismoVolumeMeshModel(dummyFile)
      } yield {
        assertModelAlmostEqual(model, readModel)
      }
      t.get

    }

    it("can be written and read again in non-standard location") {
      val statismoFile = new File(URLDecoder.decode(getClass.getResource("/TetraMeshModel2.h5").getPath, "UTF-8"))
      val dummyFile = File.createTempFile("dummy", "h5")
      dummyFile.deleteOnExit()

      val t = for {
        model <- experimental.StatismoIO.readStatismoVolumeMeshModel(statismoFile)
        _ <- experimental.StatismoIO.writeStatismoVolumeMeshModel(model, dummyFile, "/someLocation")
        readModel <- experimental.StatismoIO.readStatismoVolumeMeshModel(dummyFile, "/someLocation")
      } yield {
        assertModelAlmostEqual(model, readModel)
      }
      t.get

    }

    it("can be written in version 0.81 and read again") {
      import scalismo.io.experimental.StatismoIO.StatismoVersion.v081

      val statismoFile = new File(URLDecoder.decode(getClass.getResource("/TetraMeshModel2.h5").getPath, "UTF-8"))
      val dummyFile = File.createTempFile("dummy", "h5")
      dummyFile.deleteOnExit()

      val t = for {
        model <- experimental.StatismoIO.readStatismoVolumeMeshModel(statismoFile)
        _ <- experimental.StatismoIO.writeStatismoVolumeMeshModel(model, dummyFile, statismoVersion = v081)
        readModel <- experimental.StatismoIO.readStatismoVolumeMeshModel(dummyFile)
      } yield {
        assertModelAlmostEqual(model, readModel)
      }
      t.get

    }
  }

}
