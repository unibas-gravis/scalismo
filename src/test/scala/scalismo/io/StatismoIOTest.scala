package scalismo.io

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import java.io.File
import scalismo.io.StatismoIO.writeStatismoMeshModel
import breeze.linalg.DenseVector
import scalismo.statisticalmodel.StatisticalMeshModel

class StatismoIOTest extends FunSpec with ShouldMatchers {

  scalismo.initialize()

  describe("a Statismo Mesh Model") {

    def assertModelAlmostEqual(model1 : StatisticalMeshModel, model2 : StatisticalMeshModel) : Unit = {
      assert(model1.mean == model2.mean)
      assert(breeze.linalg.norm(model1.gp.variance -model2.gp.variance) < 1e-5)
      assert(breeze.linalg.sum(model1.gp.basisMatrix - model2.gp.basisMatrix) < 1e-5)
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
        assertModelAlmostEqual(model, readModel)
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
        assertModelAlmostEqual(model, readModel)
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
        assertModelAlmostEqual(model, readModel)
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