/*
 * Copyright 2015 University of Basel, Graphics and Vision Research Group
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package scalismo.io

import java.io.File

import scalismo.ScalismoTestSuite
import scalismo.io.StatismoIO.writeStatismoMeshModel
import scalismo.statisticalmodel.StatisticalMeshModel

class StatismoIOTest extends ScalismoTestSuite {

  describe("a Statismo Mesh Model") {

    def assertModelAlmostEqual(model1: StatisticalMeshModel, model2: StatisticalMeshModel): Unit = {
      assert(model1.mean == model2.mean)
      assert(breeze.linalg.norm(model1.gp.variance - model2.gp.variance) < 1e-5)
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