package smptk
package io

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import java.io.File
import scala.util.Success
import scala.util.Failure
import image.Geometry._
import smptk.image.Utils
import smptk.mesh.{TriangleMesh, TriangleMeshDomain}

class StatismoIOTest extends FunSpec with ShouldMatchers {

  describe("a Statismo Mesh MOdel") {
    it("can be read") {
      val statismoFile = new File("/tmp/facemodel.h5")
      val maybeModel = StatismoIO.readStatismoMeshModel(statismoFile)
      maybeModel match {
        case Success(model) => {
          val refMesh = model.mesh
          val meshPts = refMesh.domain.points

          val dgp = model.gp.discretize(meshPts.toIndexedSeq)
          
          for (i <- 0 until 10) {
            val sample = dgp.sample
            val newPoints = for ((pts, df) <- meshPts.zip(sample)) yield {
              CoordVector3D(df(0),  df(1), df(2))
            }
                  Utils.showVTK(Utils.meshToVTKMesh(TriangleMesh(TriangleMeshDomain(newPoints.toIndexedSeq, refMesh.domain.cells))))
          }

        }

        case Failure(e) => {
          println(e)
          e.printStackTrace()
          maybeModel.isSuccess should be(true)
        }
      }
    }
  }
}