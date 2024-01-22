package scalismo.mesh

import scalismo.ScalismoTestSuite
import scalismo.io.MeshIO

import java.io.File
import java.net.URLDecoder

class MeshDecimationTests extends ScalismoTestSuite {
  describe("A decimated mesh") {

    val path = getClass.getResource("/facemesh.stl").getPath
    val facemesh = MeshIO.readMesh(new File(URLDecoder.decode(path, "UTF-8"))).get

    it("has a reduced number of points") {
      val reducedMesh = facemesh.operations.decimate(facemesh.pointSet.numberOfPoints / 3)
      val reductionRatio = reducedMesh.pointSet.numberOfPoints / facemesh.pointSet.numberOfPoints.toDouble
      reductionRatio should be(0.3 +- 0.1)
    }

    it("has approximately preserves the surface") {
      val reducedMesh = facemesh.operations.decimate(facemesh.pointSet.numberOfPoints / 4)
      MeshMetrics.hausdorffDistance(reducedMesh, facemesh) < 1.0
    }
  }
}
