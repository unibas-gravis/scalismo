package scalismo.mesh

import java.io.File

import scalismo.ScalismoTestSuite
import scalismo.io.MeshIO

class MeshDecimationTests extends ScalismoTestSuite {
  describe("A decimated mesh") {

    val path = getClass.getResource("/facemesh.stl").getPath
    val facemesh = MeshIO.readMesh(new File(path)).get

    it("has a reduced number of points") {
      val reducedMesh = facemesh.operations.decimate(facemesh.pointSet.numberOfPoints / 2)
      reducedMesh.pointSet.numberOfPoints should be < (facemesh.pointSet.numberOfPoints)
    }

    it("has approximately preserves the surface") {
      val reducedMesh = facemesh.operations.decimate(facemesh.pointSet.numberOfPoints / 2)
      MeshMetrics.hausdorffDistance(reducedMesh, facemesh) < 1.0
    }
  }
}
