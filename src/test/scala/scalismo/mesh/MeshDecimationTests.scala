package scalismo.mesh

import java.io.File
import java.net.URLDecoder

import scalismo.ScalismoTestSuite
import scalismo.io.MeshIO

class MeshDecimationTests extends ScalismoTestSuite {
  describe("A decimated mesh") {

    val path = getClass.getResource("/facemesh.stl").getPath
    val facemesh = MeshIO.readMesh(new File(URLDecoder.decode(path, "UTF-8"))).get
    val pathDecimated = getClass.getResource("/facemesh_decimated.ply").getPath
    val facemeshDecimated = MeshIO.readMesh(new File(URLDecoder.decode(pathDecimated, "UTF-8"))).get
    val t0 = System.currentTimeMillis()
    val reducedMesh = facemesh.operations.decimate(facemesh.pointSet.numberOfPoints / 3)
    val t1 = System.currentTimeMillis()
    val _ = facemesh.operations.decimateOld(facemesh.pointSet.numberOfPoints / 3)
    val t2 = System.currentTimeMillis()
    println(s"Decimation time (new): ${(t1 - t0) / 1000.0}")
    println(s"Decimation time (old): ${(t2 - t1) / 1000.0}")

    it("has a reduced number of points") {
      val reductionRatio = reducedMesh.pointSet.numberOfPoints / facemesh.pointSet.numberOfPoints.toDouble
      reductionRatio should be(0.3 +- 0.1)
    }

    it("has approximately preserves the surface") {
      MeshMetrics.hausdorffDistance(reducedMesh, facemesh) < 1.0
    }

    it("that is equal to another decimated mesh") {
      reducedMesh.triangulation.triangles.toSet should equal(facemeshDecimated.triangulation.triangles.toSet)
      reducedMesh.pointSet.numberOfPoints should equal(facemeshDecimated.pointSet.numberOfPoints)
    }
  }
}
