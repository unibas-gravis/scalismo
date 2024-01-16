package scalismo.mesh

import java.io.File
import java.net.URLDecoder

import scalismo.ScalismoTestSuite
import scalismo.io.MeshIO

class MeshDecimationTests extends ScalismoTestSuite {
  describe("A decimated mesh") {

    val path = getClass.getResource("/facemesh.stl").getPath
    println(path)
    val facemesh = MeshIO.readMesh(new File(URLDecoder.decode(path, "UTF-8"))).get
    val pathDec = "src/test/resources/facemesh_dec.stl"
    val pathDecCustom = "src/test/resources/facemesh_dec_custom.stl"

//    it("has a reduced number of points") {
//      val reducedMesh = facemesh.operations.decimate(facemesh.pointSet.numberOfPoints / 3)
//      val reductionRatio = reducedMesh.pointSet.numberOfPoints / facemesh.pointSet.numberOfPoints.toDouble
//      reductionRatio should be(0.3 +- 0.1)
//    }

    it("has approximately preserves the surface") {
      val startTime = System.nanoTime()
      val reducedMesh = facemesh.operations.decimate(facemesh.pointSet.numberOfPoints / 4)
      val midTime = System.nanoTime()
      val reducedMeshCustom = facemesh.operations.decimateFaces(facemesh.triangulation.triangles.length / 4)
      val endTime = System.nanoTime()
      val elapsedTimeSeconds0 = (midTime - startTime) / 1e9
      val elapsedTimeSeconds1 = (endTime - midTime) / 1e9
      println(s"Elapsed Time: $elapsedTimeSeconds0 seconds")
      println(s"Elapsed Time: $elapsedTimeSeconds1 seconds")
      MeshIO.writeMesh(reducedMesh, new File(pathDec))
      MeshIO.writeMesh(reducedMeshCustom, new File(pathDecCustom))

      MeshMetrics.hausdorffDistance(reducedMesh, facemesh) < 1.0
    }
  }
}
