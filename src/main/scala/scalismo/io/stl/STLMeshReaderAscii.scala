package scalismo.io.stl

import scalismo.geometry.{EuclideanVector3D, Point3D}
import scalismo.mesh.TriangleMesh3D

import java.io.{BufferedReader, FileReader, IOException}
import scala.collection.mutable.ArrayBuffer
import scala.util.Try

object STLMeshReaderAscii {
  def read(file: String): Try[TriangleMesh3D] = Try {
    val breader = new BufferedReader(new FileReader(file))
    val header = breader.readLine()

    val triangles = ArrayBuffer.empty[STLTriangle]
    var line: String = null
    while ( {
      line = breader.readLine(); line != null && !line.trim.startsWith("endsolid")
    }) {
      line = line.trim.replaceAll(" +", " ")
      val triangleStrings: Array[String] = Array(line) ++
        (0 until 6).map(_ => breader.readLine().trim.replaceAll(" +", " ")).toArray
      val triangle = parseTriangleStrings(triangleStrings)
      triangles += triangle.get
    }
    breader.close()

    STLHelpers.STLTrianglesToTriangleMesh(triangles.toSeq)
  }

  private def parseNormalString(data: String): EuclideanVector3D ={
    val parts = data.split(" ").filter(f => f != " ")
    if(parts.length != 5){
      throw new IOException("Wrong faces normal format.")
    }
    else{
      EuclideanVector3D(parts(2).toFloat, parts(3).toFloat, parts(4).toFloat)
    }
  }

  private def parseVertexString(data: String): Point3D = {
    val parts = data.split(" ").filter(f => f != " ")
    if (parts.length != 4) {
      throw new IOException("Wrong faces vertex format.")
    }
    else {
      Point3D(parts(1).toFloat, parts(2).toFloat, parts(3).toFloat)
    }
  }

  private def parseTriangleStrings(data: Seq[String]): Try[STLTriangle] = Try{
    if (data.length != 7 ||
      !data(0).startsWith("facet normal") ||
      !data(1).startsWith("outer loop") ||
      !data(2).startsWith("vertex") ||
      !data(3).startsWith("vertex") ||
      !data(4).startsWith("vertex") ||
      !data(5).startsWith("endloop") ||
      !data(6).startsWith("endfacet")) {
      throw new IOException("Wrong faces description format.")
    }
    val n = parseNormalString(data(0))
    val p1 = parseVertexString(data(2))
    val p2 = parseVertexString(data(3))
    val p3 = parseVertexString(data(4))
    STLTriangle(n, p1, p2, p3)
  }
}
