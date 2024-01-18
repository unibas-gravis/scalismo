package scalismo.io.ply

import java.io.IOException
import scala.collection.mutable.ArrayBuffer
import scala.util.{Failure, Success, Try}

object PLYHeader {

  def createHeader(numVertices: Int, numFaces: Int, vertexColors: Boolean): String = {
    val header = new StringBuilder
    header.append("ply\nformat binary_little_endian 1.0\ncomment Scalismo generated PLY File\n")
    header.append(f"element vertex $numVertices\nproperty float x\nproperty float y\nproperty float z\n")
    if (vertexColors) {
      header.append("property uchar red\nproperty uchar green\nproperty uchar blue\nproperty uchar alpha\n")
    }
    header.append(f"element face $numFaces\nproperty list uchar int vertex_indices\nend_header\n")
    header.toString()
  }

  def parseHeader(header: Array[String]): HeaderInfo = {
    val fileComments: ArrayBuffer[String] = ArrayBuffer.empty[String]
    var fileFormat: Option[PLYFormat] = None
    val fileElements: ArrayBuffer[PLYElement] = ArrayBuffer.empty[PLYElement]

    var cnt = 0
    while (cnt < header.length) {
      val line = header(cnt)
      val lineSplit = line.split(" ")
      val name = lineSplit.head
      if (name == "comment" || name == "obj_info") {
        fileComments += lineSplit.drop(1).mkString(" ")
      } else if (lineSplit.length > 2) {
        if (name == "format") {
          lineSplit(1) match {
            case PLY_FORMAT_ASCII =>
              fileFormat = Option(PLYFormat(PLYTypeFormat.ASCII, lineSplit(2)))
            case PLY_FORMAT_BIG =>
              fileFormat = Option(PLYFormat(PLYTypeFormat.BINARY_BIG_ENDIAN, lineSplit(2)))
            case PLY_FORMAT_LITTLE =>
              fileFormat = Option(PLYFormat(PLYTypeFormat.BINARY_LITTLE_ENDIAN, lineSplit(2)))
            case _ => Failure(new IOException(s"Unsupported PLY format"))
          }
        }
        if (name == "element") {
          val elementFormat = lineSplit(1) match {
            case PLY_ELEMENT_VERTEX =>
              Success(PLYElementFormat.VERTEX)
            case PLY_ELEMENT_FACE =>
              Success(PLYElementFormat.FACE)
            case _ => Failure(new IOException(s"Unsupported property format"))
          }
          val elementCount = lineSplit(2).toInt
          val elementProperties: ArrayBuffer[PLYProperty] = ArrayBuffer.empty[PLYProperty]
          while (cnt < header.length - 1 && (header(cnt + 1).split(" ").head) == "property") {
            val nextLine = header(cnt + 1).split(" ")
            if (nextLine(1) == "list") {
              elementProperties += PLYProperty(nextLine(2), nextLine(4), Option(nextLine(3)))
            } else {
              elementProperties += PLYProperty(nextLine(1), nextLine(2))
            }
            cnt += 1
          }
          fileElements += PLYElement(elementFormat.get, elementCount, elementProperties.toArray)
        }
      }
      cnt += 1
    }

    val vertexInfo = fileElements.find(_.format == PLYElementFormat.VERTEX).get
    val faceInfo = fileElements.find(_.format == PLYElementFormat.FACE).get
    val faceDefined = validateElementFace(faceInfo)
    val vertexDefined = validateElementVertex(vertexInfo)
    if (!vertexDefined.status) {
      throw new IOException(
        "Unsupported element property provided"
      )
    } else if (!vertexDefined.is3DVertex) {
      throw new IOException(
        "Vertex (x,y,z) not defined in file."
      )
    } else if (!faceDefined) {
      throw new IOException(
        "Face element defined but no property defined."
      )
    }
    HeaderInfo(
      format = fileFormat.get,
      vertexInfo = vertexInfo,
      faceInfo = faceInfo,
      comments = fileComments.toArray,
      headerLength = header.map(_.getBytes("UTF-8").length).sum + header.length
    )
  }

  private def validateElementFace(element: PLYElement): Boolean = {
    if (element.count == 0) true
    else {
      element.properties.nonEmpty && element.properties.exists(prop => VERTEX_INDEX_PROPERTY_NAMES.contains(prop.name))
    }
  }

  private def validateElementVertex(element: PLYElement): PLYItemsDefined = {
    val xyz = Seq("x", "y", "z")
    val n = Seq("nx", "ny", "nz")
    val color = Seq("red", "green", "blue")
    val colora = Seq("red", "green", "blue", "alpha")
    val st = Seq("s", "t")
    val uv = Seq("u", "v")
    val texture = Seq("texture_u", "texture_v")
    val all = xyz ++ n ++ color ++ colora ++ st ++ uv ++ texture
    val names = element.properties.map(_.name)
    val is3DVertexDefined = xyz.forall(names.contains)
    val is3DNormalDefined = n.forall(names.contains)
    val is3DVertexColorDefined = color.forall(names.contains) || colora.forall(names.contains)
    val is3DUVsDefined =
      st.forall(names.contains) ||
        uv.forall(names.contains) ||
        texture.forall(names.contains)
    val status = element.properties.map(_.name).forall(item => all.contains(item))
    PLYItemsDefined(status, is3DVertexDefined, is3DNormalDefined, is3DVertexColorDefined, is3DUVsDefined)
  }

}
