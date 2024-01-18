package scalismo.io.ply

import scalismo.color.RGBA
import scalismo.common.PointId
import scalismo.geometry.Point3D
import scalismo.mesh.{TriangleCell, TriangleList, TriangleMesh3D}

import java.io.IOException
import java.nio.{ByteBuffer, ByteOrder}
import scala.util.Try

object PLYMeshReaderBinary {
  def readPoints(buffer: ByteBuffer,
                 vertexInfo: PLYElement,
                 byteOrder: ByteOrder
  ): (IndexedSeq[Point3D], Option[IndexedSeq[RGBA]]) = {
    val pIndex = PLYHelpers.getVertexPropertyIndex(vertexInfo.properties)
    val data: IndexedSeq[(Point3D, Option[RGBA])] = (0 until vertexInfo.count).map { _ =>
      val data = vertexInfo.properties.map { item =>
        item.read(buffer, byteOrder)
      }
      val p = Point3D(
        data(pIndex.x).head.asInstanceOf[Float],
        data(pIndex.y).head.asInstanceOf[Float],
        data(pIndex.z).head.asInstanceOf[Float]
      )
      val c = if (pIndex.isRGB) {
        Option(
          RGBA(
            data(pIndex.red.get).head.asInstanceOf[Int].toDouble / 255.0,
            data(pIndex.green.get).head.asInstanceOf[Int].toDouble / 255.0,
            data(pIndex.blue.get).head.asInstanceOf[Int].toDouble / 255.0,
            1.0
          )
        )
      } else if (pIndex.isRGBA) {
        Option(
          RGBA(
            data(pIndex.red.get).head.asInstanceOf[Int].toDouble / 255.0,
            data(pIndex.green.get).head.asInstanceOf[Int].toDouble / 255.0,
            data(pIndex.blue.get).head.asInstanceOf[Int].toDouble / 255.0,
            data(pIndex.alpha.get).head.asInstanceOf[Int].toDouble / 255.0
          )
        )
      } else {
        None
      }
      (p, c)
    }
    val (points, colors) = data.unzip
    val flattenedColors: Option[IndexedSeq[RGBA]] = if (colors.exists(_.isEmpty)) None else Some(colors.flatten)
    (points, flattenedColors)
  }

  def readTriangles(buffer: ByteBuffer, faceInfo: PLYElement, byteOrder: ByteOrder): IndexedSeq[TriangleCell] = {
    if (faceInfo.count == 0) {
      IndexedSeq.empty
    } else {
      val vertexIndex = PLYHelpers
        .getFacePropertyIndex(faceInfo.properties)
        .vertexIndex
        .getOrElse(
          throw new IOException(
            "Faces property vertex_index not found."
          )
        )
      (0 until faceInfo.count).map { _ =>
        val data = faceInfo.properties.map { item =>
          item.read(buffer, byteOrder)
        }
        val vertexData = data(vertexIndex)
        if (vertexData.length != 3) {
          throw new IOException(
            "Faces elements different than 3 specified."
          )
        }
        TriangleCell(PointId(vertexData(0).asInstanceOf[Int]),
                     PointId(vertexData(1).asInstanceOf[Int]),
                     PointId(vertexData(2).asInstanceOf[Int])
        )
      }
    }
  }
}
