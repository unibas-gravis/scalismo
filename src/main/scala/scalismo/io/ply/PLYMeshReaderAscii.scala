package scalismo.io.ply

import scalismo.color.RGBA
import scalismo.common.PointId
import scalismo.geometry.Point3D
import scalismo.mesh.{TriangleCell, TriangleList, TriangleMesh3D}

import java.io.IOException
import scala.util.Try

object PLYMeshReaderAscii {

  def readPoints(lineIterator: Iterator[String],
                 vertexInfo: PLYElement
  ): (IndexedSeq[Point3D], Option[IndexedSeq[RGBA]]) = {
    val pIndex = PLYHelpers.getVertexPropertyIndex(vertexInfo.properties)
    val data: IndexedSeq[(Point3D, Option[RGBA])] = (0 until vertexInfo.count).map { _ =>
      val line = lineIterator.next()
      val split = line.split(" ")
      val p = Point3D(
        split(pIndex.x).toFloat,
        split(pIndex.y).toFloat,
        split(pIndex.z).toFloat
      )
      val c = if (pIndex.isRGB) {
        Option(
          RGBA(
            split(pIndex.red.get).toDouble / 255.0,
            split(pIndex.green.get).toDouble / 255.0,
            split(pIndex.blue.get).toDouble / 255.0,
            1.0
          )
        )
      } else if (pIndex.isRGBA) {
        Option(
          RGBA(
            split(pIndex.red.get).toDouble / 255.0,
            split(pIndex.green.get).toDouble / 255.0,
            split(pIndex.blue.get).toDouble / 255.0,
            split(pIndex.alpha.get).toDouble / 255.0
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

  def readTriangles(lineIterator: Iterator[String], faceInfo: PLYElement): IndexedSeq[TriangleCell] = {
    if (faceInfo.count == 0) {
      IndexedSeq.empty
    } else {
      // TODO: Update to support texture file
      faceInfo.properties.headOption
        .map { propertyList =>
          //          val listCounterFormat = propertyList.listFormat.get
          //          val listFormat = propertyList.format
          (0 until faceInfo.count).map { _ =>
            val line = lineIterator.next()
            val split = line.split(" ")
            val cnt = split(0).toInt
            if (cnt != 3) {
              throw new IOException(
                "Faces elements different than 3 specified."
              )
            }
            (1 to 3).map { i =>
              split(i).toInt
            }
          }
        }
        .getOrElse(IndexedSeq.empty)
        .map { case Seq(id1, id2, id3) =>
          TriangleCell(PointId(id1), PointId(id2), PointId(id3))
        }
    }
  }

}
