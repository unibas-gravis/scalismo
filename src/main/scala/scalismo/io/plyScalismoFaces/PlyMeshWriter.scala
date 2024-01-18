/*
 * Copyright University of Basel, Graphics and Vision Research Group
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */
package scalismo.io.plyScalismoFaces

import java.io.{File, FileOutputStream, OutputStream, OutputStreamWriter}
import java.nio.ByteOrder

import scalismo.color.RGBA
import scalismo.io.plyScalismoFaces.PlyHelpers.PlyFormat._
import scalismo.io.plyScalismoFaces.PlyHelpers.PlyHeader._
import scalismo.io.plyScalismoFaces.PlyHelpers._
import scalismo.io.plyScalismoFaces.PlyMeshPropertyWriters._
import scalismo.geometry._
import scalismo.mesh._

/**
 * Writes a ply file based on the passed arguments. The arguments are first analyzed and the nescessary writer-chains
 * are built for the vertex and face properties. The writer are in PlyMeshPropertyWriters. The writers make use of the
 * mesh-independant List- and SequenceWriters.
 *
 * @param url
 * @param vertices
 * @param faces
 * @param color
 * @param normals
 * @param plyFormat
 * @param headerFormat
 */
private[io] case class PlyMeshWriter(url: String,
                                     vertices: Option[IndexedSeq[Point[_3D]]] = None,
                                     faces: Option[IndexedSeq[IntVector[_3D]]] = None,
                                     color: Option[MeshSurfaceProperty[RGBA]] = None,
                                     normals: Option[MeshSurfaceProperty[EuclideanVector[_3D]]] = None,
                                     plyFormat: PlyFormat = PlyFormat.ASCII,
                                     headerFormat: PlyHeader = PlyHeader.meshlab
) {

  val nVertices: Int = vertices.map(_.length).getOrElse(-1)
  val nFaces: Int = faces.map(_.length).getOrElse(-1)

  val vertexProperties: IndexedSeq[IndexedProperty] = getVertexProperties
  val faceProperties: IndexedSeq[IndexedProperty] = getFaceProperties

  def writeToStream(os: OutputStream): Unit = {
    val osw = new OutputStreamWriter(os)
    writeHeader(osw)
    osw.flush()
    writeData(os)
    osw.flush()
  }

  def writeToFile(file: File): Unit = {
    writeToStream(new FileOutputStream(file))
  }

  def write(filename: String): Unit = {
    val plyFile = new File(filename)
    writeToFile(plyFile)
  }

  private def getVertexProperties: IndexedSeq[IndexedProperty] = {

    val _vertices = vertices.map(new Vertex(_))

    val _vColors = color.flatMap {
      case c: SurfacePointProperty[RGBA] => Some(new VertexColor(c))
      case _                             => None
    }

    val _vNormals = normals.flatMap {
      case n: SurfacePointProperty[EuclideanVector[_3D]] => Some(new VertexNormal(n))
      case n: MappedSurfaceProperty[_, _] => {
        // fully evaluate the lazy mapped surface property
        val surfaceProp = SurfacePointProperty.averagedPointProperty(n)
        Some(new VertexNormal(surfaceProp))
      }
      case _ => None
    }

    IndexedSeq(
      _vertices,
      _vNormals,
      _vColors
    ).flatten
  }

  private def getFaceProperties: IndexedSeq[IndexedProperty] = {
    val _faces = faces.map(new Faces(_))

    IndexedSeq(
      _faces
    ).flatten
  }

  private def writeHeader(osw: OutputStreamWriter): Unit = {
    osw.write("ply\n")
    osw.write("format %s 1.0\n".format(plyFormat))
    osw.write("comment Created by GraVis-Faces\n")

    if (nVertices > 0) {
      writeVertexHeader(osw)
      osw.flush()
    }

    if (nFaces > 0) {
      writeFacesHeader(osw)
      osw.flush()
    }

    osw.write("end_header\n")
    osw.flush()
  }

  private def writeVertexHeader(osw: OutputStreamWriter): Unit = {
    osw.write(PLY.element + " " + PLY.vertex + " %d\n".format(nVertices))
    vertexProperties.foreach { vp =>
      vp.writeHeader(osw)
    }
    osw.flush()
  }

  private def writeFacesHeader(osw: OutputStreamWriter): Unit = {
    osw.write(PLY.element + " " + PLY.face + " %d\n".format(nFaces))
    faceProperties.foreach { fp =>
      fp.writeHeader(osw)
    }
    osw.flush()
  }

  private def writeData(os: OutputStream): Unit = {
    if (nVertices > 0) {
      writeVertexData(os)
      os.flush()
    }
    if (nFaces > 0) {
      writeFaceData(os)
      os.flush()
    }
  }

  private def writeVertexData(os: OutputStream): Unit = {
    plyFormat match {
      case PlyFormat.ASCII =>
        val osw = new OutputStreamWriter(os)
        (0 until nVertices).foreach { vIdx =>
          vertexProperties.head.write(vIdx, osw)
          vertexProperties.tail.foreach { vp =>
            osw.write(" ")
            vp.write(vIdx, osw)
          }
          osw.write("\n")
        }
        osw.flush()
      case PlyFormat.BinaryLittleEndian =>
        (0 until nVertices).foreach { vIdx =>
          vertexProperties.foreach(_.write(vIdx, os, ByteOrder.LITTLE_ENDIAN))
          os.flush()
        }
      case PlyFormat.BinaryBigEndian =>
        (0 until nVertices).foreach { vIdx =>
          vertexProperties.foreach(_.write(vIdx, os, ByteOrder.BIG_ENDIAN))
          os.flush()
        }
    }
  }

  private def writeFaceData(os: OutputStream): Unit = {
    plyFormat match {
      case PlyFormat.ASCII =>
        val osw = new OutputStreamWriter(os)
        (0 until nFaces).foreach { fIdx =>
          faceProperties.head.write(fIdx, osw)
          faceProperties.tail.foreach { fp =>
            osw.write(" ")
            fp.write(fIdx, osw)
          }
          osw.write("\n")
        }
        osw.flush()
      case PlyFormat.BinaryLittleEndian =>
        (0 until nFaces).foreach { vIdx =>
          faceProperties.foreach(_.write(vIdx, os, ByteOrder.LITTLE_ENDIAN))
          os.flush()
        }
      case PlyFormat.BinaryBigEndian =>
        (0 until nFaces).foreach { vIdx =>
          faceProperties.foreach(_.write(vIdx, os, ByteOrder.BIG_ENDIAN))
          os.flush()
        }
    }
  }

}
