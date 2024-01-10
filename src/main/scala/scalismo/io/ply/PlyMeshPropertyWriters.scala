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
package scalismo.io.ply

import java.io.{OutputStream, OutputStreamWriter}
import java.nio.ByteOrder

import scalismo.color.RGBA
import scalismo.common.PointId
import scalismo.geometry._
import scalismo.mesh.{BarycentricCoordinates, SurfacePointProperty, TriangleId, TriangleProperty}

/**
 * The object PLYPropertyWriters is a collection of helpers to write a property to a file.
 *
 * The base class is IndexedProperty. On next more concrete level we have the SurfacePointPropertyWriter,
 * TrianglePropertyWriter and VertexPerTrianglePropertyWriter. Finally the different properties occurring in a mesh are
 * written using the remaining classes in this file.
 */
object PlyMeshPropertyWriters {

  import PlyHelpers.PlyHeader._
  import PlyHelpers._
  import PlyMeshHelper._

  trait IndexedProperty {
    def writeHeader(osw: OutputStreamWriter): Unit
    def write(fIdx: Int, osw: OutputStreamWriter): Unit
    def write(fIdx: Int, os: OutputStream, bo: ByteOrder): Unit
  }

  abstract class SurfacePointPropertyWriter[A] extends IndexedProperty {
    def property: SurfacePointProperty[A]

    def write(a: A, osw: OutputStreamWriter): Unit
    def write(a: A, os: OutputStream, bo: ByteOrder): Unit

    override def write(idx: Int, osw: OutputStreamWriter): Unit = {
      write(property.atPoint(PointId(idx)), osw)
    }
    override def write(idx: Int, os: OutputStream, bo: ByteOrder): Unit = {
      write(property.atPoint(PointId(idx)), os, bo)
    }
  }

  abstract class VertexPoint[D <: Dim](override val property: SurfacePointProperty[Point[D]])
      extends SurfacePointPropertyWriter[Point[D]] {
    val numberFormat: PlyHelpers.PlyTypes.Value = PlyTypes.float
    private val writer = new SequenceWriter[Float]()

    override def write(vec: Point[D], osw: OutputStreamWriter): Unit = {
      writer.write(vec.toArray.map(_.toFloat), osw)
    }

    override def write(vec: Point[D], os: OutputStream, bo: ByteOrder): Unit = {
      writer.write(vec.toArray.map(_.toFloat), os, bo)
    }
  }

  abstract class VertexVector[D <: Dim](override val property: SurfacePointProperty[EuclideanVector[D]])
      extends SurfacePointPropertyWriter[EuclideanVector[D]] {
    val numberFormat: PlyHelpers.PlyTypes.Value = PlyTypes.float
    private val writer = new SequenceWriter[Float]()

    override def write(vec: EuclideanVector[D], osw: OutputStreamWriter): Unit = {
      writer.write(vec.toArray.map(_.toFloat), osw)
    }

    override def write(vec: EuclideanVector[D], os: OutputStream, bo: ByteOrder): Unit = {
      writer.write(vec.toArray.map(_.toFloat), os, bo)
    }
  }

  abstract class TrianglePropertyWriter[A] extends IndexedProperty {
    def property: TriangleProperty[A]
    def write(a: A, osw: OutputStreamWriter): Unit
    def write(a: A, os: OutputStream, bo: ByteOrder): Unit
    override def write(idx: Int, osw: OutputStreamWriter): Unit = {
      write(property(TriangleId(idx)), osw)
    }
    override def write(idx: Int, os: OutputStream, bo: ByteOrder): Unit = {
      write(property(TriangleId(idx)), os, bo)
    }
  }


  class Vertex(val vertices: IndexedSeq[Point[_3D]]) extends IndexedProperty {
    val numberFormat: PlyHelpers.PlyTypes.Value = PlyTypes.float
    private val writer = new SequenceWriter[Float]

    override def write(idx: Int, osw: OutputStreamWriter): Unit = {
      writer.write(vertices(idx).toArray.map(_.toFloat), osw)
    }

    override def write(idx: Int, os: OutputStream, bo: ByteOrder): Unit = {
      writer.write(vertices(idx).toArray.map(_.toFloat), os, bo)
    }

    override def writeHeader(osw: OutputStreamWriter): Unit = {
      osw.write("%s %s %s\n".format(PLY.property, numberFormat, PLY.xCoordinate))
      osw.write("%s %s %s\n".format(PLY.property, numberFormat, PLY.yCoordinate))
      osw.write("%s %s %s\n".format(PLY.property, numberFormat, PLY.zCoordinate))
    }
  }

  class VertexColor(override val property: SurfacePointProperty[RGBA]) extends SurfacePointPropertyWriter[RGBA] {
    val numberFormat: PlyHelpers.PlyTypes.Value = PlyTypes.uchar
    private val writer = new SequenceWriter[Byte]

    override def write(color: RGBA, osw: OutputStreamWriter): Unit = {
      val clamped = color.clamped
      val seq = IndexedSeq(clamped.r, clamped.g, clamped.b, clamped.a).map(zeroOne2Byte)
      writer.write(seq, osw)
    }

    override def write(color: RGBA, os: OutputStream, bo: ByteOrder): Unit = {
      val clamped = color.clamped
      val seq = IndexedSeq(clamped.r, clamped.g, clamped.b, clamped.a).map(zeroOne2Byte)
      writer.write(seq, os, bo)
    }

    override def writeHeader(osw: OutputStreamWriter): Unit = {
      osw.write("%s %s %s\n".format(PLY.property, numberFormat, PLY.red))
      osw.write("%s %s %s\n".format(PLY.property, numberFormat, PLY.green))
      osw.write("%s %s %s\n".format(PLY.property, numberFormat, PLY.blue))
      osw.write("%s %s %s\n".format(PLY.property, numberFormat, PLY.alpha))
    }
  }

  class VertexNormal(property: SurfacePointProperty[EuclideanVector[_3D]]) extends VertexVector[_3D](property) {
    override def writeHeader(osw: OutputStreamWriter): Unit = {
      osw.write("%s %s %s\n".format(PLY.property, numberFormat, PLY.nx))
      osw.write("%s %s %s\n".format(PLY.property, numberFormat, PLY.ny))
      osw.write("%s %s %s\n".format(PLY.property, numberFormat, PLY.nz))
    }
  }

  class Faces(val faces: IndexedSeq[IntVector[_3D]]) extends IndexedProperty {
    val numberFormat: PlyHelpers.PlyTypes.Value = PlyTypes.int
    private val writer = new ListWriter[Int]

    override def writeHeader(osw: OutputStreamWriter): Unit = {
      osw.write("%s %s %s %s %s\n".format(PLY.property, PLY.list, PlyTypes.uchar, numberFormat, PLY.vertexIndices))
    }

    override def write(idx: Int, osw: OutputStreamWriter): Unit = {
      writer.write(faces(idx).toArray.toIndexedSeq, osw)
    }

    override def write(idx: Int, os: OutputStream, bo: ByteOrder): Unit = {
      writer.write(faces(idx).toArray.toIndexedSeq, os, bo)
    }
  }  
}
