/*
 * Copyright 2015 University of Basel, Graphics and Vision Research Group
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package scalismo.io

import scalismo.color.{RGB, RGBA}
import scalismo.common.DiscreteField.{ScalarMeshField, ScalarVolumeMeshField}
import scalismo.common.{PointId, Scalar, UnstructuredPoints}
import scalismo.geometry.*
import scalismo.hdf5json.HDFPath
import scalismo.io.ply.PLY
import scalismo.io.statisticalmodel.{NDArray, StatisticalModelIOUtils}
import scalismo.io.stl.STL
import scalismo.mesh.*
import scalismo.mesh.TriangleMesh.*
import java.io.{BufferedReader, File, FileReader, IOException}
import scala.reflect.ClassTag
import scala.util.{Failure, Success, Try}

object MeshIO {

  def readMesh(file: File): Try[TriangleMesh[_3D]] = {
    val filename = file.getAbsolutePath
    filename match {
      case f if f.endsWith(".stl") => STL.read(file)
      case f if f.endsWith(".ply") => {
        PLY.read(file).map {
          case Right(vertexColor) => vertexColor.shape
          case Left(shape)        => shape
        }
      }
      case _ =>
        Failure(new IOException("Unknown file type received" + filename))
    }
  }

  def readVertexColorMesh3D(file: File): Try[VertexColorMesh3D] = {
    val filename = file.getAbsolutePath
    filename match {
      case f if f.endsWith(".ply") =>
        PLY.read(file).map {
          case Right(colorMesh3D) => colorMesh3D
          case Left(_)            => throw new Exception("Indicated PLY file does not contain color values.")
        }

      case _ =>
        Failure(new IOException("Unknown file type received" + filename))
    }
  }

  def writeMesh(mesh: TriangleMesh[_3D], file: File): Try[Unit] = {
    val filename = file.getAbsolutePath
    filename match {
      case f if f.endsWith(".h5")  => writeHDF5(mesh, file)
      case f if f.endsWith(".ply") => PLY.write(mesh, file)
      case f if f.endsWith(".stl") => STL.write(mesh, file)
      case _ =>
        Failure(new IOException("Unknown file type received" + filename))
    }
  }

  /**
   * Writes a [[VertexColorMesh3D]] to a file.
   *
   * **Important**: For PLY, since we use the VTK file writer, and since it does not support RGBA, only RGB, the alpha
   * channel will be ignored while writing.
   */
  def writeVertexColorMesh3D(mesh: VertexColorMesh3D, file: File): Try[Unit] = {
    val filename = file.getAbsolutePath
    filename match {
      case f if f.endsWith(".ply") => PLY.write(mesh, file)
      case _ =>
        Failure(new IOException("Unknown file type received" + filename))
    }
  }

  def writeHDF5(surface: TriangleMesh[_3D], file: File): Try[Unit] = {

    val domainPoints: IndexedSeq[Point[_3D]] = surface.pointSet.points.toIndexedSeq
    val cells: IndexedSeq[TriangleCell] = surface.cells

    val maybeError: Try[Unit] = for {
      h5file <- StatisticalModelIOUtils.createFile(file)
      _ <- h5file.writeNDArray(HDFPath("/Surface/0/Vertices"), pointSeqToNDArray(domainPoints))
      _ <- h5file.writeNDArray(HDFPath("/Surface/0/Cells"), cellSeqToNDArray(cells))
      _ <- Try {
        h5file.close()
      }
    } yield {
      ()
    }

    maybeError
  }

  private def pointSeqToNDArray[T](points: IndexedSeq[Point[_3D]]): NDArray[Double] =
    NDArray(IndexedSeq(points.size, 3), points.flatten(pt => pt.toArray.map(_.toDouble)).toArray)

  private def cellSeqToNDArray[T](cells: IndexedSeq[TriangleCell]): NDArray[Int] =
    NDArray(IndexedSeq(cells.size, 3), cells.flatten(cell => cell.pointIds.map(_.id)).toArray)

}
