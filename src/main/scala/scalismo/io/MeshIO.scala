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

import java.io.{File, IOException}

import scalismo.color.RGBA
import scalismo.common.{PointId, Scalar, UnstructuredPointsDomain}
import scalismo.geometry._
import scalismo.mesh._
import scalismo.geometry.Point._
import scalismo.geometry.Vector._
import scalismo.mesh.TriangleMesh._
import scalismo.utils.MeshConversion
import vtk._

import scala.reflect.ClassTag
import scala.reflect.runtime.universe.TypeTag
import scala.util.{Failure, Success, Try}

object MeshIO {
  /**
   * Implements methods for reading and writing D-dimensional meshes
   *
   * '''WARNING! WE ARE USING an LPS WORLD COORDINATE SYSTEM'''
   *
   * This means that when reading mesh files such as .stl or .vtk, we assume the point coordinates
   * to lie in an LPS world and map them unchanged in our coordinate system.
   *
   * The same happens at writing, we directly dump our vertex coordinates into the file format(stl, or vtk) without any
   * mirroring magic.
   *
   *
   * *
   */

  /**
   * Reads a ScalarMeshField from file. The indicated Scalar type S must match the data type encoded in the file
   *
   */

  def readScalarMeshField[S: Scalar: TypeTag: ClassTag](file: File): Try[ScalarMeshField[S]] = {
    val requiredScalarType = ImageIO.ScalarType.fromType[S]
    val filename = file.getAbsolutePath
    filename match {
      case f if f.endsWith(".vtk") => readVTKPolydata(file).flatMap { pd =>
        val spScalarType = ImageIO.ScalarType.fromVtkId(pd.GetPointData().GetScalars().GetDataType())
        MeshConversion.vtkPolyDataToScalarMeshField(pd)
        if (requiredScalarType != spScalarType) {
          Failure(new Exception(s"Invalid scalar type (expected $requiredScalarType, found $spScalarType)"))
        } else {
          MeshConversion.vtkPolyDataToScalarMeshField(pd)
        }
      }
      case _ =>
        Failure(new IOException("Unknown file type received" + filename))
    }
  }

  /**
   * Reads a ScalarMeshField from file while casting its data to the indicated Scalar type S if necessary
   *
   */

  def readScalarMeshFieldAsType[S: Scalar: TypeTag: ClassTag](file: File): Try[ScalarMeshField[S]] = {
    val filename = file.getAbsolutePath
    filename match {
      case f if f.endsWith(".vtk") => readVTKPolydata(file).flatMap(pd => MeshConversion.vtkPolyDataToScalarMeshField(pd))
      case _ =>
        Failure(new IOException("Unknown file type received" + filename))
    }
  }

  def readMesh(file: File): Try[TriangleMesh[_3D]] = {
    val filename = file.getAbsolutePath
    filename match {
      case f if f.endsWith(".h5") => readHDF5(file)
      case f if f.endsWith(".vtk") => readVTK(file)
      case f if f.endsWith(".stl") => readSTL(file)
      case f if f.endsWith(".ply") => readPLY(file)
      case _ =>
        Failure(new IOException("Unknown file type received" + filename))
    }
  }

  def readAndCorrectMesh(file: File): Try[TriangleMesh[_3D]] = {
    val filename = file.getAbsolutePath
    filename match {
      case f if f.endsWith(".vtk") => readVTK(file, correctMesh = true)
      case _ =>
        Failure(new IOException("Unknown file type received" + filename))
    }
  }

  def readLineMesh2D(file: File): Try[LineMesh[_2D]] = {
    val filename = file.getAbsolutePath
    filename match {
      case f if f.endsWith(".vtk") => readLineMeshVTK(file)
      case _ =>
        Failure(new IOException("Unknown file type received" + filename))
    }

  }

  def readLineMesh3D(file: File): Try[LineMesh[_3D]] = {
    val filename = file.getAbsolutePath
    filename match {
      case f if f.endsWith(".vtk") => readLineMeshVTK(file)
      case _ =>
        Failure(new IOException("Unknown file type received" + filename))
    }
  }

  def writeLineMesh[D <: Dim: NDSpace](polyLine: LineMesh[D], file: File): Try[Unit] = {
    val filename = file.getAbsolutePath
    filename match {
      case f if f.endsWith(".vtk") => writeLineMeshVTK(polyLine, file)
      case _ =>
        Failure(new IOException("Unknown file type received" + filename))
    }
  }

  def writeMesh(mesh: TriangleMesh[_3D], file: File): Try[Unit] = {
    val filename = file.getAbsolutePath
    filename match {
      case f if f.endsWith(".h5") => writeHDF5(mesh, file)
      case f if f.endsWith(".vtk") => writeVTK(mesh, file)
      case f if f.endsWith(".stl") => writeSTL(mesh, file)
      case _ =>
        Failure(new IOException("Unknown file type received" + filename))
    }
  }

  def writeScalarMeshField[S: Scalar: TypeTag: ClassTag](meshData: ScalarMeshField[S], file: File): Try[Unit] = {
    val filename = file.getAbsolutePath
    filename match {
      case f if f.endsWith(".vtk") => writeVTK(meshData, file)
      case _ =>
        Failure(new IOException("Unknown file type received" + filename))
    }
  }

  def writeHDF5(surface: TriangleMesh[_3D], file: File): Try[Unit] = {

    val domainPoints: IndexedSeq[Point[_3D]] = surface.pointSet.points.toIndexedSeq
    val cells: IndexedSeq[TriangleCell] = surface.cells

    val maybeError: Try[Unit] = for {
      h5file <- HDF5Utils.createFile(file)
      _ <- h5file.writeNDArray("/Surface/0/Vertices", pointSeqToNDArray(domainPoints))
      _ <- h5file.writeNDArray("/Surface/0/Cells", cellSeqToNDArray(cells))
      _ <- Try {
        h5file.close()
      }
    } yield {
      ()
    }

    maybeError
  }

  def writeVTK[S: Scalar: TypeTag: ClassTag](meshData: ScalarMeshField[S], file: File): Try[Unit] = {
    val vtkPd = MeshConversion.scalarMeshFieldToVtkPolyData(meshData)
    val err = writeVTKPdasVTK(vtkPd, file)
    vtkPd.Delete()
    err
  }

  def writeVTK(surface: TriangleMesh[_3D], file: File): Try[Unit] = {
    val vtkPd = MeshConversion.meshToVtkPolyData(surface)
    val err = writeVTKPdasVTK(vtkPd, file)
    vtkPd.Delete()
    err
  }

  def writeSTL(surface: TriangleMesh[_3D], file: File): Try[Unit] = {
    val vtkPd = MeshConversion.meshToVtkPolyData(surface)
    val err = writeVTKPdAsSTL(vtkPd, file)
    vtkPd.Delete()
    err
  }

  private def writeVTKPdasVTK(vtkPd: vtkPolyData, file: File): Try[Unit] = {
    val writer = new vtkPolyDataWriter()
    writer.SetFileName(file.getAbsolutePath)
    writer.SetInputData(vtkPd)
    writer.SetFileTypeToBinary()
    writer.Update()
    val succOrFailure = if (writer.GetErrorCode() != 0) {
      Failure(new IOException(s"could not write file ${file.getAbsolutePath} (received error code ${writer.GetErrorCode})"))
    } else {
      Success(())
    }
    writer.Delete()
    succOrFailure
  }

  private def writeVTKPdAsSTL(vtkPd: vtkPolyData, file: File): Try[Unit] = {
    val writer = new vtkSTLWriter()
    writer.SetFileName(file.getAbsolutePath)
    writer.SetInputData(vtkPd)
    writer.SetFileTypeToBinary()
    writer.Update()
    val succOrFailure = if (writer.GetErrorCode() != 0) {
      Failure(new IOException(s"could not write file ${file.getAbsolutePath} (received error code ${writer.GetErrorCode})"))
    } else {
      Success(())
    }
    writer.Delete()
    succOrFailure
  }

  private def readVTKPolydata(file: File): Try[vtkPolyData] = {

    val vtkReader = new vtkPolyDataReader()
    vtkReader.SetFileName(file.getAbsolutePath)
    vtkReader.Update()
    val errCode = vtkReader.GetErrorCode()
    if (errCode != 0) {
      return Failure(new IOException(s"Could not read vtk mesh (received error code $errCode"))
    }
    val data = vtkReader.GetOutput()
    vtkReader.Delete()
    Success(data)
  }

  private def readVTK(file: File, correctMesh: Boolean = false): Try[TriangleMesh[_3D]] = {
    for {
      vtkPd <- readVTKPolydata(file)
      mesh <- {
        if (correctMesh) MeshConversion.vtkPolyDataToCorrectedTriangleMesh(vtkPd) else MeshConversion.vtkPolyDataToTriangleMesh(vtkPd)
      }
    } yield {
      vtkPd.Delete()
      mesh
    }
  }

  private def readSTL(file: File, correctMesh: Boolean = false): Try[TriangleMesh[_3D]] = {
    val stlReader = new vtkSTLReader()
    stlReader.SetFileName(file.getAbsolutePath)

    stlReader.MergingOn()

    // With the default point locator, it may happen that the stlReader merges
    // points that are very close by but not identical. To make sure that this never happens
    // we explicitly specify the tolerance.
    val pointLocator = new vtkMergePoints()
    pointLocator.SetTolerance(0.0)

    stlReader.SetLocator(pointLocator)
    stlReader.Update()
    val errCode = stlReader.GetErrorCode()
    if (errCode != 0) {
      return Failure(new IOException(s"Could not read stl mesh (received error code $errCode"))
    }

    val vtkPd = stlReader.GetOutput()
    val mesh = if (correctMesh) MeshConversion.vtkPolyDataToCorrectedTriangleMesh(vtkPd)
    else MeshConversion.vtkPolyDataToTriangleMesh(vtkPd)

    stlReader.Delete()
    vtkPd.Delete()
    mesh
  }

  private def readPLY(file: File): Try[TriangleMesh[_3D]] = {
    val plyReader = new vtkPLYReader()
    plyReader.SetFileName(file.getAbsolutePath)

    plyReader.Update()

    val errCode = plyReader.GetErrorCode()
    if (errCode != 0) {
      return Failure(new IOException(s"Could not read stl mesh (received error code $errCode"))
    }

    val vtkPd = plyReader.GetOutput()
    val meshGeometry = MeshConversion.vtkPolyDataToTriangleMesh(vtkPd)

    // FIXME should be depending on the array
    val mesh = vtkPd.GetPointData().GetArrayName(0) match {
      case "RGBA" => {
        val s = vtkPd.GetPointData().GetArray(0)
        val colors = for (i <- 0 until s.GetNumberOfTuples()) yield {
          val rgba = s.GetTuple4(i)
          RGBA(rgba(0), rgba(1), rgba(2), rgba(3))
        }
        // FIXME this should not be a get
        VertexColorMesh3D(meshGeometry.get, new SurfacePointProperty[RGBA](meshGeometry.get.triangulation, colors.toIndexedSeq))
      }
      case "RGB" => {
        val s = vtkPd.GetPointData().GetArray(0)
        val colors = for (i <- 0 until s.GetNumberOfTuples()) yield {
          val rgba = s.GetTuple3(i)
          RGBA(rgba(0), rgba(1), rgba(2), 1.0)
        }
        // FIXME this should not be a get
        VertexColorMesh3D(meshGeometry.get, new SurfacePointProperty[RGBA](meshGeometry.get.triangulation, colors.toIndexedSeq))
      }
      case _ => meshGeometry.get
    }

    plyReader.Delete()
    vtkPd.Delete()
    Success(mesh)
  }


  def readHDF5(file: File): Try[TriangleMesh[_3D]] = {

    val maybeSurface = for {
      h5file <- HDF5Utils.openFileForReading(file)
      vertArray <- h5file.readNDArray[Double]("/Surface/0/Vertices")
      cellArray <- h5file.readNDArray[Int]("/Surface/0/Cells")
      _ <- Try {
        h5file.close()
      }
    } yield {
      TriangleMesh3D(UnstructuredPointsDomain(NDArrayToPointSeq(vertArray).toIndexedSeq), TriangleList(NDArrayToCellSeq(cellArray)))
    }

    maybeSurface
  }

  private def NDArrayToPointSeq(ndarray: NDArray[Double]): IndexedSeq[Point[_3D]] = {
    // take block of 3, map them to 3dPoints and convert the resulting array to an indexed seq 
    ndarray.data.grouped(3).map(grp => Point(grp(0).toFloat, grp(1).toFloat, grp(2).toFloat)).toIndexedSeq
  }

  private def NDArrayToCellSeq(ndarray: NDArray[Int]): IndexedSeq[TriangleCell] = {
    // take block of 3, map them to 3dPoints and convert the resulting array to an indexed seq 
    ndarray.data.grouped(3).map(grp => TriangleCell(PointId(grp(0)), PointId(grp(1)), PointId(grp(2)))).toIndexedSeq
  }

  private def pointSeqToNDArray[T](points: IndexedSeq[Point[_3D]]): NDArray[Double] =
    NDArray(IndexedSeq(points.size, 3), points.flatten(pt => pt.toArray.map(_.toDouble)).toArray)

  private def cellSeqToNDArray[T](cells: IndexedSeq[TriangleCell]): NDArray[Int] =
    NDArray(IndexedSeq(cells.size, 3), cells.flatten(cell => cell.pointIds.map(_.id)).toArray)

  private def readLineMeshVTK[D <: Dim: NDSpace: LineMesh.Create: UnstructuredPointsDomain.Create](file: File): Try[LineMesh[D]] = {
    val vtkReader = new vtkPolyDataReader()
    vtkReader.SetFileName(file.getAbsolutePath)
    vtkReader.Update()
    val errCode = vtkReader.GetErrorCode()
    if (errCode != 0) {
      return Failure(new IOException(s"Could not read vtk mesh (received error code $errCode"))
    }

    val vtkPd = vtkReader.GetOutput()
    val correctedMesh = for {
      polyline <- MeshConversion.vtkPolyDataToLineMesh[D](vtkPd)
    } yield {
      LineMesh.enforceConsistentCellDirections[D](polyline)
    }
    vtkReader.Delete()
    vtkPd.Delete()
    correctedMesh
  }

  private[this] def writeLineMeshVTK[D <: Dim: NDSpace](mesh: LineMesh[D], file: File): Try[Unit] = {
    val vtkPd = MeshConversion.lineMeshToVTKPolyData(mesh)
    val err = writeVTKPdasVTK(vtkPd, file)
    vtkPd.Delete()
    err
  }

}

