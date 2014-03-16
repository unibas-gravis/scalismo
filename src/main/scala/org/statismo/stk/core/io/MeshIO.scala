package org.statismo.stk.core
package io

import java.io.File
import mesh.TriangleMesh
import scala.util.Try
import mesh.TriangleCell
import mesh.TriangleMesh
import org.statismo.stk.core.geometry._
import scala.util.Failure
import java.io.IOException
import vtk._
import org.statismo.stk.core.utils.MeshConversion
import scala.util.Success
import org.statismo.stk.core.mesh.ScalarMeshData
import reflect.runtime.universe.{ TypeTag, typeOf }
import scala.reflect.ClassTag
import org.statismo.stk.core.common.ScalarValue
import org.apache.commons.io.FilenameUtils
import scala.util.Failure
import org.statismo.stk.core.mesh.ScalarMeshData
import org.statismo.stk.core.geometry.Point3D
import scala.util.Success
import org.statismo.stk.core.mesh.TriangleCell

object MeshIO {

  def readMesh(file: File): Try[TriangleMesh] = {
    val filename = file.getAbsolutePath()
    filename match {
      case f if f.endsWith(".h5") => readHDF5(file)
      case f if f.endsWith(".vtk") => readVTK(file)
      case f if f.endsWith(".stl") => readSTL(file)
      case _ => {
        Failure(new IOException("Unknown file type received" + filename))
      }
    }
  }

  def readAndCorrectMesh(file: File): Try[TriangleMesh] = {
    val filename = file.getAbsolutePath()
    filename match {
      case f if f.endsWith(".vtk") => readVTK(file, true)
      case _ => {
        Failure(new IOException("Unknown file type received" + filename))
      }
    }
  }

  def writeMesh(mesh: TriangleMesh, file: File): Try[Unit] = {
    val filename = file.getAbsolutePath()
    filename match {
      case f if f.endsWith(".h5") => writeHDF5(mesh, file)
      case f if f.endsWith(".vtk") => writeVTK(mesh, file)
      case f if f.endsWith(".stl") => writeSTL(mesh, file)
      case _ => {
        Failure(new IOException("Unknown file type received" + filename))
      }
    }
  }

  def writeMeshData[Scalar: ScalarValue: TypeTag: ClassTag](meshData: ScalarMeshData[Scalar], file: File): Try[Unit] = {
    val filename = file.getAbsolutePath()
    filename match {
      case f if f.endsWith(".vtk") => writeVTK(meshData, file)
      case _ => {
        Failure(new IOException("Unknown file type received" + filename))
      }
    }
  }

  def writeHDF5(surface: TriangleMesh, file: File): Try[Unit] = {

    val domainPoints: IndexedSeq[Point[ThreeD]] = surface.points.toIndexedSeq
    val cells: IndexedSeq[TriangleCell] = surface.cells

    val maybeError: Try[Unit] = for {
      h5file <- HDF5Utils.createFile(file)
      _ <- h5file.writeNDArray("/Surface/0/Vertices", pointSeqToNDArray(domainPoints))
      _ <- h5file.writeNDArray("/Surface/0/Cells", cellSeqToNDArray(cells))
      _ <- Try { h5file.close() }
    } yield { () }

    maybeError
  }


  def writeVTK[S: ScalarValue: TypeTag: ClassTag](meshData: ScalarMeshData[S], file: File): Try[Unit] = {
    val vtkPd = MeshConversion.meshDataToVtkPolyData(meshData)
    writeVTKPdasVTK(vtkPd, file)
  }

  def writeVTK(surface: TriangleMesh, file: File): Try[Unit] = {
    val vtkPd = MeshConversion.meshToVTKPolyData(surface)
    writeVTKPdasVTK(vtkPd, file)
  }

  def writeSTL(surface: TriangleMesh, file: File): Try[Unit] = {
    val vtkPd = MeshConversion.meshToVTKPolyData(surface)
    writeVTKPdAsSTL(vtkPd, file)
  }



  private def writeVTKPdasVTK(vtkPd: vtkPolyData, file: File): Try[Unit] = {
    val writer =  new vtkPolyDataWriter()
    writer.SetFileName(file.getAbsolutePath())
    writer.SetInputData(vtkPd)
    writer.SetFileTypeToBinary()
    writer.Update()
    val succOrFailure = if (writer.GetErrorCode() != 0) {
      Failure(new IOException("could not write file ${file.getAbsolutePath} (received error code ${writer.GetErrorCode})"))
    } else {
      Success(())
    }
    vtkPd.Delete
    writer.Delete()
    succOrFailure
  }



  private def writeVTKPdAsSTL(vtkPd: vtkPolyData, file: File): Try[Unit] = {
    val writer =  new vtkSTLWriter()
    writer.SetFileName(file.getAbsolutePath())
    writer.SetInputData(vtkPd)
    writer.SetFileTypeToBinary()
    writer.Update()
    val succOrFailure = if (writer.GetErrorCode() != 0) {
      Failure(new IOException("could not write file ${file.getAbsolutePath} (received error code ${writer.GetErrorCode})"))
    } else {
      Success(())
    }
    vtkPd.Delete
    writer.Delete()
    succOrFailure
  }


  private def readVTK(file: File, correctMesh: Boolean = false): Try[TriangleMesh] = {
    val vtkReader = new vtkPolyDataReader()
    vtkReader.SetFileName(file.getAbsolutePath())
    vtkReader.Update()
    val errCode = vtkReader.GetErrorCode()
    if (errCode != 0) {
      return Failure(new IOException(s"Could not read vtk mesh (received error code $errCode"))
    }

    val vtkPd = vtkReader.GetOutput()
    val mesh = if (correctMesh) MeshConversion.vtkPolyDataToCorrectedTriangleMesh(vtkPd)
    else MeshConversion.vtkPolyDataToTriangleMesh(vtkPd)

    vtkReader.Delete()
    vtkPd.Delete()
    mesh
  }



  private def readSTL(file: File, correctMesh: Boolean = false): Try[TriangleMesh] = {
    val stlReader = new vtkSTLReader()
    stlReader.SetFileName(file.getAbsolutePath())
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



  def readHDF5(file: File): Try[TriangleMesh] = {

    val filename = file.getAbsolutePath()

    val maybeSurface = for {
      h5file <- HDF5Utils.openFileForReading(file)
      vertArray <- h5file.readNDArray[Double]("/Surface/0/Vertices")
      cellArray <- h5file.readNDArray[Int]("/Surface/0/Cells")
      _ <- Try { h5file.close() }
    } yield TriangleMesh(NDArrayToPointSeq(vertArray), NDArrayToCellSeq(cellArray))

    maybeSurface
  }

  private def NDArrayToPointSeq(ndarray: NDArray[Double]): IndexedSeq[Point3D] = {
    // take block of 3, map them to 3dPoints and convert the resulting array to an indexed seq 
    ndarray.data.grouped(3).map(grp => Point3D(grp(0).toFloat, grp(1).toFloat, grp(2).toFloat)).toIndexedSeq
  }

  private def NDArrayToCellSeq(ndarray: NDArray[Int]): IndexedSeq[TriangleCell] = {
    // take block of 3, map them to 3dPoints and convert the resulting array to an indexed seq 
    ndarray.data.grouped(3).map(grp => TriangleCell(grp(0), grp(1), grp(2))).toIndexedSeq
  }

  private def pointSeqToNDArray[T](points: IndexedSeq[Point[ThreeD]]): NDArray[Double] =
    NDArray(Vector(points.size, 3), points.flatten(pt => pt.data.map(_.toDouble)).toArray)

  private def cellSeqToNDArray[T](cells: IndexedSeq[TriangleCell]): NDArray[Int] =
    NDArray(Vector(cells.size, 3), cells.flatten(cell => cell.pointIds).toArray)

}



