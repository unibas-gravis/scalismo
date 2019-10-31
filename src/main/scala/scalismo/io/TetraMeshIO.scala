package scalismo.io

import java.io.{BufferedReader, File, FileReader, IOException}

import scalismo.color.{RGB, RGBA}
import scalismo.common.{PointId, Scalar, UnstructuredPointsDomain}
import scalismo.geometry._
import scalismo.mesh.TriangleMesh._
import scalismo.mesh._
import scalismo.tetramesh.{TetrahedralCell, TetrahedralMesh}
import scalismo.utils.{MeshConversion, TetraMeshConversion, VtkHelpers}
import vtk.{vtkUnstructuredGridReader, _}

import scala.io.Source
import scala.reflect.ClassTag
import scala.reflect.runtime.universe.TypeTag
import scala.util.{Failure, Success, Try}
object TetraMeshIO {
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
   * Reads a tetrahedral mesh from file while casting its data to the indicated Scalar type S if necessary
   *
   */

  /* def readScalarMeshFieldAsType[S: Scalar: TypeTag: ClassTag](file: File): Try[ScalarMeshField[S]] = {
    val filename = file.getAbsolutePath
    filename match {
      case f if f.endsWith(".vtk") => readVTKPolydata(file).flatMap(pd => MeshConversion.vtkPolyDataToScalarMeshField(pd))
      case _ =>
        Failure(new IOException("Unknown file type received" + filename))
    }
  }*/

  def readTetraMesh(file: File): Try[TetrahedralMesh[_3D]] = {
    val filename = file.getAbsolutePath
    filename match {
      case f if f.endsWith(".inp") => readVTKAVSucd(file)
     case f if f.endsWith(".vtk") => readVTK(file)
      case f if f.endsWith(".vtu") => readVTU(file)
      /* case f if f.endsWith(".ply") => {
        readPLY(file).map { res =>
          res match {
            case Right(vertexColor) => vertexColor.shape
            case Left(shape) => shape
          }
        }
      }*/
      case _ =>
        Failure(new IOException("Unknown file type received" + filename))
    }
  }





  def writeTetraMesh(mesh: TetrahedralMesh[_3D], file: File): Try[Unit] = {
    val filename = file.getAbsolutePath
    filename match {
      // case f if f.endsWith(".h5") => writeHDF5(mesh, file)
     case f if f.endsWith(".vtk") => writeVTK(mesh, file)
      case f if f.endsWith(".vtu") => writeVTU(mesh, file)
      //case f if f.endsWith(".ply") => writePLY(Left(mesh), file)
      case _ =>
        Failure(new IOException("Unknown file type received" + filename))
    }
  }




  def writeVTK(volume: TetrahedralMesh[_3D], file: File): Try[Unit] = {
    val vtkPd = TetraMeshConversion.tetrameshTovtkUnstructuredGrid(volume)
    val err = writeVTKUgasVTK(vtkPd, file)
    vtkPd.Delete()
    err
  }



  def writeVTU(volume: TetrahedralMesh[_3D], file: File): Try[Unit] = {
    val vtkUg = TetraMeshConversion.tetrameshTovtkUnstructuredGrid(volume)
    val err = writeVTKUgasVTU(vtkUg, file)
    vtkUg.Delete()
    err
  }





  private def writeVTKUgasVTK(vtkUg: vtkUnstructuredGrid, file: File): Try[Unit] = {
    val writer = new vtkUnstructuredGridWriter()
    writer.SetFileName(file.getAbsolutePath)
    writer.SetInputData(vtkUg)
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


  private def writeVTKUgasVTU(vtkUg: vtkUnstructuredGrid, file: File): Try[Unit] = {
    val writer = new vtkXMLUnstructuredGridWriter()
    writer.SetFileName(file.getAbsolutePath)
    writer.SetInputData(vtkUg)
    writer.SetDataModeToBinary()
    writer.Update()
    val succOrFailure = if (writer.GetErrorCode() != 0) {
      Failure(new IOException(s"could not write file ${file.getAbsolutePath} (received error code ${writer.GetErrorCode})"))
    } else {
      Success(())
    }
    writer.Delete()
    succOrFailure
  }






  private def readVTKUnstructuredGrid(file: File): Try[vtkUnstructuredGrid] = {

    val vtkReader = new vtkUnstructuredGridReader()
    vtkReader.SetFileName(file.getAbsolutePath)
    vtkReader.Update()

    val extract = new vtkExtractUnstructuredGrid()
    extract.SetInputConnection(vtkReader.GetOutputPort())

    val errCode = vtkReader.GetErrorCode()
    if (errCode != 0) {
      return Failure(new IOException(s"Could not read vtk UnstructuredGrid (received error code $errCode"))
    }
    val data = vtkReader.GetOutput()
    vtkReader.Delete()
    Success(data)
  }




  private def readVTKXMLUnstructuredGrid(file: File): Try[vtkUnstructuredGrid] = {

    val vtkReader = new vtkXMLUnstructuredGridReader()
    vtkReader.SetFileName(file.getAbsolutePath)
    vtkReader.Update()

    val errCode = vtkReader.GetErrorCode()
    if (errCode != 0) {
      return Failure(new IOException(s"Could not read vtk UnstructuredGrid (received error code $errCode"))
    }
    val data = vtkReader.GetOutput()
    vtkReader.Delete()
    Success(data)
  }

  private def readvtkAVSucdUnstructuredGrid(file: File): Try[vtkUnstructuredGrid] = {
    val vtkavsReader = new vtkAVSucdReader()
    vtkavsReader.SetFileName(file.getAbsolutePath)
    vtkavsReader.Update()
    val errCode = vtkavsReader.GetErrorCode()
    if (errCode != 0) {
      return Failure(new IOException(s"Could not read vtk UnstructuredGrid (received error code $errCode"))
    }
    val data = vtkavsReader.GetOutput()
    vtkavsReader.Delete()
    Success(data)
  }

  private def readVTU(file: File, correctMesh: Boolean = false): Try[TetrahedralMesh[_3D]] = {
    for {
      vtkUg <- readVTKXMLUnstructuredGrid(file)
      tetramesh <- {
        if (correctMesh) TetraMeshConversion.vtkUnstructuredGridToCorrectedTetrahedralMesh(vtkUg) else TetraMeshConversion.vtkUnstructuredGridToTetrahedralMesh(vtkUg)
      }
    } yield {
      vtkUg.Delete()
      tetramesh
    }
  }

  private def readVTKAVSucd(file: File, correctMesh: Boolean = false): Try[TetrahedralMesh[_3D]] = {
    for {
      vtkUg <- readvtkAVSucdUnstructuredGrid(file)
      tetramesh <- {
        if (correctMesh) TetraMeshConversion.vtkUnstructuredGridToCorrectedTetrahedralMesh(vtkUg) else TetraMeshConversion.vtkUnstructuredGridToTetrahedralMesh(vtkUg)
      }
    } yield {
      vtkUg.Delete()
      tetramesh
    }
  }


  private def readVTK(file: File, correctMesh: Boolean = false): Try[TetrahedralMesh[_3D]] =
  {
    for {
      vtkUg <- readVTKUnstructuredGrid(file)
      tetramesh <- {
        if (correctMesh) TetraMeshConversion.vtkUnstructuredGridToCorrectedTetrahedralMesh(vtkUg) else TetraMeshConversion.vtkUnstructuredGridToTetrahedralMesh(vtkUg)
      }
    } yield {
      vtkUg.Delete()
      tetramesh
    }

  }




  private def getColorArray(UGrid: vtkUnstructuredGrid): Option[(String, vtkDataArray)] = {
    if (UGrid.GetPointData() == null || UGrid.GetPointData().GetNumberOfArrays() == 0) None
    else {
      val pointData = UGrid.GetPointData()
      val pointDataArrays = for (i <- 0 until pointData.GetNumberOfArrays()) yield {
        (pointData.GetArrayName(i), pointData.GetArray(i))
      }
      pointDataArrays.find { case (name, array) => name == "RGB" || name == "RGBA" }
    }
  }






  private def NDArrayToPointSeq(ndarray: NDArray[Double]): IndexedSeq[Point[_3D]] = {
    // take block of 3, map them to 3dPoints and convert the resulting array to an indexed seq
    ndarray.data.grouped(3).map(grp => Point(grp(0).toFloat, grp(1).toFloat, grp(2).toFloat)).toIndexedSeq
  }

  private def NDArrayToCellSeq(ndarray: NDArray[Int]): IndexedSeq[TetrahedralCell] = {
    // take block of 3, map them to 3dPoints and convert the resulting array to an indexed seq
    ndarray.data.grouped(4).map(grp => TetrahedralCell(PointId(grp(0)), PointId(grp(1)), PointId(grp(2)), PointId(grp(3)))).toIndexedSeq
  }

  private def pointSeqToNDArray[T](points: IndexedSeq[Point[_3D]]): NDArray[Double] =
    NDArray(IndexedSeq(points.size, 3), points.flatten(pt => pt.toArray.map(_.toDouble)).toArray)

  private def cellSeqToNDArray[T](cells: IndexedSeq[TetrahedralCell]): NDArray[Int] =
    NDArray(IndexedSeq(cells.size, 3), cells.flatten(cell => cell.pointIds.map(_.id)).toArray)



}

