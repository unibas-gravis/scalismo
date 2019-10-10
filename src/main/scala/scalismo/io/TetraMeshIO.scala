package scalismo.io

import java.io.{ BufferedReader, File, FileReader, IOException }

import scalismo.color.{ RGB, RGBA }
import scalismo.common.{ PointId, Scalar, UnstructuredPointsDomain }
import scalismo.geometry._
import scalismo.mesh.TriangleMesh._
import scalismo.mesh._
import scalismo.tetramesh.{ TetrahedralCell, TetrahedralMesh }
import scalismo.utils.{ MeshConversion, TetraMeshConversion }
import vtk.{ vtkUnstructuredGridReader, _ }

import scala.io.Source
import scala.reflect.ClassTag
import scala.reflect.runtime.universe.TypeTag
import scala.util.{ Failure, Success, Try }
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
   * Reads a ScalarMeshField from file. The indicated Scalar type S must match the data type encoded in the file
   *
   */

  /* def readScalarMeshField[S: Scalar: TypeTag: ClassTag](file: File): Try[ScalarMeshField[S]] = {
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
  }*/

  /**
   * Reads a ScalarMeshField from file while casting its data to the indicated Scalar type S if necessary
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

  /* def readVertexColorMesh3D(file: File): Try[VertexColorMesh3D] = {
    val filename = file.getAbsolutePath
    filename match {
      case f if f.endsWith(".ply") => readPLY(file).map { r =>
        r match {
          case Right(colorMesh3D) => colorMesh3D
          case Left(_) => throw new Exception("Indicated PLY file does not contain color values.")
        }
      }

      case _ =>
        Failure(new IOException("Unknown file type received" + filename))
    }
  }*/

  def readAndCorrectTetraMesh(file: File): Try[TetrahedralMesh[_3D]] = {
    val filename = file.getAbsolutePath
    filename match {
      case f if f.endsWith(".vtk") => readVTK(file, correctMesh = true)
      case _ =>
        Failure(new IOException("Unknown file type received" + filename))
    }
  }

  /*def readLineMesh3D(file: File): Try[LineMesh[_3D]] = {
    val filename = file.getAbsolutePath
    filename match {
      case f if f.endsWith(".vtk") => readLineMeshVTK(file)
      case _ =>
        Failure(new IOException("Unknown file type received" + filename))
    }
  }*/

  /*def writeLineMesh[D: NDSpace](polyLine: LineMesh[D], file: File): Try[Unit] = {
    val filename = file.getAbsolutePath
    filename match {
      case f if f.endsWith(".vtk") => writeLineMeshVTK(polyLine, file)
      case _ =>
        Failure(new IOException("Unknown file type received" + filename))
    }
  }*/

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

  /**
   * Writes a [[VertexColorMesh3D]] to a file.
   *
   * **Important**:  For PLY, since we use the VTK file writer, and since it does not support RGBA, only RGB, the alpha channel will be ignored while writing.
   */
  /*def writeVertexColorMesh3D(mesh: VertexColorMesh3D, file: File): Try[Unit] = {
    val filename = file.getAbsolutePath
    filename match {
      case f if f.endsWith(".ply") => writePLY(Right(mesh), file)
      case _ =>
        Failure(new IOException("Unknown file type received" + filename))
    }
  }*/

  /* def writeScalarMeshField[S: Scalar: TypeTag: ClassTag](meshData: ScalarMeshField[S], file: File): Try[Unit] = {
    val filename = file.getAbsolutePath
    filename match {
      case f if f.endsWith(".vtk") => writeVTK(meshData, file)
      case _ =>
        Failure(new IOException("Unknown file type received" + filename))
    }
  }*/

  /* def writeHDF5(surface: TriangleMesh[_3D], file: File): Try[Unit] = {

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
  }*/

  /*def writeVTK[S: Scalar: TypeTag: ClassTag](meshData: ScalarMeshField[S], file: File): Try[Unit] = {
    val vtkPd = MeshConversion.scalarMeshFieldToVtkPolyData(meshData)
    val err = writeVTKPdasVTK(vtkPd, file)
    vtkPd.Delete()
    err
  }*/

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

  /*def writeSTL(surface: TriangleMesh[_3D], file: File): Try[Unit] = {
    val vtkPd = MeshConversion.meshToVtkPolyData(surface)
    val err = writeVTKPdAsSTL(vtkPd, file)
    vtkPd.Delete()
    err
  }*/

  /* private def writePLY(surface: Either[TriangleMesh[_3D], VertexColorMesh3D], file: File): Try[Unit] = {

    val vtkPd = surface match {
      case Right(colorMesh) => MeshConversion.meshToVtkPolyData(colorMesh.shape)
      case Left(shapeOnly) => MeshConversion.meshToVtkPolyData(shapeOnly)
    }

    // add the colours if it is a vertex color
    surface match {
      case Right(colorMesh) => {

        val vtkColors = new vtkUnsignedCharArray()
        vtkColors.SetNumberOfComponents(3)

        // Add the three colors we have created to the array
        for (id <- colorMesh.shape.pointSet.pointIds) {
          val color = colorMesh.color(id)
          vtkColors.InsertNextTuple3((color.r * 255).toShort, (color.g * 255).toShort, (color.b * 255).toShort)
        }
        vtkColors.SetName("RGB")
        vtkPd.GetPointData().SetScalars(vtkColors)

      }
      case _ => {}
    }
    val writer = new vtkPLYWriter()
    writer.SetFileName(file.getAbsolutePath)
    writer.SetArrayName("RGB")
    writer.SetComponent(0)
    writer.SetInputData(vtkPd)
    writer.SetColorModeToDefault()
    writer.SetFileTypeToBinary()
    writer.Update()

    val succOrFailure = if (writer.GetErrorCode() != 0) {
      Failure(new IOException(s"could not write file ${file.getAbsolutePath} (received error code ${writer.GetErrorCode})"))
    } else {
      Success(())
    }
    writer.Delete()
    vtkPd.Delete()
    succOrFailure
  }*/

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

  /* private def writeVTKPdAsSTL(vtkPd: vtkPolyData, file: File): Try[Unit] = {
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
  }*/

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

  private def readVTK(file: File, correctMesh: Boolean = false): Try[TetrahedralMesh[_3D]] = {
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

  /*private def readSTL(file: File, correctMesh: Boolean = false): Try[TriangleMesh[_3D]] = {
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
  }*/

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

  /*private def readPLY(file: File): Try[Either[TriangleMesh[_3D], VertexColorMesh3D]] = {

    // read the ply header to find out if the ply is a textured mesh in ASCII (in which case we return a failure since VTKPLYReader Update() would crash otherwise)
    val breader = new BufferedReader(new FileReader(file))
    val lineIterator = Iterator.continually(breader.readLine())

    val headerLines = lineIterator.dropWhile(_ != "ply").takeWhile(_ != "end_header").toIndexedSeq

    if (headerLines.exists(_.contains("TextureFile")) && headerLines.exists(_.contains("format ascii"))) {
      Failure(new Exception("PLY file seems to be a textured mesh in ASCII format which creates issues with the VTK ply reader. Please convert it to a binary ply or to a vertex color or shape only ply."))
    } else {

      val plyReader = new vtkPLYReader()
      plyReader.SetFileName(file.getAbsolutePath)

      plyReader.Update()

      val errCode = plyReader.GetErrorCode()
      if (errCode != 0) {
        return Failure(new IOException(s"Could not read ply mesh (received VTK error code $errCode"))
      }

      val vtkPd = plyReader.GetOutput()

      val mesh = for {
        meshGeometry <- MeshConversion.vtkPolyDataToTriangleMesh(vtkPd)
      } yield {
        getColorArray(vtkPd) match {
          case Some(("RGBA", colorArray)) => {
            val colors = for (i <- 0 until colorArray.GetNumberOfTuples()) yield {
              val rgba = colorArray.GetTuple4(i)
              RGBA(rgba(0) / 255.0, rgba(1) / 255.0, rgba(2) / 255.0, rgba(3) / 255.0)
            }
            Right(VertexColorMesh3D(meshGeometry, new SurfacePointProperty[RGBA](meshGeometry.triangulation, colors)))
          }
          case Some(("RGB", colorArray)) => {
            val colors = for (i <- 0 until colorArray.GetNumberOfTuples()) yield {
              val rgb = colorArray.GetTuple3(i)
              RGBA(RGB(rgb(0) / 255.0, rgb(1) / 255.0, rgb(2) / 255.0))
            }
            Right(VertexColorMesh3D(meshGeometry, new SurfacePointProperty[RGBA](meshGeometry.triangulation, colors)))
          }
          case Some(_) => Left(meshGeometry)
          case None => Left(meshGeometry)
        }
      }
      plyReader.Delete()
      vtkPd.Delete()

      mesh
    }
  }*/

  /*def readHDF5(file: File): Try[TriangleMesh[_3D]] = {

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
  }*/

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

  /* private def readLineMeshVTK[D: NDSpace: LineMesh.Create: UnstructuredPointsDomain.Create](file: File): Try[LineMesh[D]] = {
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
  }*/

  /* private[this] def writeLineMeshVTK[D: NDSpace](mesh: LineMesh[D], file: File): Try[Unit] = {
    val vtkPd = MeshConversion.lineMeshToVTKPolyData(mesh)
    val err = writeVTKPdasVTK(vtkPd, file)
    vtkPd.Delete()
    err
  }*/

}

