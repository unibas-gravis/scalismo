package smptk.utils

import smptk.mesh.TriangleMesh
import vtk.vtkPolyData
import vtk.vtkCellArray
import vtk.vtkTriangle
import vtk.vtkPoints
import smptk.image.ScalarPixel
import smptk.image.DiscreteScalarImage3D
import ij.ImageStack
import ij.process.FloatProcessor
import ij.ImagePlus
import smptk.image.DiscreteScalarImage2D
import smptk.geometry.ThreeD
import smptk.image.DiscreteScalarImage
import smptk.geometry.TwoD
import vtk.vtkStructuredPoints
import vtk.vtkInformation
import reflect.runtime.universe.{ TypeTag, typeOf }
import scala.NotImplementedError
import scala.reflect.ClassTag
import scala.util.Try
import scala.util.Success
import vtk.vtkCharArray
import vtk.vtkShortArray
import vtk.vtkIntArray
import vtk.vtkLongArray
import vtk.vtkFloatArray
import vtk.vtkDoubleArray

object VTKHelpers {
  val VTK_CHAR = 2
  val VTK_SIGNED_CHAR = 15
  val VTK_UNSIGNED_CHAR = 3
  val VTK_SHORT = 4
  val VTK_INT = 6
  val VTK_UNSIGNED_INT = 6
  val VTK_LONG = 8
  val VTK_UNSIGNED_LONG = 9
  val VTK_FLOAT = 10
  val VTK_DOUBLE = 11
  val VTK_ID_TYPE = 12

  def getVtkScalarType[Pixel: ScalarPixel: TypeTag]: Int = {
    typeOf[Pixel] match {
      case t if t =:= typeOf[Byte] => VTK_CHAR
      case t if t =:= typeOf[Short] => VTK_SHORT
      case t if t =:= typeOf[Int] => VTK_INT
      case t if t =:= typeOf[Long] => VTK_LONG
      case t if t =:= typeOf[Float] => VTK_FLOAT
      case t if t =:= typeOf[Double] => VTK_DOUBLE
      case _ => throw new NotImplementedError("Invalid scalar Pixel Type " + typeOf[Pixel])
    }
  }
  def createVtkDataArray[A: TypeTag](data: Array[A], numComp: Int) = {
    typeOf[A] match {
      case t if t =:= typeOf[Byte] => {
        val a = new vtkCharArray()
        a.SetNumberOfComponents(numComp)
        a.SetNumberOfTuples(data.size / numComp)
        a.SetJavaArray(data.asInstanceOf[Array[Char]])
        a
      }
      case t if t =:= typeOf[Short] => {
        new vtkShortArray()
        val a = new vtkShortArray()
        a.SetNumberOfComponents(numComp)
        a.SetNumberOfTuples(data.size / numComp)
        a.SetJavaArray(data.asInstanceOf[Array[Short]])
        a
      }
      case t if t =:= typeOf[Int] => {
        new vtkIntArray()
        val a = new vtkIntArray()
        a.SetNumberOfComponents(numComp)
        a.SetNumberOfTuples(data.size / numComp)
        a.SetJavaArray(data.asInstanceOf[Array[Int]])
        a
      }
      case t if t =:= typeOf[Long] => {
        val a = new vtkLongArray()
        a.SetNumberOfComponents(numComp)
        a.SetNumberOfTuples(data.size / numComp)
        a.SetJavaArray(data.asInstanceOf[Array[Long]])
        a
      }
      case t if t =:= typeOf[Float] => {
        val a = new vtkFloatArray()
        a.SetNumberOfComponents(numComp)
        a.SetNumberOfTuples(data.size / numComp)
        a.SetJavaArray(data.asInstanceOf[Array[Float]])
        a
      }
      case t if t =:= typeOf[Double] => {
        val a = new vtkDoubleArray()
        a.SetNumberOfComponents(numComp)
        a.SetNumberOfTuples(data.size / numComp)
        a.SetJavaArray(data.asInstanceOf[Array[Double]])
        a
      }
      case _ => throw new NotImplementedError("Invalid scalar Pixel Type " + typeOf[A])
    }
  }
}

object MeshConversion {
  def meshToVTKPolyData(mesh: TriangleMesh): vtkPolyData = {
    val pointDataArray = mesh.points.force.toArray.map(_.data).flatten
    val pd = new vtkPolyData()

    val pointDataArrayVTK = VTKHelpers.createVtkDataArray(pointDataArray, 3)
    val pointsVTK = new vtkPoints
    pointsVTK.SetData(pointDataArrayVTK)
    pd.SetPoints(pointsVTK)
    
    val triangleDataArray = mesh.cells.toArray.map(_.pointIds).flatten
    val cellDataArrayVTK = VTKHelpers.createVtkDataArray(triangleDataArray, 3)
    val polysVTK = new vtkCellArray

    
    val triangles = new vtkCellArray
    triangles.SetNumberOfCells(mesh.cells.size)
    for ((cell, cell_id) <- mesh.cells.zipWithIndex) {
      val triangle = new vtkTriangle()

      triangle.GetPointIds().SetId(0, cell.ptId1);
      triangle.GetPointIds().SetId(1, cell.ptId2);
      triangle.GetPointIds().SetId(2, cell.ptId3);
      triangles.InsertNextCell(triangle);
    }
    triangles.Squeeze()
    pd.SetPolys(triangles)    
    pd
  }
   
}

object ImageConversion {
  def image2DTovtkStructuredPoints[Pixel: ScalarPixel: ClassTag: TypeTag](img: DiscreteScalarImage2D[Pixel]): vtkStructuredPoints = {
    val sp = new vtkStructuredPoints()
    val domain = img.domain
    sp.SetDimensions(domain.size(0), domain.size(1), 1)
    sp.SetOrigin(domain.origin(0), domain.origin(1), 0)
    sp.SetSpacing(domain.spacing(0), domain.spacing(1), 0)    
    
    val info = new vtkInformation() // TODO check what to do with the info
    sp.SetNumberOfScalarComponents(1, info)
    sp.SetScalarType(VTKHelpers.getVtkScalarType[Pixel], info)

    val dataArray = VTKHelpers.createVtkDataArray(img.pixelValues.toArray, 1)
    sp.GetPointData().SetScalars(dataArray)
    sp
  }

  def image3DTovtkStructuredPoints[Pixel: ScalarPixel: ClassTag: TypeTag](img: DiscreteScalarImage3D[Pixel]): vtkStructuredPoints = {
    val sp = new vtkStructuredPoints()
    val domain = img.domain
    sp.SetDimensions(domain.size(0), domain.size(1), domain.size(2))
    sp.SetOrigin(domain.origin(0), domain.origin(1), domain.origin(2))
    sp.SetSpacing(domain.spacing(0), domain.spacing(1), domain.spacing(2))        
    val info = new vtkInformation() // TODO check what to do with the info
    sp.SetNumberOfScalarComponents(1, info)
    sp.SetScalarType(VTKHelpers.getVtkScalarType[Pixel], info)
    val dataArray = VTKHelpers.createVtkDataArray(img.pixelValues.toArray, 1)
    sp.GetPointData().SetScalars(dataArray)
    sp
  }

  def image3DToImageJImagePlus[Pixel: ScalarPixel](img: DiscreteScalarImage[ThreeD, Pixel]) = {
    val pixelConv = implicitly[ScalarPixel[Pixel]]
    val domain = img.domain
    val (width, height, size) = (domain.size(0), domain.size(1), domain.size(2))

    // 	Create 3x3x3 3D stack and fill it with garbage  
    val stack = new ImageStack(width, height)

    val pixelValues = img.pixelValues.map(pixelConv.toFloat(_))
    for (slice <- 0 until size) {
      val startInd = slice * (width * height)
      val endInd = (slice + 1) * (width * height)
      val pixelForSlice = pixelValues.slice(startInd, endInd).toArray
      val bp = new FloatProcessor(width, height, pixelForSlice)
      stack.addSlice(bp)

    }
    new ImagePlus("3D image", stack)
  }

  def image2DToImageJImagePlus[Pixel: ScalarPixel](img: DiscreteScalarImage[TwoD, Pixel]) = {
    val pixelConv = implicitly[ScalarPixel[Pixel]]
    val domain = img.domain
    val bp = new FloatProcessor(domain.size(0), domain.size(1), img.pixelValues.map(pixelConv.toFloat(_)).toArray)
    new ImagePlus("2D image", bp)
  }
}