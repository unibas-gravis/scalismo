package org.statismo.stk.core.utils

import org.statismo.stk.core.mesh.TriangleMesh
import vtk.vtkPolyData
import vtk.vtkCellArray
import vtk.vtkTriangle
import vtk.vtkPoints
import org.statismo.stk.core.image.DiscreteScalarImage3D
import org.statismo.stk.core.image.DiscreteScalarImage2D
import org.statismo.stk.core.geometry.ThreeD
import org.statismo.stk.core.image.DiscreteScalarImage
import org.statismo.stk.core.geometry.TwoD
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
import org.statismo.stk.core.geometry.Point3D
import vtk.vtkIdList
import org.statismo.stk.core.mesh.TriangleCell
import scala.util.Success
import scala.util.Failure
import org.statismo.stk.core.geometry.Index3D
import org.statismo.stk.core.geometry.Vector3D
import org.statismo.stk.core.geometry.Vector2D
import org.statismo.stk.core.geometry.Point2D
import org.statismo.stk.core.geometry.Index2D
import org.statismo.stk.core.image.DiscreteImageDomain2D
import vtk.vtkDataArray
import org.statismo.stk.core.image.DiscreteImageDomain3D
import vtk.vtkImageData
import org.statismo.stk.core.mesh.ScalarMeshData
import org.statismo.stk.core.common.ScalarValue

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

  def getVtkScalarType[Pixel: ScalarValue: TypeTag]: Int = {
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

  def getVTKArrayAsJavaArray[A: TypeTag](vtkType: Int, arrayVTK: vtkDataArray): Try[Array[A]] = {
    vtkType match {
      case VTK_CHAR => {
        Try { arrayVTK.asInstanceOf[vtkCharArray].GetJavaArray().asInstanceOf[Array[A]] }
      }
      case VTK_SHORT => {
        Try { arrayVTK.asInstanceOf[vtkShortArray].GetJavaArray().asInstanceOf[Array[A]] }
      }
      case VTK_INT => {
        Try { arrayVTK.asInstanceOf[vtkIntArray].GetJavaArray().asInstanceOf[Array[A]] }
      }
      case VTK_FLOAT => {
        Try { arrayVTK.asInstanceOf[vtkFloatArray].GetJavaArray().asInstanceOf[Array[A]] }
      }
      case VTK_DOUBLE => {
        Try { arrayVTK.asInstanceOf[vtkDoubleArray].GetJavaArray().asInstanceOf[Array[A]] }
      }
      case _ => throw new NotImplementedError("Invalid scalar Pixel Type " + typeOf[A])
    }
  }
}

object MeshConversion {

  def vtkPolyDataToTriangleMesh(pd: vtkPolyData): Try[TriangleMesh] = {
    val pointsArrayVtk = pd.GetPoints().GetData().asInstanceOf[vtkFloatArray]
    val pointsArray = pointsArrayVtk.GetJavaArray()
    val points = pointsArray.grouped(3).map(p => Point3D(p(0), p(1), p(2)))

    val polys = pd.GetPolys()

    val numPolys = polys.GetNumberOfCells()

    val cellsOrFailure = Try {
      val idList = new vtkIdList()
      val cells = for (i <- 0 until numPolys) yield {
        pd.GetCellPoints(i, idList)
        if (idList.GetNumberOfIds() != 3) {
          throw new Exception("currently only triangle meshes can be read")
        }
        TriangleCell(idList.GetId(0), idList.GetId(1), idList.GetId(2))
      }
      idList.Delete()
      cells
    }
    // TODO currently all data arrays are ignored
    cellsOrFailure.map { cells =>
      TriangleMesh(points.toIndexedSeq, cells)
    }
  }

  def meshToVTKPolyData(mesh: TriangleMesh, template: Option[vtkPolyData] = None): vtkPolyData = {

    val pd = new vtkPolyData()
    
    template match {
      case Some(template) => {
        // copy triangles from template if given; actual points are set unconditionally in code below.
        pd.ShallowCopy(template)
      }
      case None => {
        val triangleDataArray = mesh.cells.toArray.map(_.pointIds).flatten
        val cellDataArrayVTK = VTKHelpers.createVtkDataArray(triangleDataArray, 3)
        val polysVTK = new vtkCellArray

        val triangles = new vtkCellArray
        triangles.SetNumberOfCells(mesh.cells.size)
        triangles.Initialize()
        for ((cell, cell_id) <- mesh.cells.zipWithIndex) {
          val triangle = new vtkTriangle()

          triangle.GetPointIds().SetId(0, cell.ptId1);
          triangle.GetPointIds().SetId(1, cell.ptId2);
          triangle.GetPointIds().SetId(2, cell.ptId3);
          triangles.InsertNextCell(triangle);
        }
        triangles.Squeeze()
        pd.SetPolys(triangles)
      }
    }

    // set points
    val pointDataArray = mesh.points.force.toArray.map(_.data).flatten.map(_.toFloat)
    val pointDataArrayVTK = VTKHelpers.createVtkDataArray(pointDataArray, 3)
    val pointsVTK = new vtkPoints
    pointsVTK.SetData(pointDataArrayVTK)
    pd.SetPoints(pointsVTK)

    pd
  }

  
   def meshDataToVtkPolyData[S : ScalarValue : ClassTag : TypeTag](meshData : ScalarMeshData[S]): vtkPolyData = {
     val pd = meshToVTKPolyData(meshData.mesh)
     val scalarData = VTKHelpers.createVtkDataArray(meshData.values, meshData.valueDimensionality) // TODO make this more general
     pd.GetPointData().SetScalars(scalarData)
     pd
   }

}

object ImageConversion {
  def image2DTovtkStructuredPoints[Pixel: ScalarValue: ClassTag: TypeTag](img: DiscreteScalarImage2D[Pixel]): vtkStructuredPoints = {
    val sp = new vtkStructuredPoints()
    val domain = img.domain
    sp.SetDimensions(domain.size(0), domain.size(1), 1)
    sp.SetOrigin(domain.origin(0), domain.origin(1), 0)
    sp.SetSpacing(domain.spacing(0), domain.spacing(1), 0)

    val info = new vtkInformation() // TODO check what to do with the info
    sp.SetNumberOfScalarComponents(1, info)
    sp.SetScalarType(VTKHelpers.getVtkScalarType[Pixel], info)

    val dataArray = VTKHelpers.createVtkDataArray(img.values.toArray, 1)
    sp.GetPointData().SetScalars(dataArray)
    sp
  }

  def image3DTovtkStructuredPoints[Pixel: ScalarValue: ClassTag: TypeTag](img: DiscreteScalarImage3D[Pixel]): vtkStructuredPoints = {
    val sp = new vtkStructuredPoints()
    val domain = img.domain
    sp.SetDimensions(domain.size(0), domain.size(1), domain.size(2))
    sp.SetOrigin(domain.origin(0), domain.origin(1), domain.origin(2))
    sp.SetSpacing(domain.spacing(0), domain.spacing(1), domain.spacing(2))
    val info = new vtkInformation() // TODO check what to do with the info
    sp.SetNumberOfScalarComponents(1, info)
    sp.SetScalarType(VTKHelpers.getVtkScalarType[Pixel], info)
    val dataArray = VTKHelpers.createVtkDataArray(img.values.toArray, 1)
    sp.GetPointData().SetScalars(dataArray)
    sp
  }

  def vtkStructuredPointsTo3DScalarImage[Pixel: ScalarValue: TypeTag](sp: vtkImageData): Try[DiscreteScalarImage3D[Pixel]] = {
    if (sp.GetNumberOfScalarComponents() != 1) {
      return Failure(new Exception(s"The image is not a scalar image (number of components is ${sp.GetNumberOfScalarComponents()}"))
    }

    if (sp.GetDimensions()(2) == 1 || sp.GetDimensions()(2) == 0) {
      return Failure(new Exception(s"The image is a 2D image - require a 3D image"))
    }

    val requiredScalarType = VTKHelpers.getVtkScalarType[Pixel]
    val spScalarType = sp.GetScalarType()
    if (requiredScalarType != spScalarType) {
      return Failure(new Exception(s"Invalid scalar type ($requiredScalarType != $spScalarType)"))
    }

    val origin = Point3D(sp.GetOrigin()(0).toFloat, sp.GetOrigin()(1).toFloat, sp.GetOrigin()(2).toFloat)
    val spacing = Vector3D(sp.GetSpacing()(0).toFloat, sp.GetSpacing()(1).toFloat, sp.GetSpacing()(2).toFloat)
    val size = Index3D(sp.GetDimensions()(0), sp.GetDimensions()(1), sp.GetDimensions()(2))

    val domain = DiscreteImageDomain3D(origin, spacing, size)
    val scalars = sp.GetPointData().GetScalars()
    val pixelArrayOrFailure = VTKHelpers.getVTKArrayAsJavaArray[Pixel](sp.GetScalarType(), scalars)
    pixelArrayOrFailure.map(pixelArray => DiscreteScalarImage3D(domain, pixelArray))
  }

  def vtkStructuredPointsTo2DScalarImage[Pixel: ScalarValue: TypeTag](sp: vtkStructuredPoints): Try[DiscreteScalarImage2D[Pixel]] = {
    if (sp.GetNumberOfScalarComponents() != 1) {
      return Failure(new Exception(s"The image is not a scalar image (number of components is ${sp.GetNumberOfScalarComponents()}"))
    }

    if (sp.GetDimensions()(2) != 1 && sp.GetDimensions()(2) != 0) {
      return Failure(new Exception(s"The image is a 3D image - require a 2D image"))
    }

    val requiredScalarType = VTKHelpers.getVtkScalarType[Pixel]
    val spScalarType = sp.GetScalarType()
    if (requiredScalarType != spScalarType) {
      return Failure(new Exception(s"Invalid scalar type ($requiredScalarType != $spScalarType)"))
    }

    val origin = Point2D(sp.GetOrigin()(0).toFloat, sp.GetOrigin()(1).toFloat)
    val spacing = Vector2D(sp.GetSpacing()(0).toFloat, sp.GetSpacing()(1).toFloat)
    val size = Index2D(sp.GetDimensions()(0), sp.GetDimensions()(1))

    val domain = DiscreteImageDomain2D(origin, spacing, size)
    val scalars = sp.GetPointData().GetScalars()
    val pixelArrayOrFailure = VTKHelpers.getVTKArrayAsJavaArray[Pixel](sp.GetScalarType(), scalars)
    pixelArrayOrFailure.map(pixelArray => DiscreteScalarImage2D(domain, pixelArray))
  }

  //  def image3DToImageJImagePlus[Pixel: ScalarValue](img: DiscreteScalarImage[ThreeD, Pixel]) = {
  //    val pixelConv = implicitly[ScalarValue[Pixel]]
  //    val domain = img.domain
  //    val (width, height, size) = (domain.size(0), domain.size(1), domain.size(2))
  //
  //    // 	Create 3x3x3 3D stack and fill it with garbage  
  //    val stack = new ImageStack(width, height)
  //
  //    val pixelValues = img.pixelValues.map(pixelConv.toFloat(_))
  //    for (slice <- 0 until size) {
  //      val startInd = slice * (width * height)
  //      val endInd = (slice + 1) * (width * height)
  //      val pixelForSlice = pixelValues.slice(startInd, endInd).toArray
  //      val bp = new FloatProcessor(width, height, pixelForSlice)
  //      stack.addSlice(bp)
  //
  //    }
  //    new ImagePlus("3D image", stack)
  //  }
  //
  //  def image2DToImageJImagePlus[Pixel: ScalarValue](img: DiscreteScalarImage[TwoD, Pixel]) = {
  //    val pixelConv = implicitly[ScalarValue[Pixel]]
  //    val domain = img.domain
  //    val bp = new FloatProcessor(domain.size(0), domain.size(1), img.pixelValues.map(pixelConv.toFloat(_)).toArray)
  //    new ImagePlus("2D image", bp)
  //  }

}