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
package scalismo.utils

import scalismo.common.{ ValueClassScalarArray, PrimitiveScalarArray, ScalarArray, Scalar }
import scalismo.geometry._
import scalismo.image.{ DiscreteImageDomain, DiscreteScalarImage }
import scalismo.io.ImageIO
import scalismo.mesh.{ ScalarMeshField, TriangleCell, TriangleMesh }
import spire.math.{ UByte, UInt, ULong, UShort }
import vtk._

import scala.reflect.ClassTag
import scala.reflect.runtime.universe.{ TypeTag, typeOf }
import scala.util.{ Failure, Try }

object VtkHelpers {

  val VTK_CHAR = 2
  val VTK_SIGNED_CHAR = 15
  val VTK_UNSIGNED_CHAR = 3
  val VTK_SHORT = 4
  val VTK_UNSIGNED_SHORT = 5
  val VTK_INT = 6
  val VTK_UNSIGNED_INT = 7
  val VTK_LONG = 8
  val VTK_UNSIGNED_LONG = 9
  val VTK_FLOAT = 10
  val VTK_DOUBLE = 11
  val VTK_ID_TYPE = 12

  // ATTENTION: Writing out (signed) bytes using vtkCharArray seems to be broken in VTK, so we need to work around it.
  // We do this by writing the bytes into a vtkUnsignedCharArray first, then converting the scalar data.
  // This conversion must take place on the vtkStructuredPoints object containing the data, so we leave it to the caller of this method.
  def scalarArrayToVtkDataArray[A: TypeTag](data: ScalarArray[A], numComp: Int): vtkDataArray = {
    def init[T <: vtkDataArray](a: T): T = {
      a.SetNumberOfComponents(numComp)
      a.SetNumberOfTuples(data.length / numComp)
      a
    }

    typeOf[A] match {
      case t if t =:= typeOf[Short] =>
        val a = init(new vtkShortArray())
        a.SetJavaArray(data.asInstanceOf[PrimitiveScalarArray[Short]].rawData)
        a
      case t if t =:= typeOf[Int] =>
        val a = init(new vtkIntArray())
        a.SetJavaArray(data.asInstanceOf[PrimitiveScalarArray[Int]].rawData)
        a
      case t if t =:= typeOf[Long] =>
        val a = init(new vtkLongArray())
        a.SetJavaArray(data.asInstanceOf[PrimitiveScalarArray[Long]].rawData)
        a
      case t if t =:= typeOf[Float] =>
        val a = init(new vtkFloatArray())
        a.SetJavaArray(data.asInstanceOf[PrimitiveScalarArray[Float]].rawData)
        a
      case t if t =:= typeOf[Double] =>
        val a = init(new vtkDoubleArray())
        a.SetJavaArray(data.asInstanceOf[PrimitiveScalarArray[Double]].rawData)
        a
      case t if t =:= typeOf[Byte] =>
        /* ATTENTION: The following does NOT produce the correct result!
         * val a = init(new vtkCharArray())
         * a.SetJavaArray(data.asInstanceOf[Array[Byte]].map(_.toChar))
         * Here is the workaround:
         */
        val a = init(new vtkUnsignedCharArray())
        a.SetJavaArray(data.asInstanceOf[PrimitiveScalarArray[Byte]].rawData)
        a
      case t if t =:= typeOf[UByte] =>
        val a = init(new vtkUnsignedCharArray())
        //        a.SetJavaArray(data.asInstanceOf[Array[UByte]].map(_.toByte))
        a.SetJavaArray(data.asInstanceOf[ValueClassScalarArray[UByte, Byte]].rawData)
        a
      case t if t =:= typeOf[UShort] =>
        val a = init(new vtkUnsignedShortArray())
        val raw = data.asInstanceOf[ValueClassScalarArray[UShort, Char]].rawData
        a.SetJavaArray(ArrayUtils.fastMap[Char, Short](raw, _.toShort))
        a
      case t if t =:= typeOf[UInt] =>
        val a = init(new vtkUnsignedIntArray())
        a.SetJavaArray(data.asInstanceOf[ValueClassScalarArray[UInt, Int]].rawData)
        a
      case t if t =:= typeOf[ULong] =>
        val a = init(new vtkUnsignedLongArray())
        a.SetJavaArray(data.asInstanceOf[ValueClassScalarArray[ULong, Long]].rawData)
        a
      case _ => throw new NotImplementedError("Invalid scalar Pixel Type " + typeOf[A])
    }
  }

  def vtkDataArrayToScalarArray[A: TypeTag](vtkType: Int, arrayVTK: vtkDataArray): Try[ScalarArray[A]] = Try {
    vtkType match {
      // simple cases, no magic needed
      case VTK_SHORT =>
        val p = arrayVTK.asInstanceOf[vtkShortArray].GetJavaArray()
        Scalar.ShortIsScalar.createArray(p).asInstanceOf[ScalarArray[A]]
      case VTK_INT =>
        val p = arrayVTK.asInstanceOf[vtkIntArray].GetJavaArray()
        Scalar.IntIsScalar.createArray(p).asInstanceOf[ScalarArray[A]]
      case VTK_LONG =>
        val p = arrayVTK.asInstanceOf[vtkLongArray].GetJavaArray()
        Scalar.LongIsScalar.createArray(p).asInstanceOf[ScalarArray[A]]
      case VTK_FLOAT =>
        val p = arrayVTK.asInstanceOf[vtkFloatArray].GetJavaArray()
        Scalar.FloatIsScalar.createArray(p).asInstanceOf[ScalarArray[A]]
      case VTK_DOUBLE =>
        val p = arrayVTK.asInstanceOf[vtkDoubleArray].GetJavaArray()
        Scalar.DoubleIsScalar.createArray(p).asInstanceOf[ScalarArray[A]]
      // complicated cases, so we're more explicit about what we're doing
      case VTK_CHAR | VTK_SIGNED_CHAR =>
        val in = arrayVTK.asInstanceOf[vtkCharArray].GetJavaArray()
        val out: Array[Byte] = ArrayUtils.fastMap[Char, Byte](in, { c => c.toByte })
        Scalar.ByteIsScalar.createArray(out).asInstanceOf[ScalarArray[A]]
      case VTK_UNSIGNED_CHAR =>
        val in = arrayVTK.asInstanceOf[vtkUnsignedCharArray].GetJavaArray()
        Scalar.UByteIsScalar.createArray(in).asInstanceOf[ScalarArray[A]]
      case VTK_UNSIGNED_SHORT =>
        val in = arrayVTK.asInstanceOf[vtkUnsignedShortArray].GetJavaArray()
        val chars = ArrayUtils.fastMap[Short, Char](in, _.toChar)
        Scalar.UShortIsScalar.createArray(chars).asInstanceOf[ScalarArray[A]]
      case VTK_UNSIGNED_INT =>
        val in = arrayVTK.asInstanceOf[vtkUnsignedIntArray].GetJavaArray()
        Scalar.UIntIsScalar.createArray(in).asInstanceOf[ScalarArray[A]]
      case VTK_UNSIGNED_LONG =>
        val in = arrayVTK.asInstanceOf[vtkUnsignedLongArray].GetJavaArray()
        Scalar.ULongIsScalar.createArray(in).asInstanceOf[ScalarArray[A]]
      case _ => throw new NotImplementedError("Invalid scalar Pixel Type " + typeOf[A])
    }
  }

}

object MeshConversion {

  private def vtkPolyDataToTriangleMeshCommon(pd: vtkPolyData, correctFlag: Boolean = false) = {

    val newPd = if (correctFlag) {
      val triangleFilter = new vtkTriangleFilter
      triangleFilter.SetInputData(pd)
      triangleFilter.Update()
      triangleFilter.GetOutput()
    } else pd

    val polys = newPd.GetPolys()
    val numPolys = polys.GetNumberOfCells()

    val pointsArrayVtk = newPd.GetPoints().GetData().asInstanceOf[vtkFloatArray]
    val pointsArray = pointsArrayVtk.GetJavaArray()
    val points = pointsArray.grouped(3).map(p => Point(p(0), p(1), p(2)))

    Try {
      val idList = new vtkIdList()
      val cells = for (i <- 0 until numPolys) yield {
        newPd.GetCellPoints(i, idList)
        if (idList.GetNumberOfIds() != 3) {
          throw new Exception("Not a triangle mesh")
        }

        TriangleCell(idList.GetId(0), idList.GetId(1), idList.GetId(2))
      }
      idList.Delete()
      (points, cells)
    }
  }

  def vtkPolyDataToTriangleMesh(pd: vtkPolyData): Try[TriangleMesh] = {
    // TODO currently all data arrays are ignored    
    val cellsPointsOrFailure = vtkPolyDataToTriangleMeshCommon(pd)
    cellsPointsOrFailure.map {
      case (points, cells) =>
        TriangleMesh(points.toIndexedSeq, cells)
    }
  }

  def vtkPolyDataToCorrectedTriangleMesh(pd: vtkPolyData): Try[TriangleMesh] = {

    val cellsPointsOrFailure = vtkPolyDataToTriangleMeshCommon(pd, correctFlag = true)
    cellsPointsOrFailure.map {
      case (points, cells) =>
        val cellPointIds = cells.flatMap(_.pointIds).distinct
        val oldId2newId = cellPointIds.zipWithIndex.toMap
        val newCells = cells.map(c => TriangleCell(oldId2newId(c.ptId1), oldId2newId(c.ptId2), oldId2newId(c.ptId3)))
        val oldPoints = points.toIndexedSeq
        val newPoints = cellPointIds.map(oldPoints)
        TriangleMesh(newPoints, newCells)
    }
  }

  def meshToVtkPolyData(mesh: TriangleMesh, template: Option[vtkPolyData] = None): vtkPolyData = {

    val pd = new vtkPolyData

    template match {
      case Some(tpl) =>
        // copy triangles from template if given; actual points are set unconditionally in code below.
        pd.ShallowCopy(tpl)
      case None =>
        val triangles = new vtkCellArray
        triangles.SetNumberOfCells(mesh.cells.size)
        triangles.Initialize()
        for ((cell, cell_id) <- mesh.cells.zipWithIndex) {
          val triangle = new vtkTriangle()

          triangle.GetPointIds().SetId(0, cell.ptId1)
          triangle.GetPointIds().SetId(1, cell.ptId2)
          triangle.GetPointIds().SetId(2, cell.ptId3)
          triangles.InsertNextCell(triangle)
        }
        triangles.Squeeze()
        pd.SetPolys(triangles)
    }

    // set points
    val pointDataArray = mesh.points.toIndexedSeq.toArray.map(_.data).flatten
    val pointDataArrayVTK = VtkHelpers.scalarArrayToVtkDataArray(Scalar.FloatIsScalar.createArray(pointDataArray), 3)
    val pointsVTK = new vtkPoints
    pointsVTK.SetData(pointDataArrayVTK)
    pd.SetPoints(pointsVTK)

    pd
  }

  def scalarMeshFieldToVtkPolyData[S: Scalar: ClassTag: TypeTag](meshData: ScalarMeshField[S]): vtkPolyData = {
    val pd = meshToVtkPolyData(meshData.mesh)
    val scalarData = VtkHelpers.scalarArrayToVtkDataArray(meshData.data, 1) // TODO make this more general
    pd.GetPointData().SetScalars(scalarData)
    pd
  }
}

trait NDImageToVtkOps[D <: Dim] {
  def toVtk[Pixel: Scalar: ClassTag: TypeTag](img: DiscreteScalarImage[D, Pixel]): vtkStructuredPoints = {
    val sp = new vtkStructuredPoints()
    sp.SetNumberOfScalarComponents(1, new vtkInformation())

    val dataArray = VtkHelpers.scalarArrayToVtkDataArray(img.data, 1)
    sp.GetPointData().SetScalars(dataArray)

    setDomainInfo(img.domain, sp)

    // for (signed) byte data, we need to work around a bug in VTK
    if (typeOf[Pixel] =:= typeOf[Byte]) {
      recastDataToSignedChar(sp)
    }

    sp
  }

  def setDomainInfo(domain: DiscreteImageDomain[D], sp: vtkStructuredPoints): Unit

  def fromVtk[Pixel: Scalar: TypeTag: ClassTag](sp: vtkImageData): Try[DiscreteScalarImage[D, Pixel]]

  // ATTENTION: Writing out (signed) bytes using vtkCharArray seems to be broken in VTK, so we need to work around it.
  // We do this by writing the bytes into a vtkUnsignedCharArray first, then converting the scalar data.
  // Also see VtkHelpers.javaArrayToVtkDataArray().
  private def recastDataToSignedChar(sp: vtkStructuredPoints): Unit = {
    val cast = new vtkImageCast
    cast.SetInputData(sp)
    cast.SetOutputScalarTypeToChar()
    cast.Update()
    val csp = cast.GetOutput()
    sp.GetPointData().GetScalars().Delete()
    sp.GetPointData().SetScalars(csp.GetPointData().GetScalars())
    cast.Delete()
    csp.Delete()
  }
}

object NDImageToVtkOps {

  implicit object _2DNDImageToVtkOps extends NDImageToVtkOps[_2D] {

    override def setDomainInfo(domain: DiscreteImageDomain[_2D], sp: vtkStructuredPoints): Unit = {
      sp.SetDimensions(domain.size(0), domain.size(1), 1)
      sp.SetOrigin(domain.origin(0), domain.origin(1), 0)
      sp.SetSpacing(domain.spacing(0), domain.spacing(1), 0)
    }

    override def fromVtk[Pixel: Scalar: TypeTag: ClassTag](sp: vtkImageData): Try[DiscreteScalarImage[_2D, Pixel]] = {
      if (sp.GetNumberOfScalarComponents() != 1) {
        return Failure(new Exception(s"The image is not a scalar image (number of components is ${sp.GetNumberOfScalarComponents()}"))
      }

      if (sp.GetDimensions()(2) != 1 && sp.GetDimensions()(1) != 0) {
        return Failure(new Exception(s"The image is a 3D image - require a 2D image"))
      }

      val requiredScalarType = ImageIO.ScalarType.fromType[Pixel]
      val spScalarType = ImageIO.ScalarType.fromVtkId(sp.GetScalarType())

      if (requiredScalarType != spScalarType) {
        return Failure(new Exception(s"Invalid scalar type (expected $requiredScalarType, found $spScalarType)"))
      }

      val origin = Point(sp.GetOrigin()(0).toFloat, sp.GetOrigin()(1).toFloat)
      val spacing = Vector(sp.GetSpacing()(0).toFloat, sp.GetSpacing()(1).toFloat)
      val size = Index(sp.GetDimensions()(0), sp.GetDimensions()(1))

      val domain = DiscreteImageDomain[_2D](origin, spacing, size)
      val scalars = sp.GetPointData().GetScalars()

      val pixelArrayOrFailure = VtkHelpers.vtkDataArrayToScalarArray[Pixel](sp.GetScalarType(), scalars)
      pixelArrayOrFailure.map(pixelArray => DiscreteScalarImage(domain, pixelArray))

    }
  }

  implicit object _3DNDImageToVtkOps extends NDImageToVtkOps[_3D] {
    override def setDomainInfo(domain: DiscreteImageDomain[_3D], sp: vtkStructuredPoints): Unit = {
      sp.SetDimensions(domain.size(0), domain.size(1), domain.size(2))
      sp.SetOrigin(domain.origin(0), domain.origin(1), domain.origin(2))
      sp.SetSpacing(domain.spacing(0), domain.spacing(1), domain.spacing(2))
    }

    override def fromVtk[Pixel: Scalar: TypeTag: ClassTag](sp: vtkImageData): Try[DiscreteScalarImage[_3D, Pixel]] = {
      if (sp.GetNumberOfScalarComponents() != 1) {
        return Failure(new Exception(s"The image is not a scalar image (number of components is ${sp.GetNumberOfScalarComponents()}"))
      }

      if (sp.GetDimensions()(2) == 1 || sp.GetDimensions()(2) == 0) {
        return Failure(new Exception(s"The image is a 2D image - require a 3D image"))
      }

      val requiredScalarType = ImageIO.ScalarType.fromType[Pixel]
      val spScalarType = ImageIO.ScalarType.fromVtkId(sp.GetScalarType())

      if (requiredScalarType != spScalarType) {
        return Failure(new Exception(s"Invalid scalar type (expected $requiredScalarType, found $spScalarType)"))
      }

      val origin = Point(sp.GetOrigin()(0).toFloat, sp.GetOrigin()(1).toFloat, sp.GetOrigin()(2).toFloat)
      val spacing = Vector(sp.GetSpacing()(0).toFloat, sp.GetSpacing()(1).toFloat, sp.GetSpacing()(2).toFloat)
      val size = Index(sp.GetDimensions()(0), sp.GetDimensions()(1), sp.GetDimensions()(2))

      val domain = DiscreteImageDomain[_3D](origin, spacing, size)
      val scalars = sp.GetPointData().GetScalars()
      val pixelArrayOrFailure = VtkHelpers.vtkDataArrayToScalarArray[Pixel](sp.GetScalarType(), scalars)
      pixelArrayOrFailure.map(pixelArray => DiscreteScalarImage(domain, pixelArray))
    }

  }

}

object ImageConversion {

  def imageToVtkStructuredPoints[D <: Dim: NDImageToVtkOps, Pixel: Scalar: ClassTag: TypeTag](img: DiscreteScalarImage[D, Pixel]): vtkStructuredPoints = {
    implicitly[NDImageToVtkOps[D]].toVtk(img)
  }

  def vtkStructuredPointsToScalarImage[D <: Dim: NDImageToVtkOps, Pixel: Scalar: TypeTag: ClassTag](sp: vtkImageData): Try[DiscreteScalarImage[D, Pixel]] = {
    implicitly[NDImageToVtkOps[D]].fromVtk(sp)
  }
}