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

import scalismo.common.{
  DiscreteField,
  PointId,
  PrimitiveScalarArray,
  Scalar,
  ScalarArray,
  UnstructuredPoints,
  ValueClassScalarArray
}
import scalismo.common
import scalismo.common.DiscreteField.{ScalarMeshField, ScalarVolumeMeshField}
import scalismo.common.interpolation.BSplineImageInterpolator3D
import scalismo.geometry.{_2D, _3D, EuclideanVector, IntVector2D, IntVector3D, NDSpace, Point}
import scalismo.image.{DiscreteImage, DiscreteImageDomain, DiscreteImageDomain3D, StructuredPoints}
import scalismo.io.{ImageIO, ScalarDataType}
import scalismo.mesh.{
  LineCell,
  LineList,
  LineMesh,
  TetrahedralCell,
  TetrahedralList,
  TetrahedralMesh,
  TetrahedralMesh3D,
  TriangleCell,
  TriangleList,
  TriangleMesh,
  TriangleMesh3D
}
import scalismo.utils.ImageConversion.{
  VtkAutomaticInterpolatorSelection,
  VtkCubicInterpolation,
  VtkInterpolationMode,
  VtkLinearInterpolation,
  VtkNearestNeighborInterpolation
}
import spire.math.{UByte, UInt, ULong, UShort}
import vtk.{
  vtkCellArray,
  vtkCharArray,
  vtkDataArray,
  vtkDoubleArray,
  vtkFloatArray,
  vtkIdList,
  vtkImageCast,
  vtkImageData,
  vtkImageReslice,
  vtkImageToStructuredPoints,
  vtkInformation,
  vtkIntArray,
  vtkLandmarkTransform,
  vtkLine,
  vtkLongArray,
  vtkPointSet,
  vtkPoints,
  vtkPolyData,
  vtkShortArray,
  vtkSignedCharArray,
  vtkStructuredPoints,
  vtkTetra,
  vtkTriangle,
  vtkTriangleFilter,
  vtkUnsignedCharArray,
  vtkUnsignedIntArray,
  vtkUnsignedShortArray,
  vtkUnstructuredGrid
}

import scala.reflect.ClassTag
import scala.util.{Failure, Success, Try}

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
  def scalarArrayToVtkDataArray[A: Scalar](data: IndexedSeq[A], numComp: Int): vtkDataArray = {
    def init[T <: vtkDataArray](a: T): T = {
      a.SetNumberOfComponents(numComp)
      a.SetNumberOfTuples(data.length / numComp)
      a
    }

    Scalar[A].scalarType match {
      case Scalar.ShortScalar =>
        val a = init(new vtkShortArray())
        a.SetJavaArray(data.asInstanceOf[PrimitiveScalarArray[Short]].rawData)
        a
      case Scalar.IntScalar =>
        val a = init(new vtkIntArray())
        a.SetJavaArray(data.asInstanceOf[PrimitiveScalarArray[Int]].rawData)
        a
      case Scalar.LongScalar =>
        val a = init(new vtkLongArray())
        a.SetJavaArray(data.asInstanceOf[PrimitiveScalarArray[Long]].rawData)
        a
      case Scalar.FloatScalar =>
        val a = init(new vtkFloatArray())
        a.SetJavaArray(data.asInstanceOf[PrimitiveScalarArray[Float]].rawData)
        a
      case Scalar.DoubleScalar =>
        val a = init(new vtkDoubleArray())
        a.SetJavaArray(data.asInstanceOf[PrimitiveScalarArray[Double]].rawData)
        a
      case Scalar.ByteScalar =>
        /* ATTENTION: The following does NOT produce the correct result!
         * val a = init(new vtkCharArray())
         * a.SetJavaArray(data.asInstanceOf[Array[Byte]].map(_.toChar))
         * Here is the workaround:
         */
        val a = init(new vtkUnsignedCharArray())
        a.SetJavaArray(data.asInstanceOf[PrimitiveScalarArray[Byte]].rawData)
        a
      case Scalar.UByteScalar =>
        val a = init(new vtkUnsignedCharArray())
        //        a.SetJavaArray(data.asInstanceOf[Array[UByte]].map(_.toByte))
        a.SetJavaArray(data.asInstanceOf[ValueClassScalarArray[UByte, Byte]].rawData)
        a
      case Scalar.UShortScalar =>
        val a = init(new vtkUnsignedShortArray())
        val raw = data.asInstanceOf[ValueClassScalarArray[UShort, Char]].rawData
        a.SetJavaArray(ArrayUtils.fastMap[Char, Short](raw, _.toShort))
        a
      case Scalar.UIntScalar =>
        val a = init(new vtkUnsignedIntArray())
        a.SetJavaArray(data.asInstanceOf[ValueClassScalarArray[UInt, Int]].rawData)
        a
    }
  }

  def vtkDataArrayToScalarArray[A: ClassTag: Scalar](vtkType: Int, arrayVTK: vtkDataArray): Try[ScalarArray[A]] = Try {
    val scalar = Scalar[A]

    // There seems to be a bug in VTK, thathe array 1 element to big. To work
    // around this, we compute the number of elements manually and take only this number of elements.
    val numElementsInArray = arrayVTK.GetNumberOfTuples().toInt * arrayVTK.GetNumberOfComponents()

    vtkType match {
      // simple cases, no magic needed
      case VTK_SHORT =>
        val p = arrayVTK.asInstanceOf[vtkShortArray].GetJavaArray().take(numElementsInArray)
        Scalar.ShortIsScalar.createArray(p).map((s: Short) => scalar.fromShort(s))
      case VTK_INT =>
        val p = arrayVTK.asInstanceOf[vtkIntArray].GetJavaArray().take(numElementsInArray)
        Scalar.IntIsScalar.createArray(p).map((i: Int) => scalar.fromInt(i))
      case VTK_FLOAT =>
        val p = arrayVTK.asInstanceOf[vtkFloatArray].GetJavaArray().take(numElementsInArray)
        Scalar.FloatIsScalar.createArray(p).map((f: Float) => scalar.fromFloat(f))
      case VTK_DOUBLE =>
        val p = arrayVTK.asInstanceOf[vtkDoubleArray].GetJavaArray().take(numElementsInArray)
        Scalar.DoubleIsScalar.createArray(p).map((d: Double) => scalar.fromDouble(d))
      // complicated cases, so we're more explicit about what we're doing
      case VTK_CHAR =>
        val in = arrayVTK.asInstanceOf[vtkCharArray] // .GetJavaArray()
        val out: Array[Byte] = new Array[Byte](numElementsInArray)
        var i = 0
        while (i < out.length) {
          out(i) = in.GetValue(i).toByte
          i += 1
        }
        Scalar.ByteIsScalar.createArray(out).map((b: Byte) => scalar.fromByte(b))
      case VTK_SIGNED_CHAR =>
        val in = arrayVTK.asInstanceOf[vtkSignedCharArray]

        // vtkSignedCharArray does not seem to have a GetJavaArray method.
        // We therefore need to copy it manually
        val out: Array[Byte] = new Array[Byte](numElementsInArray)
        var i = 0
        while (i < out.length) {
          out(i) = in.GetValue(i).toByte
          i += 1
        }
        Scalar.ByteIsScalar.createArray(out).map((b: Byte) => scalar.fromByte(b))
      case VTK_UNSIGNED_CHAR =>
        val in = arrayVTK.asInstanceOf[vtkUnsignedCharArray].GetJavaArray().take(numElementsInArray)
        Scalar.UByteIsScalar.createArray(in).map((s: UByte) => scalar.fromShort(s.toShort))
      case VTK_UNSIGNED_SHORT =>
        val in = arrayVTK.asInstanceOf[vtkUnsignedShortArray].GetJavaArray().take(numElementsInArray)
        val chars = ArrayUtils.fastMap[Short, Char](in, _.toChar)
        Scalar.UShortIsScalar.createArray(chars).map((s: UShort) => scalar.fromInt(s.toInt))
      case VTK_UNSIGNED_INT =>
        val in = arrayVTK.asInstanceOf[vtkUnsignedIntArray].GetJavaArray().take(numElementsInArray)
        Scalar.UIntIsScalar.createArray(in).asInstanceOf[ScalarArray[A]]
      case _ => throw new NotImplementedError("Unsupported Scalar Pixel Type " + Scalar[A].scalarType)
    }
  }

}

object TetrahedralMeshConversion {

  private def extractPointsAndCells(ug: vtkUnstructuredGrid) = Try {
    val grids = ug.GetCells()
    val numGrids = grids.GetNumberOfCells().toInt

    val points = CommonConversions.vtkConvertPoints[_3D](ug)

    val idList = new vtkIdList()

    val cells = for (i <- 0 until numGrids) yield {
      ug.GetCellPoints(i, idList)
      if (idList.GetNumberOfIds() != 4) {
        throw new Exception("Not a tetrahedral mesh")
      }

      TetrahedralCell(PointId(idList.GetId(0).toInt),
                      PointId(idList.GetId(1).toInt),
                      PointId(idList.GetId(2).toInt),
                      PointId(idList.GetId(3).toInt)
      )
    }
    idList.Delete()
    (points, cells)
  }

  def vtkUnstructuredGridToTetrahedralMesh(ug: vtkUnstructuredGrid): Try[TetrahedralMesh[_3D]] = {
    val cellsPointsOrFailure = extractPointsAndCells(ug)
    cellsPointsOrFailure.map { case (points, cells) =>
      TetrahedralMesh3D(UnstructuredPoints(points.toIndexedSeq), TetrahedralList(cells))
    }
  }

  def tetrahedralMeshToVTKUnstructuredGrid(tetramesh: TetrahedralMesh[_3D],
                                           template: Option[vtkUnstructuredGrid] = None
  ): vtkUnstructuredGrid = {

    val ug = new vtkUnstructuredGrid()

    template match {
      case Some(tpl) => {
        // copy tetrahedrons from template if given; actual points are set unconditionally in code below.
        ug.ShallowCopy(tpl)
      }
      case None => {
        val VTK_TETRA = new vtk.vtkTetra().GetCellType()
        val tetrahedrons = new vtkCellArray
        tetrahedrons.SetNumberOfCells(tetramesh.tetrahedralization.tetrahedrons.size)
        tetrahedrons.Initialize()

        for ((cell, cell_id) <- tetramesh.tetrahedralization.tetrahedrons.zipWithIndex) {
          val tetrahedron = new vtkTetra()
          tetrahedron.GetPointIds().SetId(0, cell.ptId1.id)
          tetrahedron.GetPointIds().SetId(1, cell.ptId2.id)
          tetrahedron.GetPointIds().SetId(2, cell.ptId3.id)
          tetrahedron.GetPointIds().SetId(3, cell.ptId4.id)

          tetrahedrons.InsertNextCell(tetrahedron)
        }

        tetrahedrons.Squeeze()
        ug.SetCells(VTK_TETRA, tetrahedrons)
      }
    }

    // set points
    val pointDataArray = tetramesh.pointSet.pointSequence.toArray.flatMap(_.toArray)
    val pointDataArrayVTK = VtkHelpers.scalarArrayToVtkDataArray(Scalar.DoubleIsScalar.createArray(pointDataArray), 3)
    val pointsVTK = new vtkPoints
    pointsVTK.SetData(pointDataArrayVTK)
    ug.SetPoints(pointsVTK)
    ug
  }

  def scalarVolumeMeshFieldToVtkUnstructuredGrid[S: Scalar: ClassTag](
    tetraMeshData: ScalarVolumeMeshField[S],
    template: Option[vtkUnstructuredGrid] = None
  ): vtkUnstructuredGrid = {

    val grid = tetrahedralMeshToVTKUnstructuredGrid(tetraMeshData.mesh, template)
    val scalarData = VtkHelpers.scalarArrayToVtkDataArray(ScalarArray(tetraMeshData.data.toArray), 1)
    grid.GetPointData().SetScalars(scalarData)
    grid
  }

  def vtkUnstructuredGridToScalarVolumeMeshField[S: Scalar: ClassTag](
    ug: vtkUnstructuredGrid
  ): Try[ScalarVolumeMeshField[S]] = {
    for {
      mesh <- vtkUnstructuredGridToTetrahedralMesh(ug)
      scalarData <- VtkHelpers
        .vtkDataArrayToScalarArray[S](ug.GetPointData().GetScalars().GetDataType(), ug.GetPointData().GetScalars())
    } yield {
      DiscreteField(mesh, scalarData)
    }
  }

}

object MeshConversion {

  private def vtkPolyDataToTriangleMeshCommon(pd: vtkPolyData, correctFlag: Boolean = false) = Try {

    val newPd = if (correctFlag) {
      val triangleFilter = new vtkTriangleFilter
      triangleFilter.SetInputData(pd)
      triangleFilter.Update()
      triangleFilter.GetOutput()
    } else pd

    val polys = newPd.GetPolys()
    val numPolys = polys.GetNumberOfCells().toInt

    val points = CommonConversions.vtkConvertPoints[_3D](newPd)

    val idList = new vtkIdList()
    val cells = for (i <- 0 until numPolys) yield {
      newPd.GetCellPoints(i, idList)
      if (idList.GetNumberOfIds() != 3) {
        throw new Exception("Not a triangle mesh")
      }

      TriangleCell(PointId(idList.GetId(0).toInt), PointId(idList.GetId(1).toInt), PointId(idList.GetId(2).toInt))
    }
    idList.Delete()
    (points, cells)
  }

  def vtkPolyDataToTriangleMesh(pd: vtkPolyData): Try[TriangleMesh[_3D]] = {
    // TODO currently all data arrays are ignored
    val cellsPointsOrFailure = vtkPolyDataToTriangleMeshCommon(pd)
    cellsPointsOrFailure.map { case (points, cells) =>
      TriangleMesh3D(UnstructuredPoints(points.toIndexedSeq), TriangleList(cells))
    }
  }

  def vtkPolyDataToCorrectedTriangleMesh(pd: vtkPolyData): Try[TriangleMesh[_3D]] = {

    val cellsPointsOrFailure = vtkPolyDataToTriangleMeshCommon(pd, correctFlag = true)
    cellsPointsOrFailure.map { case (points, cells) =>
      val cellPointIds = cells.flatMap(_.pointIds).distinct
      val oldId2newId = cellPointIds.zipWithIndex.map { case (id, index) => (id, PointId(index)) }.toMap
      val newCells = cells.map(c => TriangleCell(oldId2newId(c.ptId1), oldId2newId(c.ptId2), oldId2newId(c.ptId3)))
      val oldPoints = points.toIndexedSeq
      val newPoints = cellPointIds.map(id => oldPoints(id.id))
      TriangleMesh3D(UnstructuredPoints(newPoints), TriangleList(newCells))
    }
  }

  def meshToVtkPolyData(mesh: TriangleMesh[_3D], template: Option[vtkPolyData] = None): vtkPolyData = {

    val pd = new vtkPolyData

    template match {
      case Some(tpl) =>
        // copy triangles from template if given; actual points are set unconditionally in code below.
        pd.ShallowCopy(tpl)
      case None =>
        val triangles = new vtkCellArray
        triangles.SetNumberOfCells(mesh.triangulation.triangles.size)
        triangles.Initialize()
        for ((cell, cell_id) <- mesh.triangulation.triangles.zipWithIndex) {
          val triangle = new vtkTriangle()

          triangle.GetPointIds().SetId(0, cell.ptId1.id)
          triangle.GetPointIds().SetId(1, cell.ptId2.id)
          triangle.GetPointIds().SetId(2, cell.ptId3.id)
          triangles.InsertNextCell(triangle)
        }
        triangles.Squeeze()
        pd.SetPolys(triangles)
    }

    // set points
    val pointDataArray = mesh.pointSet.pointSequence.toArray.flatMap(_.toArray)
    val pointDataArrayVTK = VtkHelpers.scalarArrayToVtkDataArray(Scalar.DoubleIsScalar.createArray(pointDataArray), 3)
    val pointsVTK = new vtkPoints
    pointsVTK.SetData(pointDataArrayVTK)
    pd.SetPoints(pointsVTK)

    pd
  }

  def scalarMeshFieldToVtkPolyData[S: Scalar: ClassTag](meshData: ScalarMeshField[S],
                                                        template: Option[vtkPolyData] = None
  ): vtkPolyData = {
    val pd = meshToVtkPolyData(meshData.mesh, template)
    val scalarData =
      VtkHelpers.scalarArrayToVtkDataArray(ScalarArray(meshData.data.toArray), 1) // TODO make this more general
    pd.GetPointData().SetScalars(scalarData)
    pd
  }

  def vtkPolyDataToScalarMeshField[S: Scalar: ClassTag](pd: vtkPolyData): Try[ScalarMeshField[S]] = {
    for {
      mesh <- vtkPolyDataToTriangleMesh(pd)
      scalarData <- VtkHelpers
        .vtkDataArrayToScalarArray[S](pd.GetPointData().GetScalars().GetDataType(), pd.GetPointData().GetScalars())
    } yield {
      common.ScalarMeshField(mesh, scalarData)
    }
  }

  def vtkPolyDataToLineMesh[D: NDSpace: LineMesh.Create: UnstructuredPoints.Create](
    pd: vtkPolyData
  ): Try[LineMesh[D]] = {
    val lines = pd.GetLines()
    val numPolys = lines.GetNumberOfCells().toInt
    val points = CommonConversions.vtkConvertPoints[D](pd)
    val idList = new vtkIdList()
    val cellsOrFailure = Try {
      for (i <- 0 until numPolys) yield {
        pd.GetCellPoints(i, idList)
        if (idList.GetNumberOfIds() != 2) {
          throw new scala.Exception("Not a poly line")
        } else {
          LineCell(PointId(idList.GetId(0).toInt), PointId(idList.GetId(1).toInt))
        }
      }
    }
    idList.Delete()

    cellsOrFailure.map(cells => LineMesh(UnstructuredPoints[D](points.toIndexedSeq), LineList(cells)))
  }

  def lineMeshToVTKPolyData[D: NDSpace](mesh: LineMesh[D], template: Option[vtkPolyData] = None): vtkPolyData = {

    val pd = new vtkPolyData

    template match {
      case Some(tpl) =>
        // copy triangles from template if given; actual points are set unconditionally in code below.
        pd.ShallowCopy(tpl)
      case None =>
        val lines = new vtkCellArray
        lines.SetNumberOfCells(mesh.lines.size)
        lines.Initialize()
        for ((cell, cell_id) <- mesh.lines.zipWithIndex) {
          val line = new vtkLine()

          line.GetPointIds().SetId(0, cell.ptId1.id)
          line.GetPointIds().SetId(1, cell.ptId2.id)
          lines.InsertNextCell(line)
        }
        lines.Squeeze()
        pd.SetLines(lines)
    }

    // set points. As vtk knows only about 3D poitns, we lift our 2D points to 3D-
    val pointDim = NDSpace[D].dimensionality
    val meshPointsIn3D = if (pointDim == 2) {
      mesh.pointSet.points.map(p => Point(p(0), p(1), 0f))
    } else {
      mesh.pointSet.points.map(p => Point(p(0), p(1), p(2)))
    }
    val pointDataArray = meshPointsIn3D.toIndexedSeq.toArray.flatMap(_.toArray).map(_.toFloat)
    val pointDataArrayVTK = VtkHelpers.scalarArrayToVtkDataArray(Scalar.FloatIsScalar.createArray(pointDataArray), 3)
    val pointsVTK = new vtkPoints
    pointsVTK.SetData(pointDataArrayVTK)
    pd.SetPoints(pointsVTK)

    pd
  }

}

object CommonConversions {

  private[scalismo] def vtkConvertPoints[D: NDSpace](pd: vtkPointSet): Iterator[Point[D]] = {
    val vtkType = pd.GetPoints().GetDataType()

    val pointsArray = VtkHelpers.vtkDataArrayToScalarArray[Float](vtkType, pd.GetPoints().GetData()) match {
      case Success(data) => data.toArray
      // this should actually never happen. The points array can only be float or double, and hence
      // vtkDataArrayToScalarArray should always return success. If not, something very strange is happening,
      // in which case we just throw the exception
      case Failure(t) => throw t
    }

    // vtk point are alwyas 3D. Therefore we take all three coordinates out of the array but,
    // if we are in 2D, take only the first 2. Finally, we need to convert them from float to double.
    pointsArray
      .grouped(3)
      .map(p => Point(p.take(NDSpace[D].dimensionality).map(_.toDouble)))
  }

}

trait CanConvertToVtk[D] {

  def toVtk[Pixel: Scalar: ClassTag](img: DiscreteImage[D, Pixel],
                                     interpolationMode: VtkInterpolationMode
  ): vtkStructuredPoints = {

    val sp = new vtkStructuredPoints()
    sp.SetNumberOfScalarComponents(1, new vtkInformation())
    val dataArray = img.data match {
      case data: ScalarArray[Pixel @unchecked] => VtkHelpers.scalarArrayToVtkDataArray(data, 1)
      case data: IndexedSeq[Pixel @unchecked]  => VtkHelpers.scalarArrayToVtkDataArray(ScalarArray(data.toArray), 1)
    }
    sp.GetPointData().SetScalars(dataArray)

    // In the case of 3D, this might create a new vtkStructuredPoints data due to image orientation
    val orientedSP = setDomainInfo(img.domain.pointSet, sp, interpolationMode)

    if (Scalar[Pixel].scalarType == Scalar.ByteScalar) {
      recastDataToSignedChar(orientedSP)
    }

    orientedSP
  }

  def setDomainInfo(domain: StructuredPoints[D],
                    sp: vtkStructuredPoints,
                    interpolationMode: VtkInterpolationMode
  ): vtkStructuredPoints

  def fromVtk[Pixel: Scalar: ClassTag](sp: vtkImageData): Try[DiscreteImage[D, Pixel]]

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

object CanConvertToVtk {

  implicit object _2DCanConvertToVtk$ extends CanConvertToVtk[_2D] {

    override def setDomainInfo(domain: StructuredPoints[_2D],
                               sp: vtkStructuredPoints,
                               interpolationMode: VtkInterpolationMode
    ): vtkStructuredPoints = {
      sp.SetDimensions(domain.size(0), domain.size(1), 1)
      sp.SetOrigin(domain.origin(0), domain.origin(1), 0)
      sp.SetSpacing(domain.spacing(0), domain.spacing(1), 0)
      sp
    }

    override def fromVtk[Pixel: Scalar: ClassTag](sp: vtkImageData): Try[DiscreteImage[_2D, Pixel]] = {
      if (sp.GetNumberOfScalarComponents() != 1) {
        return Failure(
          new Exception(s"The image is not a scalar image (number of components is ${sp.GetNumberOfScalarComponents()}")
        )
      }

      if (sp.GetDimensions()(2) != 1 && sp.GetDimensions()(1) != 0) {
        return Failure(new Exception(s"The image is a 3D image - require a 2D image"))
      }

      val requiredScalarType = ScalarDataType.fromType[Pixel]
      val spScalarType = ScalarDataType.fromVtkId(sp.GetScalarType())

      if (requiredScalarType != spScalarType) {
        return Failure(new Exception(s"Invalid scalar type (expected $requiredScalarType, found $spScalarType)"))
      }

      val origin = Point(sp.GetOrigin()(0).toFloat, sp.GetOrigin()(1).toFloat)
      val spacing = EuclideanVector(sp.GetSpacing()(0).toFloat, sp.GetSpacing()(1).toFloat)
      val size = IntVector2D(sp.GetDimensions()(0), sp.GetDimensions()(1))

      val domain = DiscreteImageDomain(StructuredPoints[_2D](origin, spacing, size))
      val scalars = sp.GetPointData().GetScalars()

      val pixelArrayOrFailure = VtkHelpers.vtkDataArrayToScalarArray[Pixel](sp.GetScalarType(), scalars)
      pixelArrayOrFailure.map(pixelArray => DiscreteImage(domain, pixelArray))

    }

  }

  implicit object _3DCanConvertToVtk$ extends CanConvertToVtk[_3D] {
    override def setDomainInfo(domain: StructuredPoints[_3D],
                               sp: vtkStructuredPoints,
                               interpolationMode: VtkInterpolationMode
    ): vtkStructuredPoints = {

      // Here depending on the image directions (if read from Nifti, can be anything RAS, ASL, LAS, ..),
      // we need to reslice the image in vtk's voxel ordering that is RAI (which in our LPS world coordinates system
      // means +i <=> +x, +j<=>+y, +k<=>+z)

      sp.SetDimensions(domain.size(0), domain.size(1), domain.size(2))

      val corners = List(
        IntVector3D(0, 0, 0),
        IntVector3D(domain.size(0) - 1, 0, 0),
        IntVector3D(0, domain.size(1) - 1, 0),
        IntVector3D(0, 0, domain.size(2) - 1),
        IntVector3D(domain.size(0) - 1, domain.size(1) - 1, 0),
        IntVector3D(domain.size(0) - 1, 0, domain.size(2) - 1),
        IntVector3D(0, domain.size(1) - 1, domain.size(2) - 1),
        IntVector3D(domain.size(0) - 1, domain.size(1) - 1, domain.size(2) - 1)
      )
      val cornerImages = corners.map(domain.indexToPoint)
      val newOriginX = cornerImages.map(p => p.x).min
      val newOriginY = cornerImages.map(p => p.y).min
      val newOriginZ = cornerImages.map(p => p.z).min

      val vtkSourceCorners = new vtkPoints()
      corners.foreach(c => vtkSourceCorners.InsertNextPoint(c.toArray.map(_.toDouble)))

      val vtkTargetCorners = new vtkPoints()
      cornerImages.foreach(c => vtkTargetCorners.InsertNextPoint(c.toArray.map(_.toDouble)))

      val landmarkTransform = new vtkLandmarkTransform()

      landmarkTransform.SetSourceLandmarks(vtkTargetCorners)
      landmarkTransform.SetTargetLandmarks(vtkSourceCorners)
      landmarkTransform.SetModeToAffine()
      landmarkTransform.Update()

      val reslice = new vtkImageReslice()

      reslice.SetInputData(sp)
      reslice.SetResliceTransform(landmarkTransform)

      interpolationMode match {
        case VtkCubicInterpolation             => reslice.SetInterpolationModeToCubic()
        case VtkLinearInterpolation            => reslice.SetInterpolationModeToLinear()
        case VtkNearestNeighborInterpolation   => reslice.SetInterpolationModeToNearestNeighbor()
        case VtkAutomaticInterpolatorSelection => {}
      }

      reslice.SetOutputSpacing(domain.spacing(0), domain.spacing(1), domain.spacing(2))
      reslice.SetOutputOrigin(newOriginX, newOriginY, newOriginZ)

      val newXSpatialSize = cornerImages.map(p => p.x).max - newOriginX
      val newYSpatialSize = cornerImages.map(p => p.y).max - newOriginY
      val newZSpatialSize = cornerImages.map(p => p.z).max - newOriginZ

      val newXExtent = math.round(newXSpatialSize / domain.spacing(0)).toInt
      val newYExtent = math.round(newYSpatialSize / domain.spacing(1)).toInt
      val newZExtent = math.round(newZSpatialSize / domain.spacing(2)).toInt

      reslice.SetOutputExtent(0, newXExtent, 0, newYExtent, 0, newZExtent)

      val conv = new vtkImageToStructuredPoints()
      conv.SetInputConnection(reslice.GetOutputPort())
      conv.Update()
      conv.GetStructuredPointsOutput()
    }

    override def fromVtk[Pixel: Scalar: ClassTag](sp: vtkImageData): Try[DiscreteImage[_3D, Pixel]] = {
      if (sp.GetNumberOfScalarComponents() != 1) {
        return Failure(
          new Exception(s"The image is not a scalar image (number of components is ${sp.GetNumberOfScalarComponents()}")
        )
      }

      if (sp.GetDimensions()(2) == 1 || sp.GetDimensions()(2) == 0) {
        return Failure(new Exception(s"The image is a 2D image - require a 3D image"))
      }

      val requiredScalarType = ScalarDataType.fromType[Pixel]
      val spScalarType = ScalarDataType.fromVtkId(sp.GetScalarType())

      if (requiredScalarType != spScalarType) {
        return Failure(new Exception(s"Invalid scalar type (expected $requiredScalarType, found $spScalarType)"))
      }

      val origin = Point(sp.GetOrigin()(0).toFloat, sp.GetOrigin()(1).toFloat, sp.GetOrigin()(2).toFloat)
      val spacing = EuclideanVector(sp.GetSpacing()(0).toFloat, sp.GetSpacing()(1).toFloat, sp.GetSpacing()(2).toFloat)
      val size = IntVector3D(sp.GetDimensions()(0), sp.GetDimensions()(1), sp.GetDimensions()(2))

      val domain = DiscreteImageDomain(StructuredPoints[_3D](origin, spacing, size))
      val scalars = sp.GetPointData().GetScalars()
      val pixelArrayOrFailure = VtkHelpers.vtkDataArrayToScalarArray[Pixel](sp.GetScalarType(), scalars)
      pixelArrayOrFailure.map(pixelArray => DiscreteImage(domain, pixelArray))
    }
  }

}

object ImageConversion {

  sealed trait VtkInterpolationMode
  case object VtkCubicInterpolation extends VtkInterpolationMode
  case object VtkNearestNeighborInterpolation extends VtkInterpolationMode
  case object VtkLinearInterpolation extends VtkInterpolationMode
  case object VtkAutomaticInterpolatorSelection extends VtkInterpolationMode

  def imageToVtkStructuredPoints[D: CanConvertToVtk, Pixel: Scalar: ClassTag](
    img: DiscreteImage[D, Pixel],
    interpolationMode: VtkInterpolationMode = VtkAutomaticInterpolatorSelection
  ): vtkStructuredPoints = {

    // If the interpolation model is set to automatic, we apply
    //  a crude heuristic to understand how we need to interpolate. If the number of values is small
    // we are likely dealing with a labelmap and should interpolate using Nearest Neighbor.
    // If the number of values is large, we are dealing with a normal image
    // and should use higher order interpolation schemes.
    val selectedInterpolationMode = interpolationMode match {
      case VtkAutomaticInterpolatorSelection =>
        if (img.values.toSet.size <= 255) {
          ImageConversion.VtkNearestNeighborInterpolation
        } else {
          ImageConversion.VtkCubicInterpolation
        }
      case _ => interpolationMode
    }
    implicitly[CanConvertToVtk[D]].toVtk(img, selectedInterpolationMode)

  }

  def vtkStructuredPointsToScalarImage[D: CanConvertToVtk, Pixel: Scalar: ClassTag](
    sp: vtkImageData
  ): Try[DiscreteImage[D, Pixel]] = {
    implicitly[CanConvertToVtk[D]].fromVtk(sp)
  }
}
