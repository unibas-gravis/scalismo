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

import scalismo.common._
import scalismo.geometry._
import scalismo.image.{DiscreteImageDomain, DiscreteScalarImage}
import scalismo.io.ImageIO
import scalismo.mesh._
import scalismo.tetramesh.{TetrahedralCell, TetrahedralList, TetrahedralMesh, TetrahedralMesh3D}
import spire.math.{UByte, UInt, ULong, UShort}
import vtk._

import scala.reflect.ClassTag
import scala.reflect.runtime.universe.{TypeTag, typeOf}
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

  def vtkDataArrayToScalarArray[A: TypeTag: ClassTag: Scalar](vtkType: Int, arrayVTK: vtkDataArray): Try[ScalarArray[A]] = Try {
    val scalar = Scalar[A]
    vtkType match {
      // simple cases, no magic needed
      case VTK_SHORT =>
        val p = arrayVTK.asInstanceOf[vtkShortArray].GetJavaArray()
        Scalar.ShortIsScalar.createArray(p).map((s: Short) => scalar.fromShort(s))
      case VTK_INT =>
        val p = arrayVTK.asInstanceOf[vtkIntArray].GetJavaArray()
        Scalar.IntIsScalar.createArray(p).map((i: Int) => scalar.fromInt(i))
      case VTK_FLOAT =>
        val p = arrayVTK.asInstanceOf[vtkFloatArray].GetJavaArray()
        Scalar.FloatIsScalar.createArray(p).map((f: Float) => scalar.fromFloat(f))
      case VTK_DOUBLE =>
        val p = arrayVTK.asInstanceOf[vtkDoubleArray].GetJavaArray()
        Scalar.DoubleIsScalar.createArray(p).map((d: Double) => scalar.fromDouble(d))
      // complicated cases, so we're more explicit about what we're doing
      case VTK_CHAR =>
        val in = arrayVTK.asInstanceOf[vtkCharArray].GetJavaArray()
        val out: Array[Byte] = ArrayUtils.fastMap[Char, Byte](in, { c => c.toByte })
        Scalar.ByteIsScalar.createArray(out).map((b: Byte) => scalar.fromByte(b))
      case VTK_SIGNED_CHAR =>
        val in = arrayVTK.asInstanceOf[vtkSignedCharArray]

        // vtkSignedCharArray does not seem to have a GetJavaArray method.
        // We therefore need to copy it manually
        val out: Array[Byte] = new Array[Byte](in.GetNumberOfTuples() * in.GetNumberOfComponents())
        var i = 0
        while (i < out.length) {
          out(i) = in.GetValue(i).toByte
          i += 1
        }
        Scalar.ByteIsScalar.createArray(out).map((b: Byte) => scalar.fromByte(b))
      case VTK_UNSIGNED_CHAR =>
        val in = arrayVTK.asInstanceOf[vtkUnsignedCharArray].GetJavaArray()
        Scalar.UByteIsScalar.createArray(in).map((s: UByte) => scalar.fromShort(s.toShort))
      case VTK_UNSIGNED_SHORT =>
        val in = arrayVTK.asInstanceOf[vtkUnsignedShortArray].GetJavaArray()
        val chars = ArrayUtils.fastMap[Short, Char](in, _.toChar)
        Scalar.UShortIsScalar.createArray(chars).map((s: UShort) => scalar.fromInt(s.toInt))
      case VTK_UNSIGNED_INT =>
        val in = arrayVTK.asInstanceOf[vtkUnsignedIntArray].GetJavaArray()
        Scalar.UIntIsScalar.createArray(in).asInstanceOf[ScalarArray[A]]
      case _ => throw new NotImplementedError("Unsupported Scalar Pixel Type " + typeOf[A])
    }
  }


  def vtkDataArrayToScalarArray[A: TypeTag: ClassTag: Scalar](vtkType: Int, arrayVTK: vtkAbstractArray): Try[ScalarArray[A]] = Try {
    val scalar = Scalar[A]
    vtkType match {
      // simple cases, no magic needed
      case VTK_SHORT =>
        val p = arrayVTK.asInstanceOf[vtkShortArray].GetJavaArray()
        Scalar.ShortIsScalar.createArray(p).map((s: Short) => scalar.fromShort(s))
      case VTK_INT =>
        val p = arrayVTK.asInstanceOf[vtkIntArray].GetJavaArray()
        Scalar.IntIsScalar.createArray(p).map((i: Int) => scalar.fromInt(i))
      case VTK_FLOAT =>
        val p = arrayVTK.asInstanceOf[vtkFloatArray].GetJavaArray()
        Scalar.FloatIsScalar.createArray(p).map((f: Float) => scalar.fromFloat(f))
      case VTK_DOUBLE =>
        val p = arrayVTK.asInstanceOf[vtkDoubleArray].GetJavaArray()
        Scalar.DoubleIsScalar.createArray(p).map((d: Double) => scalar.fromDouble(d))
      // complicated cases, so we're more explicit about what we're doing
      case VTK_CHAR =>
        val in = arrayVTK.asInstanceOf[vtkCharArray].GetJavaArray()
        val out: Array[Byte] = ArrayUtils.fastMap[Char, Byte](in, { c => c.toByte })
        Scalar.ByteIsScalar.createArray(out).map((b: Byte) => scalar.fromByte(b))
      case VTK_SIGNED_CHAR =>
        val in = arrayVTK.asInstanceOf[vtkSignedCharArray]

        // vtkSignedCharArray does not seem to have a GetJavaArray method.
        // We therefore need to copy it manually
        val out: Array[Byte] = new Array[Byte](in.GetNumberOfTuples() * in.GetNumberOfComponents())
        var i = 0
        while (i < out.length) {
          out(i) = in.GetValue(i).toByte
          i += 1
        }
        Scalar.ByteIsScalar.createArray(out).map((b: Byte) => scalar.fromByte(b))
      case VTK_UNSIGNED_CHAR =>
        val in = arrayVTK.asInstanceOf[vtkUnsignedCharArray].GetJavaArray()
        Scalar.UByteIsScalar.createArray(in).map((s: UByte) => scalar.fromShort(s.toShort))
      case VTK_UNSIGNED_SHORT =>
        val in = arrayVTK.asInstanceOf[vtkUnsignedShortArray].GetJavaArray()
        val chars = ArrayUtils.fastMap[Short, Char](in, _.toChar)
        Scalar.UShortIsScalar.createArray(chars).map((s: UShort) => scalar.fromInt(s.toInt))
      case VTK_UNSIGNED_INT =>
        val in = arrayVTK.asInstanceOf[vtkUnsignedIntArray].GetJavaArray()
        Scalar.UIntIsScalar.createArray(in).asInstanceOf[ScalarArray[A]]
      case _ => throw new NotImplementedError("Unsupported Scalar Pixel Type " + typeOf[A])
    }
  }


}





















  object TetraMeshConversion {

  private def vtkUnstructuredGridToTetrahedralMeshCommon(ug: vtkUnstructuredGrid, correctFlag: Boolean = false) = Try {
    val newUg = if (correctFlag) {
      val tetrahedralFilter = new vtkDelaunay3D
      tetrahedralFilter.SetInputData(ug)
      tetrahedralFilter.Update()
      tetrahedralFilter.GetOutput()
    } else ug


    val grids = newUg.GetCells()
    val numGrids = grids.GetNumberOfCells()

    val points = vtkConvertPoints[_3D](newUg)

    val idList = new vtkIdList()
    val cells = for (i <- 0 until numGrids) yield {
      newUg.GetCellPoints(i, idList)
      if (idList.GetNumberOfIds() != 4) {
        throw new Exception("Not a tetrahedral mesh")
      }

      TetrahedralCell(PointId(idList.GetId(0)), PointId(idList.GetId(1)), PointId(idList.GetId(2)), PointId(idList.GetId(3)))
    }
    idList.Delete()
    (points, cells)
  }



  private[scalismo] def vtkConvertPoints[D: NDSpace](ug: vtkUnstructuredGrid): Iterator[Point[D]] = {
    val vtkType = ug.GetPoints().GetDataType()


    val pointsArray = VtkHelpers.vtkDataArrayToScalarArray[Float](vtkType, ug.GetPoints().GetData()) match {
      case Success(data) => data.toArray
      case Failure(t) => {
        // this should actually never happen. The points array can only be float or double, and hence
        // vtkDataArrayToScalarArray should always return success. If not, something very strange is happening,
        // in which case we just throw the exception
        throw t;
      }
    }

    // vtk point are alwyas 3D. Therefore we take all three coordinates out of the array but,
    // if we are in 2D, take only the first 2. Finally, we need to convert them from float to double.
    pointsArray.grouped(3)
      .map(p => Point(p.take(NDSpace[D].dimensionality).toArray.map(_.toDouble)))
  }

  def vtkUnstructuredGridToTetrahedralMesh(ug: vtkUnstructuredGrid): Try[TetrahedralMesh[_3D]] = {
    // TODO currently all data arrays are ignored    
    val cellsPointsOrFailure = vtkUnstructuredGridToTetrahedralMeshCommon(ug)
    cellsPointsOrFailure.map {
      case (points, cells) => TetrahedralMesh3D(UnstructuredPointsDomain(points.toIndexedSeq), TetrahedralList(cells))
    }
  }

  def vtkUnstructuredGridToCorrectedTetrahedralMesh(ug: vtkUnstructuredGrid): Try[TetrahedralMesh[_3D]] = {

    val cellsPointsOrFailure = vtkUnstructuredGridToTetrahedralMeshCommon(ug, correctFlag = true)
    cellsPointsOrFailure.map {
      case (points, cells) =>
        val cellPointIds = cells.flatMap(_.pointIds).distinct
        val oldId2newId = cellPointIds.zipWithIndex.map { case (id, index) => (id, PointId(index)) }.toMap
        val newCells = cells.map(c => TetrahedralCell(oldId2newId(c.ptId1), oldId2newId(c.ptId2), oldId2newId(c.ptId3), oldId2newId(c.ptId4)))
        val oldPoints = points.toIndexedSeq
        val newPoints = cellPointIds.map(id => oldPoints(id.id))
        TetrahedralMesh3D(UnstructuredPointsDomain(newPoints), TetrahedralList(newCells))
    }
  }

  def tetrameshTovtkUnstructuredGrid(tetramesh: TetrahedralMesh[_3D], template: Option[vtkUnstructuredGrid] = None): vtkUnstructuredGrid = {

    val ug = new vtkUnstructuredGrid()



    template match {
      case Some(tpl) =>
        // copy tetrahedrons from template if given; actual points are set unconditionally in code below.
        ug.ShallowCopy(tpl)
      case None =>
        val tetrahedrons = new vtkCellArray
          val vtkcelltyp= new vtkUnsignedCharArray
          val vtkidtyp= new vtkIdTypeArray
        tetrahedrons.SetNumberOfCells(tetramesh.tetrahedralization.tetrahedrons.size)
        tetrahedrons.Initialize()

        vtkcelltyp.Initialize()
        vtkidtyp.Initialize()
        for ((cell, cell_id) <- tetramesh.tetrahedralization.tetrahedrons.zipWithIndex) {

          val tetrahedron = new vtkTetra()
          tetrahedron.GetPointIds().SetId(0, cell.ptId1.id)
          tetrahedron.GetPointIds().SetId(1, cell.ptId2.id)
          tetrahedron.GetPointIds().SetId(2, cell.ptId3.id)
          tetrahedron.GetPointIds().SetId(3, cell.ptId4.id)

          tetrahedrons.InsertNextCell(tetrahedron)
          vtkcelltyp.InsertNextValue(tetrahedron.GetCellType().toChar)
           vtkidtyp.InsertNextValue(cell_id)

        }

        tetrahedrons.Squeeze()
        vtkcelltyp.Squeeze()
        vtkidtyp.Squeeze()
        ug.SetCells(vtkcelltyp,vtkidtyp,tetrahedrons)






    }

    // set points
    val pointDataArray = tetramesh.pointSet.pointSequence.toArray.flatMap(_.toArray)
    val pointDataArrayVTK = VtkHelpers.scalarArrayToVtkDataArray(Scalar.DoubleIsScalar.createArray(pointDataArray), 3)
    val pointsVTK = new vtkPoints
    pointsVTK.SetData(pointDataArrayVTK)
    ug.SetPoints(pointsVTK)
    ug
  }

  /*def scalarMeshFieldToVtkPolyData[S: Scalar: ClassTag: TypeTag](meshData: ScalarMeshField[S]): vtkPolyData = {
    val pd = tetrameshTovtkUnstructuredGrid(meshData.mesh)
    val scalarData = VtkHelpers.scalarArrayToVtkDataArray(meshData.data, 1) // TODO make this more general
    pd.GetPointData().SetScalars(scalarData)
    pd
  }*/

  /*def vtkPolyDataToScalarMeshField[S: Scalar: TypeTag: ClassTag](pd: vtkPolyData): Try[ScalarMeshField[S]] = {
    for {
      mesh <- vtkPolyDataToTriangleMesh(pd)
      scalarData <- VtkHelpers.vtkDataArrayToScalarArray[S](pd.GetPointData().GetScalars().GetDataType(), pd.GetPointData().GetScalars())
    } yield {
      ScalarMeshField(mesh, scalarData)
    }
  }*/

  /*def vtkUnstructuredGridToLineTetraMesh[D: NDSpace: LineMesh.Create: UnstructuredPointsDomain.Create](pd: vtkUnstructuredGrid): Try[LineTetrahedralMesh[D]] = {
    val lines = pd.GetLines()
    val numPolys = lines.GetNumberOfCells()
    val points = vtkConvertPoints[D](pd)
    val idList = new vtkIdList()
    val cellsOrFailure = Try {
      for (i <- 0 until numPolys) yield {
        pd.GetCellPoints(i, idList)
        if (idList.GetNumberOfIds() != 2) {
          throw new scala.Exception("Not a poly line")
        } else {
          LineCell(PointId(idList.GetId(0)), PointId(idList.GetId(1)))
        }
      }
    }
    idList.Delete()

    cellsOrFailure.map(cells => LineMesh(UnstructuredPointsDomain[D](points.toIndexedSeq), LineList(cells)))
  }*/

  /* def lineMeshToVTKPolyData[D: NDSpace](mesh: LineMesh[D], template: Option[vtkPolyData] = None): vtkPolyData = {

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
  }*/

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
      val numPolys = polys.GetNumberOfCells()

      val points = vtkConvertPoints[_3D](newPd)

      val idList = new vtkIdList()
      val cells = for (i <- 0 until numPolys) yield {
        newPd.GetCellPoints(i, idList)
        if (idList.GetNumberOfIds() != 3) {
          throw new Exception("Not a triangle mesh")
        }

        TriangleCell(PointId(idList.GetId(0)), PointId(idList.GetId(1)), PointId(idList.GetId(2)))
      }
      idList.Delete()
      (points, cells)
    }

    private[scalismo] def vtkConvertPoints[D: NDSpace](pd: vtkPolyData): Iterator[Point[D]] = {
      val vtkType = pd.GetPoints().GetDataType()


      val v = pd.GetPoints().GetData()
      val pointsArray = VtkHelpers.vtkDataArrayToScalarArray[Float](vtkType, pd.GetPoints().GetData()) match {
        case Success(data) => data.toArray
        case Failure(t) => {
          // this should actually never happen. The points array can only be float or double, and hence
          // vtkDataArrayToScalarArray should always return success. If not, something very strange is happening,
          // in which case we just throw the exception
          throw t;
        }
      }

      // vtk point are alwyas 3D. Therefore we take all three coordinates out of the array but,
      // if we are in 2D, take only the first 2. Finally, we need to convert them from float to double.
      pointsArray.grouped(3)
        .map(p => Point(p.take(NDSpace[D].dimensionality).toArray.map(_.toDouble)))
    }

    def vtkPolyDataToTriangleMesh(pd: vtkPolyData): Try[TriangleMesh[_3D]] = {
      // TODO currently all data arrays are ignored
      val cellsPointsOrFailure = vtkPolyDataToTriangleMeshCommon(pd)
      cellsPointsOrFailure.map {
        case (points, cells) =>
          TriangleMesh3D(UnstructuredPointsDomain(points.toIndexedSeq), TriangleList(cells))
      }
    }

    def vtkPolyDataToCorrectedTriangleMesh(pd: vtkPolyData): Try[TriangleMesh[_3D]] = {

      val cellsPointsOrFailure = vtkPolyDataToTriangleMeshCommon(pd, correctFlag = true)
      cellsPointsOrFailure.map {
        case (points, cells) =>
          val cellPointIds = cells.flatMap(_.pointIds).distinct
          val oldId2newId = cellPointIds.zipWithIndex.map { case (id, index) => (id, PointId(index)) }.toMap
          val newCells = cells.map(c => TriangleCell(oldId2newId(c.ptId1), oldId2newId(c.ptId2), oldId2newId(c.ptId3)))
          val oldPoints = points.toIndexedSeq
          val newPoints = cellPointIds.map(id => oldPoints(id.id))
          TriangleMesh3D(UnstructuredPointsDomain(newPoints), TriangleList(newCells))
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

    def scalarMeshFieldToVtkPolyData[S: Scalar : ClassTag : TypeTag](meshData: ScalarMeshField[S]): vtkPolyData = {
      val pd = meshToVtkPolyData(meshData.mesh)
      val scalarData = VtkHelpers.scalarArrayToVtkDataArray(meshData.data, 1) // TODO make this more general
      pd.GetPointData().SetScalars(scalarData)
      pd
    }

    def vtkPolyDataToScalarMeshField[S: Scalar : TypeTag : ClassTag](pd: vtkPolyData): Try[ScalarMeshField[S]] = {
      for {
        mesh <- vtkPolyDataToTriangleMesh(pd)
        scalarData <- VtkHelpers.vtkDataArrayToScalarArray[S](pd.GetPointData().GetScalars().GetDataType(), pd.GetPointData().GetScalars())
      } yield {
        ScalarMeshField(mesh, scalarData)
      }
    }

    def vtkPolyDataToLineMesh[D: NDSpace : LineMesh.Create : UnstructuredPointsDomain.Create](pd: vtkPolyData): Try[LineMesh[D]] = {
      val lines = pd.GetLines()
      val numPolys = lines.GetNumberOfCells()
      val points = vtkConvertPoints[D](pd)
      val idList = new vtkIdList()
      val cellsOrFailure = Try {
        for (i <- 0 until numPolys) yield {
          pd.GetCellPoints(i, idList)
          if (idList.GetNumberOfIds() != 2) {
            throw new scala.Exception("Not a poly line")
          } else {
            LineCell(PointId(idList.GetId(0)), PointId(idList.GetId(1)))
          }
        }
      }
      idList.Delete()

      cellsOrFailure.map(cells => LineMesh(UnstructuredPointsDomain[D](points.toIndexedSeq), LineList(cells)))
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


  trait CanConvertToVtk[D] {
    def toVtk[Pixel: Scalar : ClassTag : TypeTag](img: DiscreteScalarImage[D, Pixel]): vtkStructuredPoints = {
      val sp = new vtkStructuredPoints()
      sp.SetNumberOfScalarComponents(1, new vtkInformation())
      val dataArray = VtkHelpers.scalarArrayToVtkDataArray(img.data, 1)
      sp.GetPointData().SetScalars(dataArray)

      // In the case of 3D, this might create a new vtkStructuredPoints data due to image orientation
      val orientedSP = setDomainInfo(img.domain, sp)

      if (typeOf[Pixel] =:= typeOf[Byte]) {
        recastDataToSignedChar(orientedSP)
      }

      orientedSP
    }

    def setDomainInfo(domain: DiscreteImageDomain[D], sp: vtkStructuredPoints): vtkStructuredPoints

    def fromVtk[Pixel: Scalar : TypeTag : ClassTag](sp: vtkImageData): Try[DiscreteScalarImage[D, Pixel]]

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

      override def setDomainInfo(domain: DiscreteImageDomain[_2D], sp: vtkStructuredPoints): vtkStructuredPoints = {
        sp.SetDimensions(domain.size(0), domain.size(1), 1)
        sp.SetOrigin(domain.origin(0), domain.origin(1), 0)
        sp.SetSpacing(domain.spacing(0), domain.spacing(1), 0)
        sp
      }

      override def fromVtk[Pixel: Scalar : TypeTag : ClassTag](sp: vtkImageData): Try[DiscreteScalarImage[_2D, Pixel]] = {
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
        val spacing = EuclideanVector(sp.GetSpacing()(0).toFloat, sp.GetSpacing()(1).toFloat)
        val size = IntVector(sp.GetDimensions()(0), sp.GetDimensions()(1))

        val domain = DiscreteImageDomain[_2D](origin, spacing, size)
        val scalars = sp.GetPointData().GetScalars()

        val pixelArrayOrFailure = VtkHelpers.vtkDataArrayToScalarArray[Pixel](sp.GetScalarType(), scalars)
        pixelArrayOrFailure.map(pixelArray => DiscreteScalarImage(domain, pixelArray))

      }
    }

    implicit object _3DCanConvertToVtk$ extends CanConvertToVtk[_3D] {
      override def setDomainInfo(domain: DiscreteImageDomain[_3D], sp: vtkStructuredPoints): vtkStructuredPoints = {

        // Here depending on the image directions (if read from Nifti, can be anything RAS, ASL, LAS, ..),
        // we need to reslice the image in vtk's voxel ordering that is RAI (which in our LPS world coordinates system
        // means +i <=> +x, +j<=>+y, +k<=>+z)

        sp.SetDimensions(domain.size(0), domain.size(1), domain.size(2))

        val corners = List(
          Point(0, 0, 0), Point(domain.size(0) - 1, 0, 0), Point(0, domain.size(1) - 1, 0), Point(0, 0, domain.size(2) - 1), Point(domain.size(0) - 1, domain.size(1) - 1, 0),
          Point(domain.size(0) - 1, 0, domain.size(2) - 1), Point(0, domain.size(1) - 1, domain.size(2) - 1), Point(domain.size(0) - 1, domain.size(1) - 1, domain.size(2) - 1)
        )
        val cornerImages = corners.map(domain.indexToPhysicalCoordinateTransform)
        val newOriginX = cornerImages.map(p => p(0)).min
        val newOriginY = cornerImages.map(p => p(1)).min
        val newOriginZ = cornerImages.map(p => p(2)).min

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
        reslice.SetInterpolationModeToCubic()

        reslice.SetOutputSpacing(domain.spacing(0), domain.spacing(1), domain.spacing(2))
        reslice.SetOutputOrigin(newOriginX, newOriginY, newOriginZ)

        val newXSpatialSize = cornerImages.map(p => p(0)).max - newOriginX
        val newYSpatialSize = cornerImages.map(p => p(1)).max - newOriginY
        val newZSpatialSize = cornerImages.map(p => p(2)).max - newOriginZ

        val newXExtent = math.round(newXSpatialSize / domain.spacing(0)).toInt
        val newYExtent = math.round(newYSpatialSize / domain.spacing(1)).toInt
        val newZExtent = math.round(newZSpatialSize / domain.spacing(2)).toInt

        reslice.SetOutputExtent(0, newXExtent, 0, newYExtent, 0, newZExtent)
        val conv = new vtkImageToStructuredPoints()
        conv.SetInputConnection(reslice.GetOutputPort())
        conv.Update()
        conv.GetStructuredPointsOutput()
      }

      override def fromVtk[Pixel: Scalar : TypeTag : ClassTag](sp: vtkImageData): Try[DiscreteScalarImage[_3D, Pixel]] = {
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
        val spacing = EuclideanVector(sp.GetSpacing()(0).toFloat, sp.GetSpacing()(1).toFloat, sp.GetSpacing()(2).toFloat)
        val size = IntVector(sp.GetDimensions()(0), sp.GetDimensions()(1), sp.GetDimensions()(2))

        val domain = DiscreteImageDomain[_3D](origin, spacing, size)
        val scalars = sp.GetPointData().GetScalars()
        val pixelArrayOrFailure = VtkHelpers.vtkDataArrayToScalarArray[Pixel](sp.GetScalarType(), scalars)
        pixelArrayOrFailure.map(pixelArray => DiscreteScalarImage(domain, pixelArray))
      }

    }

  }

  object ImageConversion {

    def imageToVtkStructuredPoints[D: CanConvertToVtk, Pixel: Scalar : ClassTag : TypeTag](img: DiscreteScalarImage[D, Pixel]): vtkStructuredPoints = {
      implicitly[CanConvertToVtk[D]].toVtk(img)
    }

    def vtkStructuredPointsToScalarImage[D: CanConvertToVtk, Pixel: Scalar : TypeTag : ClassTag](sp: vtkImageData): Try[DiscreteScalarImage[D, Pixel]] = {
      implicitly[CanConvertToVtk[D]].fromVtk(sp)
    }
  }

