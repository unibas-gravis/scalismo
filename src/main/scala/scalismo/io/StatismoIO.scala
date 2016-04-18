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

import java.io.File

import scalismo.common.{ PointId, UnstructuredPointsDomain }
import scalismo.geometry.{ Point, _3D }
import scalismo.io.StatismoIO.StatismoModelType.StatismoModelType
import scalismo.mesh.{ TriangleCell, TriangleList, TriangleMesh, TriangleMesh3D }
import scalismo.mesh.TriangleMesh._
import scalismo.statisticalmodel.StatisticalMeshModel

import scala.util.Try
import breeze.linalg.DenseVector
import breeze.linalg.DenseMatrix
import scala.util.Failure
import scala.util.Success
import java.util.Calendar
import ncsa.hdf.`object`._
import java.io.DataOutputStream
import java.io.FileOutputStream
import java.io.DataInputStream
import java.io.FileInputStream

object StatismoIO {
  object StatismoModelType extends Enumeration {
    type StatismoModelType = Value
    val Polygon_Mesh, Unknown = Value

    def fromString(s: String): Value = {
      s match {
        case "POLYGON_MESH_MODEL" => Polygon_Mesh
        case _ => Unknown
      }
    }
  }

  type ModelCatalog = Seq[CatalogEntry]

  case class CatalogEntry(name: String, modelType: StatismoModelType, modelPath: String)
  object NoCatalogPresentException extends Exception

  /**
   * List all models that are stored in the given hdf5 file.
   */
  def readModelCatalog(file: File): Try[ModelCatalog] = {
    import scala.collection.JavaConverters._
    val filename = file.getAbsoluteFile

    def flatten[A](xs: Seq[Try[A]]): Try[Seq[A]] = Try(xs.map(_.get))

    for {
      h5file <- HDF5Utils.openFileForReading(file)
      catalogGroup <- if (h5file.exists("/catalog")) h5file.getGroup("/catalog") else Failure(NoCatalogPresentException)
      modelEntries = for (entryGroupObj <- catalogGroup.getMemberList.asScala.toSeq if entryGroupObj.isInstanceOf[Group]) yield {
        val entryGroup = entryGroupObj.asInstanceOf[Group]
        readCatalogEntry(h5file, entryGroup)
      }
      modelCatalog <- flatten(modelEntries)
    } yield {
      modelCatalog
    }
  }

  private def readCatalogEntry(h5file: HDF5File, entryGroup: Group): Try[CatalogEntry] = {
    val name = entryGroup.getName
    for {
      location <- h5file.readString(entryGroup.getFullName + "/modelPath")
      modelType <- h5file.readString(entryGroup.getFullName + "/modelType")
    } yield {
      CatalogEntry(name, StatismoModelType.fromString(modelType), location)
    }
  }

  /**
   * Reads a statistical mesh model from a statismo file
   * @param file The statismo file
   * @param modelPath a path in the hdf5 file where the model is stored
   * @return
   */
  def readStatismoMeshModel(file: File, modelPath: String = "/"): Try[StatisticalMeshModel] = {

    def extractOrthonormalPCABasisMatrix(pcaBasisMatrix: DenseMatrix[Float], pcaVarianceVector: DenseVector[Float]): DenseMatrix[Float] = {
      // this is an old statismo format, that has the pcaVariance directly stored in the PCA matrix,
      // i.e. pcaBasis = U * sqrt(lambda), where U is a matrix of eigenvectors and lambda the corresponding eigenvalues.
      // We recover U from it.

      val lambdaSqrt = pcaVarianceVector.map(l => math.sqrt(l).toFloat)
      val lambdaSqrtInv = lambdaSqrt.map(l => if (l > 1e-8) 1.0f / l else 0f)

      // The following code is an efficient way to compute: pcaBasisMatrix * breeze.linalg.diag(lambdaSqrtInv)
      // (diag returns densematrix, so the direct computation would be very slow)
      val U = DenseMatrix.zeros[Float](pcaBasisMatrix.rows, pcaBasisMatrix.cols)
      for (i <- 0 until pcaBasisMatrix.cols) {
        U(::, i) := pcaBasisMatrix(::, i) * lambdaSqrtInv(i)
      }
      U
    }

    val modelOrFailure = for {
      h5file <- HDF5Utils.openFileForReading(file)

      representerName <- h5file.readStringAttribute(s"$modelPath/representer/", "name")
      // read mesh according to type given in representer
      mesh <- representerName match {
        case "vtkPolyDataRepresenter" => readVTKMeshFromRepresenterGroup(h5file, modelPath)
        case "itkMeshRepresenter" => readVTKMeshFromRepresenterGroup(h5file, modelPath)
        case _ =>
          h5file.readStringAttribute(s"$modelPath/representer/", "datasetType") match {
            case Success("POLYGON_MESH") => readStandardMeshFromRepresenterGroup(h5file, modelPath)
            case Success(datasetType) => Failure(new Exception(s"can only read model of datasetType POLYGON_MESH. Got $datasetType instead"))
            case Failure(t) => Failure(t)
          }
      }

      meanArray <- h5file.readNDArray[Float](s"$modelPath/model/mean")
      meanVector = DenseVector(meanArray.data)
      pcaBasisArray <- h5file.readNDArray[Float](s"$modelPath/model/pcaBasis")
      majorVersion <- if (h5file.exists("/version/majorVersion")) h5file.readInt("/version/majorVersion")
      else {
        if (representerName == "vtkPolyDataRepresenter" || representerName == "itkMeshRepresenter") Success(0)
        else Failure(new Throwable(s"no entry /version/majorVersion provided in statismo file."))
      }
      minorVersion <- if (h5file.exists("/version/minorVersion")) h5file.readInt("/version/minorVersion")
      else {
        if (representerName == "vtkPolyDataRepresenter" || representerName == "itkMeshRepresenter") Success(8)
        else Failure(new Throwable(s"no entry /version/minorVersion provided in statismo file."))
      }
      pcaVarianceArray <- h5file.readNDArray[Float](s"$modelPath/model/pcaVariance")
      pcaVarianceVector = DenseVector(pcaVarianceArray.data)
      pcaBasisMatrix = ndArrayToMatrix(pcaBasisArray)
      pcaBasis <- (majorVersion, minorVersion) match {
        case (1, _) => Success(pcaBasisMatrix)
        case (0, 9) => Success(pcaBasisMatrix)
        case (0, 8) => Success(extractOrthonormalPCABasisMatrix(pcaBasisMatrix, pcaVarianceVector)) // an old statismo version
        case v => Failure(new Throwable(s"Unsupported version ${v._1}.${v._2}"))
      }

      _ <- Try {
        h5file.close()
      }
    } yield {
      // statismo stores the mean as the point position, not as a displacement on the reference.
      def flatten(v: IndexedSeq[Point[_3D]]) = DenseVector(v.flatten(pt => Array(pt(0), pt(1), pt(2))).toArray)
      val refpointsVec = flatten(mesh.pointSet.points.toIndexedSeq)
      val meanDefVector = meanVector - refpointsVec

      StatisticalMeshModel(mesh, meanDefVector, pcaVarianceVector, pcaBasis)
    }

    modelOrFailure
  }

  object StatismoVersion extends Enumeration {
    type StatismoVersion = Value
    val v081, v090 = Value
  }

  import StatismoVersion._

  def writeStatismoMeshModel(model: StatisticalMeshModel, file: File, modelPath: String = "/", statismoVersion: StatismoVersion = v090): Try[Unit] = {

    val discretizedMean = model.mean.pointSet.points.toIndexedSeq.flatten(_.toArray)
    val variance = model.gp.variance

    val pcaBasis = model.gp.basisMatrix.copy
    if (statismoVersion == v081) {
      // statismo 081 has the variance included in the pcaBasis
      for (i <- 0 until variance.length) {
        pcaBasis(::, i) *= math.sqrt(variance(i)).toFloat
      }
    }
    val maybeError = for {
      h5file <- HDF5Utils.createFile(file)
      _ <- h5file.writeArray(s"$modelPath/model/mean", discretizedMean.toArray)
      _ <- h5file.writeArray(s"$modelPath/model/noiseVariance", Array(0f))
      _ <- h5file.writeNDArray(s"$modelPath/model/pcaBasis", NDArray(Array(pcaBasis.rows, pcaBasis.cols), pcaBasis.t.flatten(false).toArray))
      _ <- h5file.writeArray(s"$modelPath/model/pcaVariance", variance.toArray)
      _ <- h5file.writeString(s"$modelPath/modelinfo/build-time", Calendar.getInstance.getTime.toString)
      group <- h5file.createGroup(s"$modelPath/representer")
      _ <- if (statismoVersion == v090) {
        for {
          _ <- writeRepresenterStatismov090(h5file, group, model, modelPath)
          _ <- h5file.writeInt("/version/majorVersion", 0)
          _ <- h5file.writeInt("/version/minorVersion", 9)
        } yield Success(())
      } else {
        for {
          _ <- writeRepresenterStatismov081(h5file, group, model, modelPath)
          _ <- h5file.writeInt("/version/majorVersion", 0)
          _ <- h5file.writeInt("/version/minorVersion", 8)
        } yield Success(())
      }
      _ <- h5file.writeString(s"$modelPath/modelinfo/modelBuilder-0/buildTime", Calendar.getInstance.getTime.toString)
      _ <- h5file.writeString(s"$modelPath/modelinfo/modelBuilder-0/builderName", "This is a useless info. The stkCore did not handle Model builder info at creation time.")
      _ <- h5file.createGroup(s"$modelPath/modelinfo/modelBuilder-0/parameters")
      _ <- h5file.createGroup(s"$modelPath/modelinfo/modelBuilder-0/dataInfo")
      _ <- Try {
        h5file.close()
      }
    } yield ()

    maybeError
  }

  private def writeRepresenterStatismov090(h5file: HDF5File, group: Group, model: StatisticalMeshModel, modelPath: String): Try[Unit] = {

    val cellArray = model.referenceMesh.cells.map(_.ptId1.id) ++ model.referenceMesh.cells.map(_.ptId2.id) ++ model.referenceMesh.cells.map(_.ptId3.id)
    val pts = model.referenceMesh.pointSet.points.toIndexedSeq.par.map(p => (p.toArray(0).toDouble, p.toArray(1).toDouble, p.toArray(2).toDouble))
    val pointArray = pts.map(_._1.toFloat) ++ pts.map(_._2.toFloat) ++ pts.map(_._3.toFloat)

    for {
      _ <- h5file.writeStringAttribute(group.getFullName, "name", "itkStandardMeshRepresenter")
      _ <- h5file.writeStringAttribute(group.getFullName, "version/majorVersion", "0")
      _ <- h5file.writeStringAttribute(group.getFullName, "version/minorVersion", "9")
      _ <- h5file.writeStringAttribute(group.getFullName, "datasetType", "POLYGON_MESH")

      _ <- h5file.writeNDArray[Int](s"$modelPath/representer/cells", NDArray(IndexedSeq(3, model.referenceMesh.cells.size), cellArray.toArray))
      _ <- h5file.writeNDArray[Float](s"$modelPath/representer/points", NDArray(IndexedSeq(3, model.referenceMesh.pointSet.points.size), pointArray.toArray))
    } yield Success(())
  }

  private def writeRepresenterStatismov081(h5file: HDF5File, group: Group, model: StatisticalMeshModel, modelPath: String): Try[Unit] = {

    // we simply store the reference into a vtk file and store the file (the binary data) into the representer

    def refAsByteArray(ref: TriangleMesh[_3D]): Try[Array[Byte]] = {
      val tmpfile = File.createTempFile("temp", ".vtk")
      tmpfile.deleteOnExit()
      for {
        _ <- MeshIO.writeMesh(ref, tmpfile)
        ba <- readFileAsByteArray(tmpfile)
      } yield ba
    }

    def readFileAsByteArray(f: File): Try[Array[Byte]] = {
      Try {
        val fileData = new Array[Byte](f.length().toInt)
        val dis = new DataInputStream(new FileInputStream(f))
        dis.readFully(fileData)
        dis.close()
        fileData
      }
    }

    for {
      _ <- h5file.writeStringAttribute(group.getFullName, "name", "itkMeshRepresenter")
      ba <- refAsByteArray(model.referenceMesh)
      _ <- h5file.writeNDArray[Byte](s"$modelPath/representer/reference", NDArray(IndexedSeq(ba.length, 1), ba))
    } yield Success(())
  }

  private def ndArrayToMatrix(array: NDArray[Float])(implicit dummy: DummyImplicit, dummy2: DummyImplicit) = {
    // the data in ndarray is stored row-major, but DenseMatrix stores it column major. We therefore
    // do switch dimensions and transpose
    DenseMatrix.create(array.dims(1).toInt, array.dims(0).toInt, array.data).t
  }

  private def ndArrayToMatrix(array: NDArray[Double])(implicit dummy: DummyImplicit) = {
    // the data in ndarray is stored row-major, but DenseMatrix stores it column major. We therefore
    // do switch dimensions and transpose
    DenseMatrix.create(array.dims(1).toInt, array.dims(0).toInt, array.data).t
  }

  private def ndArrayToMatrix(array: NDArray[Int]) = {
    // the data in ndarray is stored row-major, but DenseMatrix stores it column major. We therefore
    // do switch dimensions and transpose

    DenseMatrix.create(array.dims(1).toInt, array.dims(0).toInt, array.data).t
  }

  private def readStandardMeshFromRepresenterGroup(h5file: HDF5File, modelPath: String): Try[TriangleMesh[_3D]] = {
    for {
      vertArray <- h5file.readNDArray[Float](s"$modelPath/representer/points").flatMap(vertArray =>
        if (vertArray.dims(0) != 3)
          Failure(new Exception("the representer points are not 3D points"))
        else
          Success(vertArray))
      vertMat = ndArrayToMatrix(vertArray)
      points = for (i <- 0 until vertMat.cols) yield Point(vertMat(0, i), vertMat(1, i), vertMat(2, i))
      cellArray <- h5file.readNDArray[Int](s"$modelPath/representer/cells").flatMap(cellArray =>
        if (cellArray.dims(0) != 3)
          Failure(new Exception("the representer cells are not triangles"))
        else
          Success(cellArray))
      cellMat = ndArrayToMatrix(cellArray)
      cells = for (i <- 0 until cellMat.cols) yield TriangleCell(PointId(cellMat(0, i)), PointId(cellMat(1, i)), PointId(cellMat(2, i)))
      cellArray <- h5file.readNDArray[Int](s"$modelPath/representer/cells")
    } yield TriangleMesh3D(UnstructuredPointsDomain(points), TriangleList(cells))
  }

  /*
   * reads the reference (a vtk file), which is stored as a byte array in the hdf5 file)
   */
  private def readVTKMeshFromRepresenterGroup(h5file: HDF5File, modelPath: String): Try[TriangleMesh[_3D]] = {
    for {
      rawdata <- h5file.readNDArray[Byte](s"$modelPath/representer/reference")
      vtkFile <- writeTmpFile(rawdata.data)
      triangleMesh <- MeshIO.readMesh(vtkFile)
    } yield triangleMesh
  }

  private def writeTmpFile(data: Array[Byte]): Try[File] = {
    val tmpfile = File.createTempFile("temp", ".vtk")
    tmpfile.deleteOnExit()

    Try {
      val stream = new DataOutputStream(new FileOutputStream(tmpfile))
      stream.write(data)
      stream.close()
    } map (_ => tmpfile)
  }

  //  def main(args: Array[String]): Unit = {
  //    org.statismo.stk.core.initialize
  //    val model = readStatismoMeshModel(new File("/tmp/skull-gaussian-50-0.h5")).get
  //    println(StatismoIO.writeStatismoMeshModel(model, new File("/tmp/x.h5"), StatismoVersion.v081))
  //  }
}