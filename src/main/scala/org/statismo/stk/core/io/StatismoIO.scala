package org.statismo.stk.core.io

import java.io.File
import org.statismo.stk.core.io.StatismoIO.StatismoModelType.StatismoModelType

import scala.util
import scala.util.Try
import breeze.linalg.DenseVector
import breeze.linalg.DenseMatrix
import scala.util.Failure
import scala.util.Success
import scala.reflect.ClassTag
import reflect.runtime.universe.{TypeTag, typeOf}
import org.statismo.stk.core.mesh.TriangleCell
import org.statismo.stk.core.mesh.TriangleMesh
import org.statismo.stk.core.geometry._
import java.util.Calendar
import ncsa.hdf.`object`._
import ncsa.hdf.`object`.h5._
import java.util.List
import java.io.DataOutputStream
import java.io.FileOutputStream
import ncsa.hdf.`object`.h5.H5Group
import org.statismo.stk.core.mesh.TriangleMesh
import java.io.DataInputStream
import java.io.FileInputStream
import org.statismo.stk.core.statisticalmodel.StatisticalMeshModel

object StatismoIO {
  object StatismoModelType extends Enumeration {
    type StatismoModelType = Value
    val  Polygon_Mesh, Unknown = Value

    def fromString(s : String) : Value = {
      s match {
        case "POLYGON_MESH_MODEL" => Polygon_Mesh
        case _ => Unknown
      }
    }
  }

  type ModelCatalog = Seq[CatalogEntry]

  case class CatalogEntry(val name : String, val modelType : StatismoModelType, val modelPath : String)
  object NoCatalogPresentException extends Exception


  /**
   * List all models that are stored in the given hdf5 file.
   */
  def readModelCatalog(file : File) : Try[ModelCatalog] = {
    import scala.collection.JavaConverters._
    val filename = file.getAbsoluteFile

    def flatten[A](xs: Seq[Try[A]]) : Try[Seq[A]] = Try(xs.map(_.get))

    for {
      h5file <- HDF5Utils.openFileForReading(file)
      catalogGroup <- if (h5file.exists("/catalog")) h5file.getGroup("/catalog") else Failure(NoCatalogPresentException)
      modelEntries = for (entryGroupObj <- catalogGroup.getMemberList().asScala.toSeq if (entryGroupObj.isInstanceOf[Group])) yield {
        val entryGroup = entryGroupObj.asInstanceOf[Group]
        readCatalogEntry(h5file, entryGroup)
      }
      modelCatalog <- flatten(modelEntries)
    } yield {
      modelCatalog
    }
  }

  private def readCatalogEntry(h5file : HDF5File, entryGroup : Group) : Try[CatalogEntry] = {
    val name = entryGroup.getName()
    for {
      location <- h5file.readString(entryGroup.getFullName() + "/modelPath")
      modelType <- h5file.readString(entryGroup.getFullName() + "/modelType")
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
  def readStatismoMeshModel(file: File, modelPath : String = "/"): Try[StatisticalMeshModel] = {
    val filename = file.getAbsolutePath()

    def extractOrthonormalPCABasisMatrix(pcaBasisMatrix: DenseMatrix[Float], pcaVarianceVector: DenseVector[Float]): DenseMatrix[Float] = {
      // this is an old statismo format, that has the pcaVariance directly stored in the PCA matrix,
      // i.e. pcaBasis = U * sqrt(lmbda), where U is a matrix of eigenvectors and lmbda the corresponding eigenvalues.
      // We recover U from it.

      val lambdaSqrt = pcaVarianceVector.map(l => math.sqrt(l).toFloat)
      val lambdaSqrtInv = lambdaSqrt.map(l => if (l > 1e-8) (1.0f / l) else 0f)

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
        case _ => {
          h5file.readStringAttribute(s"$modelPath/representer/", "datasetType") match {
            case Success("POLYGON_MESH") => readStandardMeshFromRepresenterGroup(h5file, modelPath)
            case Success(datasetType) => Failure(new Exception(s"can only read model of datasetType POLYGON_MESH. Got $datasetType instead"))
            case Failure(t) => Failure(t)
          }
        }
      }

      meanArray <- h5file.readNDArray[Float](s"$modelPath/model/mean")
      meanVector = DenseVector(meanArray.data)
      pcaBasisArray <- h5file.readNDArray[Float](s"$modelPath/model/pcaBasis")
      majorVersion <-
        if (h5file.exists("/version/majorVersion") ) h5file.readInt("/version/majorVersion")
        else {
          if (representerName == "vtkPolyDataRepresenter" || representerName == "itkMeshRepresenter") Success(0)
          else Failure(new Throwable(s"no entry /version/majorVersion provided in statismo file." ))
        }
      minorVersion <-
        if (h5file.exists("/version/minorVersion")) h5file.readInt("/version/minorVersion")
        else {
          if (representerName == "vtkPolyDataRepresenter" || representerName == "itkMeshRepresenter") Success(8)
          else Failure(new Throwable(s"no entry /version/minorVersion provided in statismo file." ))
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
      // statismo stores the mean as the point position and not as a displaceme
      // ref. we compensate for this
      def flatten(v: IndexedSeq[Point[ThreeD]]) = DenseVector(v.flatten(pt => Array(pt(0), pt(1), pt(2))).toArray)
      val refpointsVec = flatten(mesh.points.toIndexedSeq)
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

  def writeStatismoMeshModel(model: StatisticalMeshModel, file: File, modelPath : String = "/", statismoVersion: StatismoVersion = v090): Try[Unit] = {

    val discretizedMean = model.mesh.points.map(p => p + model.gp.mean(p)).toIndexedSeq.flatten(_.data)
    val pcaVariance = model.gp.eigenPairs.map(p => p._1).toArray
    val pcaBasis = DenseMatrix.zeros[Float](model.mesh.points.size * model.gp.outputDimensionality, model.gp.rank)
    for {
      (point, idx) <- model.mesh.points.toSeq.par.zipWithIndex
      ((lmda, phi), j) <- model.gp.eigenPairs.zipWithIndex
    } {
      pcaBasis(idx * model.gp.outputDimensionality until (idx + 1) * model.gp.outputDimensionality, j) := (phi(point)).toBreezeVector
    }

    if (statismoVersion == v081) {
      // statismo 081 has the variance included in the pcaBasis
      for (i <- 0 until pcaVariance.length) {
        pcaBasis(::, i) *= math.sqrt(pcaVariance(i)).toFloat
      }
    }

    val maybeError = for {
      h5file <- HDF5Utils.createFile(file)
      _ <- h5file.writeArray(s"$modelPath/model/mean", discretizedMean.toArray)
      _ <- h5file.writeArray(s"$modelPath/model/noiseVariance", Array(0f))
      _ <- h5file.writeNDArray(s"$modelPath/model/pcaBasis", NDArray(Array(pcaBasis.rows, pcaBasis.cols), pcaBasis.t.flatten(false).toArray))
      _ <- h5file.writeArray(s"$modelPath/model/pcaVariance", pcaVariance.toArray)
      _ <- h5file.writeString(s"$modelPath/modelinfo/build-time", Calendar.getInstance.getTime.toString)
      group <- h5file.createGroup(s"$modelPath/representer")
      _ <- if (statismoVersion == v090) {
        for {
          _ <- writeRepresenterStatismov090(h5file, group, model, modelPath)
          _ <- h5file.writeInt("/version/majorVersion", 0)
          _ <- h5file.writeInt("/version/minorVersion", 9)

        } yield Success(())
      }
        else
        {
          for {
          _ <- writeRepresenterStatismov081(h5file, group, model, modelPath)
          _ <- h5file.writeInt("/version/majorVersion", 0)
          _ <- h5file.writeInt("/version/minorVersion", 8)
          }
            yield (Success(()))
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

    val cellArray = model.mesh.cells.map(_.ptId1) ++ model.mesh.cells.map(_.ptId2) ++ model.mesh.cells.map(_.ptId3)
    val pts = model.mesh.points.toIndexedSeq.par.map(p => (p.data(0).toDouble, p.data(1).toDouble, p.data(2).toDouble))
    val pointArray = pts.map(_._1.toFloat) ++ pts.map(_._2.toFloat) ++ pts.map(_._3.toFloat)

    for {
      _ <- h5file.writeStringAttribute(group.getFullName, "name", "itkStandardMeshRepresenter")
      _ <- h5file.writeStringAttribute(group.getFullName, "version/majorVersion", "0")
      _ <- h5file.writeStringAttribute(group.getFullName, "version/minorVersion", "9")
      _ <- h5file.writeStringAttribute(group.getFullName, "datasetType", "POLYGON_MESH")

      _ <- h5file.writeNDArray[Int](s"$modelPath/representer/cells", NDArray(Vector(3, model.mesh.cells.size), cellArray.toArray))
      _ <- h5file.writeNDArray[Float](s"$modelPath/representer/points", NDArray(Vector(3, model.mesh.points.size), pointArray.toArray))
    } yield Success(())
  }


  private def writeRepresenterStatismov081(h5file: HDF5File, group: Group, model: StatisticalMeshModel, modelPath : String): Try[Unit] = {

    // we simply store the reference into a vtk file and store the file (the binary data) into the representer

    def refAsByteArray(ref: TriangleMesh): Try[Array[Byte]] = {
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
        val dis = new DataInputStream(new FileInputStream(f));
        dis.readFully(fileData);
        dis.close();
        fileData
      }
    }

    for {
      _ <- h5file.writeStringAttribute(group.getFullName, "name", "itkMeshRepresenter")
      ba <- refAsByteArray(model.mesh)
      _ <- h5file.writeNDArray[Byte](s"$modelPath/representer/reference", NDArray(Vector(ba.length, 1), ba))
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

  private def readStandardMeshFromRepresenterGroup(h5file: HDF5File, modelPath : String): Try[TriangleMesh] = {
    for {
      vertArray <- h5file.readNDArray[Float](s"$modelPath/representer/points").flatMap(vertArray =>
        if (vertArray.dims(0) != 3)
          Failure(new Exception("the representer points are not 3D points"))
        else
          Success(vertArray))
      vertMat = ndArrayToMatrix(vertArray)
      points = for (i <- 0 until vertMat.cols) yield Point3D(vertMat(0, i), vertMat(1, i), vertMat(2, i))
      cellArray <- h5file.readNDArray[Int](s"$modelPath/representer/cells").flatMap(cellArray =>
        if (cellArray.dims(0) != 3)
          Failure(new Exception("the representer cells are not triangles"))
        else
          Success(cellArray))
      cellMat = ndArrayToMatrix(cellArray)
      cells = for (i <- 0 until cellMat.cols) yield (TriangleCell(cellMat(0, i), cellMat(1, i), cellMat(2, i)))
      cellArray <- h5file.readNDArray[Int](s"$modelPath/representer/cells")
    } yield TriangleMesh(points, cells)
  }

  /*
   * reads the reference (a vtk file), which is stored as a byte array in the hdf5 file)
   */
  private def readVTKMeshFromRepresenterGroup(h5file: HDF5File, modelPath : String): Try[TriangleMesh] = {
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