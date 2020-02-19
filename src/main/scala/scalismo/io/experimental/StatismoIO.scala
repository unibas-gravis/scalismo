package scalismo.io.experimental

import java.io.{DataOutputStream, File, FileOutputStream}
import java.util.Calendar

import breeze.linalg.{DenseMatrix, DenseVector}
import ncsa.hdf.`object`.Group
import scalismo.common.{PointId, UnstructuredPointsDomain}
import scalismo.geometry.{_3D, Point}
import scalismo.io.{HDF5File, HDF5Utils, MeshIO, NDArray}
import scalismo.mesh.{TetrahedralCell, TetrahedralList, TetrahedralMesh, TetrahedralMesh3D}
import scalismo.statisticalmodel.experimental.StatisticalVolumeMeshModel

import scala.util.{Failure, Success, Try}

object StatismoIO {

  /**
   * Reads a statistical mesh model from a statismo file
   * @param file The statismo file
   * @param modelPath a path in the hdf5 file where the model is stored
   * @return
   */
  def readStatismoVolumeMeshModel(file: File, modelPath: String = "/"): Try[StatisticalVolumeMeshModel] = {

    val modelOrFailure = for {
      h5file <- HDF5Utils.openFileForReading(file)

      representerName <- h5file.readStringAttribute(s"$modelPath/representer/", "name")
      // read mesh according to type given in representer
      mesh <- representerName match {
        case "itkMeshRepresenter" => readVTKVolumeMeshFromRepresenterGroup(h5file, modelPath)
        case _ =>
          h5file.readStringAttribute(s"$modelPath/representer/", "datasetType") match {
            case Success("POLYGON_MESH") => readStandardVolumeMeshFromRepresenterGroup(h5file, modelPath)
            case Success(datasetType) =>
              Failure(new Exception(s"can only read model of datasetType POLYGON_MESH. Got $datasetType instead"))
            case Failure(t) => Failure(t)
          }
      }

      meanArray <- h5file.readNDArray[Float](s"$modelPath/model/mean")
      meanVector = DenseVector(meanArray.data.map(_.toDouble))
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
      pcaVarianceVector = DenseVector(pcaVarianceArray.data.map(_.toDouble))
      pcaBasisMatrix = ndFloatArrayToDoubleMatrix(pcaBasisArray)
      pcaBasis <- (majorVersion, minorVersion) match {
        case (1, _) => Success(pcaBasisMatrix)
        case (0, 9) => Success(pcaBasisMatrix)
        case (0, 8) =>
          Success(extractOrthonormalPCABasisMatrix(pcaBasisMatrix, pcaVarianceVector)) // an old statismo version
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

      StatisticalVolumeMeshModel(mesh, meanDefVector, pcaVarianceVector, pcaBasis)
    }

    modelOrFailure
  }

  object StatismoVersion extends Enumeration {
    type StatismoVersion = Value
    val v081, v090 = Value
  }

  import StatismoVersion._

  def writeStatismoVolumeMeshModel(model: StatisticalVolumeMeshModel,
                                   file: File,
                                   modelPath: String = "/",
                                   statismoVersion: StatismoVersion = v090): Try[Unit] = {

    val discretizedMean = model.mean.pointSet.points.toIndexedSeq.flatten(_.toArray)
    val variance = model.gp.variance

    val pcaBasis = model.gp.basisMatrix.copy
    if (statismoVersion == v081) {
      // statismo 081 has the variance included in the pcaBasis
      for (i <- 0 until variance.length) {
        pcaBasis(::, i) *= math.sqrt(variance(i))
      }
    }
    val maybeError = for {
      h5file <- HDF5Utils.createFile(file)
      _ <- h5file.writeArray[Float](s"$modelPath/model/mean", discretizedMean.toArray.map(_.toFloat))
      _ <- h5file.writeArray[Float](s"$modelPath/model/noiseVariance", Array(0f))
      _ <- h5file.writeNDArray[Float](s"$modelPath/model/pcaBasis",
                                      NDArray(Array(pcaBasis.rows, pcaBasis.cols).map(_.toLong).toIndexedSeq,
                                              pcaBasis.t.flatten(false).toArray.map(_.toFloat)))
      _ <- h5file.writeArray[Float](s"$modelPath/model/pcaVariance", variance.toArray.map(_.toFloat))
      _ <- h5file.writeString(s"$modelPath/modelinfo/build-time", Calendar.getInstance.getTime.toString)
      group <- h5file.createGroup(s"$modelPath/representer")
      _ <- writeRepresenterStatismov090(h5file, group, model, modelPath)
      _ <- h5file.writeInt("/version/majorVersion", 0)
      _ <- h5file.writeInt("/version/minorVersion", 9)
      _ <- h5file.writeString(s"$modelPath/modelinfo/modelBuilder-0/buildTime", Calendar.getInstance.getTime.toString)
      _ <- h5file.writeString(s"$modelPath/modelinfo/modelBuilder-0/builderName",
                              "This is a useless info. The stkCore did not handle Model builder info at creation time.")
      _ <- h5file.createGroup(s"$modelPath/modelinfo/modelBuilder-0/parameters")
      _ <- h5file.createGroup(s"$modelPath/modelinfo/modelBuilder-0/dataInfo")
      _ <- Try {
        h5file.close()
      }
    } yield ()

    maybeError
  }

  private def writeRepresenterStatismov090(h5file: HDF5File,
                                           group: Group,
                                           model: StatisticalVolumeMeshModel,
                                           modelPath: String): Try[Unit] = {

    val cellArray = model.referenceVolumeMesh.cells.map(_.ptId1.id) ++ model.referenceVolumeMesh.cells
      .map(_.ptId2.id) ++ model.referenceVolumeMesh.cells.map(_.ptId3.id) ++ model.referenceVolumeMesh.cells
      .map(_.ptId4.id)
    val pts = model.referenceVolumeMesh.pointSet.points.toIndexedSeq.par.map(p =>
      (p.toArray(0).toDouble, p.toArray(1).toDouble, p.toArray(2).toDouble)
    )
    val pointArray = pts.map(_._1) ++ pts.map(_._2) ++ pts.map(_._3)

    for {
      _ <- h5file.writeStringAttribute(group.getFullName, "name", "itkStandardMeshRepresenter")
      _ <- h5file.writeStringAttribute(group.getFullName, "version/majorVersion", "0")
      _ <- h5file.writeStringAttribute(group.getFullName, "version/minorVersion", "9")
      _ <- h5file.writeStringAttribute(group.getFullName, "datasetType", "POLYGON_MESH")

      _ <- h5file.writeNDArray[Int](s"$modelPath/representer/cells",
                                    NDArray(IndexedSeq(4, model.referenceVolumeMesh.cells.size), cellArray.toArray))
      _ <- h5file.writeNDArray[Float](
        s"$modelPath/representer/points",
        NDArray(IndexedSeq(3, model.referenceVolumeMesh.pointSet.points.size), pointArray.toArray.map(_.toFloat))
      )
    } yield Success(())
  }

  private def ndFloatArrayToDoubleMatrix(array: NDArray[Float])(implicit dummy: DummyImplicit,
                                                                dummy2: DummyImplicit): DenseMatrix[Double] = {
    // the data in ndarray is stored row-major, but DenseMatrix stores it column major. We therefore
    // do switch dimensions and transpose
    DenseMatrix.create(array.dims(1).toInt, array.dims(0).toInt, array.data.map(_.toDouble)).t
  }

  private def ndIntArrayToIntMatrix(array: NDArray[Int]) = {
    // the data in ndarray is stored row-major, but DenseMatrix stores it column major. We therefore
    // do switch dimensions and transpose

    DenseMatrix.create(array.dims(1).toInt, array.dims(0).toInt, array.data).t
  }

  private def readStandardVolumeMeshFromRepresenterGroup(h5file: HDF5File,
                                                         modelPath: String): Try[TetrahedralMesh[_3D]] = {
    for {
      vertArray <- h5file
        .readNDArray[Float](s"$modelPath/representer/points")
        .flatMap(vertArray =>
          if (vertArray.dims(0) != 3)
            Failure(new Exception("the representer points are not 3D points"))
          else
            Success(vertArray)
        )
      vertMat = ndFloatArrayToDoubleMatrix(vertArray)
      points = for (i <- 0 until vertMat.cols) yield Point(vertMat(0, i), vertMat(1, i), vertMat(2, i))
      cellArray <- h5file
        .readNDArray[Int](s"$modelPath/representer/cells")
        .flatMap(cellArray =>
          if (cellArray.dims(0) != 4)
            Failure(new Exception("the representer cells are not tetrahedrons"))
          else
            Success(cellArray)
        )
      cellMat = ndIntArrayToIntMatrix(cellArray)
      cells = for (i <- 0 until cellMat.cols)
        yield TetrahedralCell(PointId(cellMat(0, i)),
                              PointId(cellMat(1, i)),
                              PointId(cellMat(2, i)),
                              PointId(cellMat(3, i)))
      cellArray <- h5file.readNDArray[Int](s"$modelPath/representer/cells")
    } yield TetrahedralMesh3D(UnstructuredPointsDomain(points), TetrahedralList(cells))
  }

  /*
   * reads the reference (a vtk file), which is stored as a byte array in the hdf5 file)
   */
  private def readVTKVolumeMeshFromRepresenterGroup(h5file: HDF5File, modelPath: String): Try[TetrahedralMesh[_3D]] = {
    for {
      rawdata <- h5file.readNDArray[Byte](s"$modelPath/representer/reference")
      vtkFile <- writeTmpFile(rawdata.data)
      tetrahedralMesh <- MeshIO.readTetrahedralMesh(vtkFile)
    } yield tetrahedralMesh
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

  private def ndArrayFloatToMatrix(array: NDArray[Float]) = {
    // the data in ndarray is stored row-major, but DenseMatrix stores it column major. We therefore
    // do switch dimensions and transpose
    DenseMatrix.create(array.dims(1).toInt, array.dims(0).toInt, array.data).t
  }

  private def ndArrayIntToMatrix(array: NDArray[Int]) = {
    // the data in ndarray is stored row-major, but DenseMatrix stores it column major. We therefore
    // do switch dimensions and transpose
    DenseMatrix.create(array.dims(1).toInt, array.dims(0).toInt, array.data).t
  }

  private def extractOrthonormalPCABasisMatrix(pcaBasisMatrix: DenseMatrix[Double],
                                               pcaVarianceVector: DenseVector[Double]): DenseMatrix[Double] = {
    // this is an old statismo format, that has the pcaVariance directly stored in the PCA matrix,
    // i.e. pcaBasis = U * sqrt(lambda), where U is a matrix of eigenvectors and lambda the corresponding eigenvalues.
    // We recover U from it.

    val lambdaSqrt = pcaVarianceVector.map(l => math.sqrt(l))
    val lambdaSqrtInv = lambdaSqrt.map(l => if (l > 1e-8) 1.0f / l else 0f)

    // The following code is an efficient way to compute: pcaBasisMatrix * breeze.linalg.diag(lambdaSqrtInv)
    // (diag returns densematrix, so the direct computation would be very slow)
    val U = DenseMatrix.zeros[Double](pcaBasisMatrix.rows, pcaBasisMatrix.cols)
    for (i <- 0 until pcaBasisMatrix.cols) {
      U(::, i) := pcaBasisMatrix(::, i) * lambdaSqrtInv(i)
    }
    U
  }

}
