package scalismo.io

import java.io._
import java.util.Calendar

import breeze.linalg.{DenseMatrix, DenseVector}
import ncsa.hdf.`object`.Group
import scalismo.common.{DiscreteDomain, DomainWarp, Vectorizer}
import scalismo.geometry._
import scalismo.image.{CreateDiscreteImageDomain, DiscreteImageDomain, StructuredPoints}
import scalismo.io.StatismoIO.StatismoModelType.StatismoModelType
import scalismo.mesh._
import scalismo.statisticalmodel.{DiscreteLowRankGaussianProcess, PointDistributionModel}

import scala.util.{Failure, Success, Try}
import scala.language.higherKinds


object StatismoIO {

  object StatismoModelType extends Enumeration {
    type StatismoModelType = Value
    val Polygon_Mesh, Unknown = Value

    def fromString(s: String): Value = {
      s match {
        case "POLYGON_MESH_MODEL" => Polygon_Mesh
        case _                    => Unknown
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

    def flatten[A](xs: Seq[Try[A]]): Try[Seq[A]] = Try(xs.map(_.get))

    for {
      h5file <- HDF5Utils.openFileForReading(file)
      catalogGroup <- if (h5file.exists("/catalog")) h5file.getGroup("/catalog") else Failure(NoCatalogPresentException)
      modelEntries = for (entryGroupObj <- catalogGroup.getMemberList.asScala.toSeq
                          if entryGroupObj.isInstanceOf[Group]) yield {
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
  def readStatismoPointModel[D: NDSpace, DDomain[D] <: DiscreteDomain[D]](file: File, modelPath: String = "/")
                                                                         (implicit typeHelper: StatismoDomainIO[D, DDomain], canWarp: DomainWarp[D, DDomain], vectorizer: Vectorizer[EuclideanVector[D]]):
  Try[PointDistributionModel[D, DDomain]] = {

    val dim: Int = vectorizer.dim // NDSpace[D].dimensionality


    val modelOrFailure = for {
      h5file <- HDF5Utils.openFileForReading(file)
      pointsMatrix: DenseMatrix[Double] <- readStandardPointsFromRepresenterGroup(h5file, modelPath, dim)

      connectivityMatrix <- readStandardConnectiveityRepresenterGroup(h5file, modelPath)
      meanVector <- readStandardMeanVector(h5file, modelPath)
//      refVector <- readStandardReferenceVector(h5file, modelPath)

      (pcaVarianceVector, pcaBasis) <- readStandardPCAbasis(h5file, modelPath)

      _ <- Try {
        h5file.close()
      }
    } yield {
      // TODO: Below can probably be written a lot nicer.
      val points = for (i <- 0 until pointsMatrix.cols) yield {
        val col: DenseVector[Double] = DenseVector((0 until dim).map(j => pointsMatrix(j,i)).toArray)
        val vec: EuclideanVector[D] = vectorizer.unvectorize(col)
        vec.toPoint
      }
      val mesh: DDomain[D] = typeHelper.createDomainWithCells(connectivityMatrix, points)

      // TODO: Fold to avoid loop over dim
      val refVector: DenseVector[Double] = DenseVector(mesh.pointSet.points.toIndexedSeq.flatMap(p => (0 until dim).map(i => p(i))).toArray)

      val meanDefVector: DenseVector[Double] = meanVector - refVector

      PointDistributionModel[D, DDomain](mesh, meanDefVector, pcaVarianceVector, pcaBasis)
    }

    modelOrFailure
  }

  object StatismoVersion extends Enumeration {
    type StatismoVersion = Value
    val v090 = Value
  }

  import StatismoVersion._

  def writeStatismoPointModel[D: NDSpace, DDomain[D] <: DiscreteDomain[D]](model: PointDistributionModel[D, DDomain],
                                                                           file: File,
                                                                           modelPath: String = "/",
                                                                           statismoVersion: StatismoVersion = v090)(implicit typeHelper: StatismoDomainIO[D, DDomain],  vectorizer: Vectorizer[EuclideanVector[D]]): Try[Unit] = {

    val dim: Int = NDSpace[D].dimensionality

    val discretizedMean = model.mean.pointSet.points.toIndexedSeq.flatten(_.toArray)

    val variance = model.gp.variance

    val pcaBasis = model.gp.basisMatrix.copy
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
      _ <-
        for {
          _ <- writeRepresenterStatismov090(h5file, group, model, modelPath)
          _ <- h5file.writeInt("/version/majorVersion", 0)
          _ <- h5file.writeInt("/version/minorVersion", 9)
        } yield Success(())
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

  private def writeRepresenterStatismov090[D: NDSpace, DDomain[D] <: DiscreteDomain[D]](h5file: HDF5File,
                                                                                        group: Group,
                                                                                        model: PointDistributionModel[D, DDomain],
                                                                                        modelPath: String)(implicit typeHelper: StatismoDomainIO[D, DDomain]): Try[Unit] = {

    val cells = typeHelper.cellsToArray(model.reference)
    val dim: Int = NDSpace[D].dimensionality

    val dv: Array[Float] = (0 until dim).flatMap(i => model.reference.pointSet.points.toIndexedSeq.map(p => p(i))).toArray.map(_.toFloat)
//    val dv2: Array[Float] = model.reference.pointSet.points.toIndexedSeq.flatMap(_.toBreezeVector.data).toArray.map(_.toFloat) // wrong order

    val points: NDArray[Float] = NDArray(
      IndexedSeq(dim, model.reference.pointSet.numberOfPoints), dv
    )

    for {
      _ <- h5file.writeStringAttribute(group.getFullName, "name", "itkStandardMeshRepresenter")
      _ <- h5file.writeStringAttribute(group.getFullName, "version/majorVersion", "0")
      _ <- h5file.writeStringAttribute(group.getFullName, "version/minorVersion", "9")
      _ <- h5file.writeStringAttribute(group.getFullName, "datasetType", "POLYGON_MESH")

      _ <- h5file.writeNDArray[Int](s"$modelPath/representer/cells", cells)
      _ <- h5file.writeNDArray[Float](s"$modelPath/representer/points", points)
    } yield Success(())
  }

  private def ndFloatArrayToDoubleMatrix(array: NDArray[Float])(implicit
                                                                dummy: DummyImplicit,
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

  private def readStandardPCAbasis(h5file: HDF5File, modelPath: String): Try[(DenseVector[Double], DenseMatrix[Double])] = {
    for{
      representerName <- h5file.readStringAttribute(s"$modelPath/representer/", "name")
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
    }
      yield (pcaVarianceVector, pcaBasis)
  }

  // TODO: Combine readStandardPointsFromRepresenterGroup and readStandardReferenceVector - both read the same but returns different
  private def readStandardPointsFromRepresenterGroup(h5file: HDF5File, modelPath: String, dim: Int): Try[DenseMatrix[Double]] = {
    for {
      vertArray <- h5file
        .readNDArray[Float](s"$modelPath/representer/points")
        .flatMap(vertArray =>
          if (vertArray.dims(0) != dim)
            Failure(new Exception(s"the representer points are not in ${dim}D"))
          else
            Success(vertArray)
        )
    }
      yield ndFloatArrayToDoubleMatrix(vertArray)
  }

//  private def readStandardReferenceVector(h5file: HDF5File, modelPath: String): Try[DenseVector[Double]] = {
//    for {
//      pointArray <- h5file.readNDArray[Float](s"$modelPath/representer/points")
//    }
//      yield DenseVector(pointArray.data.map(_.toDouble))
//  }

  private def readStandardMeanVector(h5file: HDF5File, modelPath: String): Try[DenseVector[Double]] = {
    for {
      meanArray <- h5file.readNDArray[Float](s"$modelPath/model/mean")
    }
      yield DenseVector(meanArray.data.map(_.toDouble))
  }

  private def readStandardConnectiveityRepresenterGroup(h5file: HDF5File, modelPath: String): Try[DenseMatrix[Int]] = {
    for{
      cellArray <- h5file
        .readNDArray[Int](s"$modelPath/representer/cells")
    }
      yield ndIntArrayToIntMatrix(cellArray)
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


  //===============================================================
  // Reading and writing of deformation models
  //===============================================================

  /**
   * Writes a GP defined on an image domain with values of type A
   * as a statismo file.
   *
   * @param gp the gaussian process
   * @param file the file to which it is written
   * @param modelPath an optional path into the hdf5 file
   * @tparam D the dimensionality of the domain
   * @tparam A The type of the values of the Gaussian process
   * @return Success of failure
   */
  def writeStatismoImageModel[D: NDSpace, A: Vectorizer](gp: DiscreteLowRankGaussianProcess[D, DiscreteImageDomain, A],
                                                         file: File,
                                                         modelPath: String): Try[Unit] = {

    val discretizedMean = gp.meanVector.map(_.toFloat)
    val variance = gp.variance.map(_.toFloat)

    val pcaBasis = gp.basisMatrix.copy.map(_.toFloat)

    val maybeError = for {
      h5file <- HDF5Utils.createFile(file)
      _ <- h5file.writeArray(s"$modelPath/model/mean", discretizedMean.toArray)
      _ <- h5file.writeArray(s"$modelPath/model/noiseVariance", Array(0f))
      _ <- h5file.writeNDArray(
        s"$modelPath/model/pcaBasis",
        NDArray(Array(pcaBasis.rows.toLong, pcaBasis.cols.toLong), pcaBasis.t.flatten(false).toArray)
      )
      _ <- h5file.writeArray(s"$modelPath/model/pcaVariance", variance.toArray)
      _ <- h5file.writeString(s"$modelPath/modelinfo/build-time", Calendar.getInstance.getTime.toString)
      group <- h5file.createGroup(s"$modelPath/representer")
      _ <- {
        for {
          _ <- writeImageRepresenter(h5file, group, gp, modelPath)
          _ <- h5file.writeInt("/version/majorVersion", 0)
          _ <- h5file.writeInt("/version/minorVersion", 9)
        } yield Success(())
      }
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

  private def writeImageRepresenter[D: NDSpace, A: Vectorizer](
                                                                h5file: HDF5File,
                                                                group: Group,
                                                                gp: DiscreteLowRankGaussianProcess[D, DiscreteImageDomain, A],
                                                                modelPath: String
                                                              ): Try[Unit] = {

    val dim = NDSpace[D].dimensionality

    val domain = gp.domain
    val pointSet = domain.pointSet

    // we create a dummy array with 0 vectors. This needs to be there to satisfy the
    // statismo file format, even though it is useless in this context
    val vectorizer = implicitly[Vectorizer[A]]
    val pixelValues = DenseVector.zeros[Float](pointSet.numberOfPoints * vectorizer.dim)

    val direction =
      NDArray(IndexedSeq(dim, dim), pointSet.directions.toBreezeMatrix.flatten(false).toArray.map(_.toFloat))
    val imageDimension: Int = pointSet.dimensionality
    val origin: Array[Float] = domain.origin.toBreezeVector.toArray.map(_.toFloat)
    val spacing: Array[Float] = domain.spacing.toBreezeVector.toArray.map(_.toFloat)
    val size: Array[Int] = domain.size.toBreezeVector.toArray

    for {
      _ <- h5file.writeStringAttribute(group.getFullName, "name", "itkStandardImageRepresenter")
      _ <- h5file.writeStringAttribute(group.getFullName, "version", "0.1")
      _ <- h5file.writeStringAttribute(group.getFullName, "datasetType", "IMAGE")
      _ <- h5file.writeNDArray[Float](s"$modelPath/representer/direction", direction)
      _ <- h5file.writeFloat(s"$modelPath/modelinfo/scores", 0f)
      _ <- h5file
        .writeNDArray[Int](s"$modelPath/representer/imageDimension", NDArray(IndexedSeq(1, 1), Array(imageDimension)))
      _ <- h5file.writeNDArray[Int](s"$modelPath/representer/size", NDArray(IndexedSeq(dim, 1), size))
      _ <- h5file.writeNDArray[Float](s"$modelPath/representer/origin", NDArray(IndexedSeq(dim, 1), origin))
      _ <- h5file.writeNDArray[Float](s"$modelPath/representer/spacing", NDArray(IndexedSeq(dim, 1), spacing))
      _ <- h5file.writeNDArray[Float](s"$modelPath/representer/pointData/pixelValues",
        NDArray(IndexedSeq(dim, pointSet.numberOfPoints), pixelValues.toArray))
      _ <- h5file.writeInt(s"$modelPath/representer/pointData/pixelDimension", pointSet.dimensionality)
      _ <- h5file.writeIntAttribute(s"$modelPath/representer/pointData/pixelValues", "datatype", 10)

    } yield Success(())
  }

  /**
   * Reads a GP defined on an image domain with values of type A
   * from a statismo file.
   *
   * @param file the file from which to read
   * @param modelPath an optional path into the hdf5 file, from where the model should be read
   * @tparam D the dimensinality of the domain
   * @tparam A the type of the values that the GP represents
   *
   * @return The gaussian process (wrapped in a Success) or Failure.
   */
  def readStatismoImageModel[D: NDSpace: CreateDiscreteImageDomain, A: Vectorizer](
                                                                                    file: java.io.File,
                                                                                    modelPath: String = "/"
                                                                                  ): Try[DiscreteLowRankGaussianProcess[D, DiscreteImageDomain, A]] = {

    val modelOrFailure = for {
      h5file <- HDF5Utils.openFileForReading(file)

      representerName <- h5file.readStringAttribute(s"$modelPath/representer/", "name")
      // read mesh according to type given in representer
      image <- representerName match {
        case "itkStandardImageRepresenter" => readImageRepresenter(h5file, modelPath)
        case _ =>
          h5file.readStringAttribute(s"$modelPath/representer/", "datasetType") match {
            case Success("IMAGE") => readImageRepresenter(h5file, modelPath)
            case Success(datasetType) =>
              Failure(new Exception(s"can only read model of datasetType IMAGE. Got $datasetType instead"))
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
      pcaVarianceVector = DenseVector(pcaVarianceArray.data).map(_.toDouble)
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

      val gp = new DiscreteLowRankGaussianProcess[D, DiscreteImageDomain, A](image,
        meanVector.map(_.toDouble),
        pcaVarianceVector.map(_.toDouble),
        pcaBasisMatrix.map(_.toDouble))
      gp

    }

    modelOrFailure

  }

  private def readImageRepresenter[D: NDSpace: CreateDiscreteImageDomain](
                                                                           h5file: HDF5File,
                                                                           modelPath: String
                                                                         ): Try[DiscreteImageDomain[D]] = {

    val dim = NDSpace[D].dimensionality

    for {
      origin <- h5file
        .readNDArray[Float](s"$modelPath/representer/origin")
        .flatMap(origin =>
          if (origin.dims(0) != dim)
            Failure(new Exception("the representer direction is not 3D"))
          else
            Success(origin)
        )
      originScalismo = Point[D](ndArrayFloatToMatrix(origin).toDenseVector.toArray.map(_.toDouble))

      spacing <- h5file
        .readNDArray[Float](s"$modelPath/representer/spacing")
        .flatMap(spacing =>
          if (spacing.dims(0) != dim)
            Failure(new Exception(s"the representer direction is not $dim"))
          else
            Success(spacing)
        )
      spacingScalismo = EuclideanVector[D](ndArrayFloatToMatrix(spacing).toDenseVector.toArray.map(_.toDouble))

      size <- h5file
        .readNDArray[Int](s"$modelPath/representer/size")
        .flatMap(size =>
          if (size.dims(0) != dim)
            Failure(new Exception(s"the representer direction is not $dim"))
          else
            Success(size)
        )
      sizeScalismo = IntVector[D](ndArrayIntToMatrix(size).toDenseVector.toArray)

    } yield {

      DiscreteImageDomain(StructuredPoints[D](originScalismo, spacingScalismo, sizeScalismo))

    }

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

}
