package org.statismo.stk.core
package io

import java.io.File
import scala.util.Try
import breeze.linalg.DenseVector
import breeze.linalg.DenseMatrix
import scala.util.Failure
import scala.util.Success
import scala.reflect.ClassTag
import reflect.runtime.universe.{ TypeTag, typeOf }
import org.statismo.stk.core.mesh.TriangleCell
import org.statismo.stk.core.mesh.TriangleMesh
import statisticalmodel.StatisticalMeshModel
import org.statismo.stk.core.geometry._
import java.util.Calendar
import ncsa.hdf.`object`._
import ncsa.hdf.`object`.h5._
import java.util.List
import java.io.DataOutputStream
import java.io.FileOutputStream
import org.statismo.stk.core.utils.Visualization.VTKStatmodelViewer

object StatismoIO {

  def readStatismoMeshModel(file: File): Try[StatisticalMeshModel] = {
    val filename = file.getAbsolutePath()

    def extractPcaBasisMatrix(pcaBasisMatrix: DenseMatrix[Float], pcaVarianceVector: DenseVector[Float]): DenseMatrix[Float] = {

      val lambdaSqrt = pcaVarianceVector.map(l => math.sqrt(l).toFloat)
      val lambdaSqrtInv = lambdaSqrt.map(l => if (l > 1e-8) (1.0f / l) else 0f)

      val UTU = pcaBasisMatrix(::, 0 until 1).t * pcaBasisMatrix(::, 0 until 1) // take only first row of to be faster
      if (Math.abs(UTU(0, 0) - pcaVarianceVector(0)) < pcaVarianceVector(0) * 1e-3) {
        // this is an old statismo format, that has the pcaVariance directly stored in the PCA matrix, 
        // i.e. pcaBasis = U * sqrt(lmbda), where U is a matrix of eigenvectors and lmbda the corresponding eigenvalues. 
        // We recover U from it.

        // The following code is an efficient way to compute: pcaBasisMatrix * breeze.linalg.diag(lambdaSqrtInv)
        // (diag returns densematrix, so the direct computation would be very slow)
        val U = DenseMatrix.zeros[Float](pcaBasisMatrix.rows, pcaBasisMatrix.cols)
        for (i <- 0 until pcaBasisMatrix.cols) {
          U(::, i) := pcaBasisMatrix(::, i) * lambdaSqrtInv(i)
        }
        U
      } else {
        pcaBasisMatrix // this is already the right matrix - nothing to do here
      }

    }

    val modelOrFailure = for {
      h5file <- HDF5Utils.openFileForReading(file)
      meanArray <- h5file.readNDArray[Float]("/model/mean")
      meanVector = DenseVector(meanArray.data)
      pcaBasisArray <- h5file.readNDArray[Float]("/model/pcaBasis")
      pcaBasisMatrix = ndArrayToMatrix(pcaBasisArray)
      pcaVarianceArray <- h5file.readNDArray[Float]("/model/pcaVariance")
      pcaVarianceVector = DenseVector(pcaVarianceArray.data)

      // read mesh according to type given in representer
      mesh <- h5file.readStringAttribute("/representer/", "name") match {
        case Success("vtkPolyDataRepresenter") => readVTKMeshFromRepresenterGroup(h5file)
        case Success("itkMeshRepresenter") => readVTKMeshFromRepresenterGroup(h5file)
        case Success(_) => {
          h5file.readStringAttribute("/representer/", "datasetType") match {
            case Success("POLYGON_MESH") => readStandardMeshFromRepresenterGroup(h5file)
            case Success(datasetType) => Failure(new Exception(s"can only read model of datasetType POLYGON_MESH. Got $datasetType instead"))
            case Failure(t) => Failure(t)
          }
        }
        case Failure(t) => Failure(t)
      }
      _ <- Try { h5file.close() }
    } yield {
      // statismo stores the mean as the point position and not as a displaceme
      // ref. we compensate for this
      def flatten(v: IndexedSeq[Point[ThreeD]]) = DenseVector(v.flatten(pt => Array(pt(0), pt(1), pt(2))).toArray)
      val refpointsVec = flatten(mesh.points.toIndexedSeq)
      val meanDefVector = meanVector - refpointsVec
      val pcaBasisNormalized = extractPcaBasisMatrix(pcaBasisMatrix, pcaVarianceVector)
      StatisticalMeshModel(mesh, meanDefVector, pcaVarianceVector, pcaBasisNormalized)
    }

    modelOrFailure
  }

  object StatismoVersion extends Enumeration {
    type StatismoVersion = Value
    val v081, v090 = Value
  }

  import StatismoVersion._
  def writeStatismoMeshModel(model: StatisticalMeshModel, file: File, statismoVersion: StatismoVersion = v090): Try[Unit] = {

    val cellArray = model.mesh.cells.map(_.ptId1) ++ model.mesh.cells.map(_.ptId2) ++ model.mesh.cells.map(_.ptId3)
    val pts = model.mesh.points.toIndexedSeq.par.map(p => (p.data(0).toDouble, p.data(1).toDouble, p.data(2).toDouble))
    val pointArray = pts.map(_._1.toFloat) ++ pts.map(_._2.toFloat) ++ pts.map(_._3.toFloat)
    val discretizedMean = model.mesh.points.map(p => p + model.gp.mean(p)).toIndexedSeq.flatten(_.data)
    val pcaVariance = model.gp.eigenPairs.map(p => p._1).toArray
    val pcaBasis = DenseMatrix.zeros[Float](model.mesh.points.size * model.gp.outputDim, model.gp.rank)
    for {
      (point, idx) <- model.mesh.points.toSeq.par.zipWithIndex
      ((lmda, phi), j) <- model.gp.eigenPairs.zipWithIndex
    } {
      pcaBasis(idx * model.gp.outputDim until (idx + 1) * model.gp.outputDim, j) := (phi(point)).toBreezeVector
    }

    if (statismoVersion == v081) {
      // statismo 081 has the variance included in the pcaBasis
      for (i <- 0 until pcaVariance.length) {
        pcaBasis(::, i) *= math.sqrt(pcaVariance(i)).toFloat
      }
    }

    val maybeError = for {
      h5file <- HDF5Utils.createFile(file)
      _ <- h5file.writeArray("/model/mean", discretizedMean.toArray)
      _ <- h5file.writeArray("/model/noiseVariance", Array(0f))
      _ <- h5file.writeNDArray("/model/pcaBasis", NDArray(Array(pcaBasis.rows, pcaBasis.cols), pcaBasis.t.flatten(false).toArray))
      _ <- h5file.writeArray("/model/pcaVariance", pcaVariance.toArray)
      _ <- h5file.writeString("/modelinfo/build-time", Calendar.getInstance.getTime.toString)
      group <- h5file.createGroup("/representer")

      _ <- h5file.writeStringAttribute(group.getFullName, "name", "itkStandardMeshRepresenter")
      _ <- h5file.writeStringAttribute(group.getFullName, "version", "0.1")
      _ <- h5file.writeStringAttribute(group.getFullName, "datasetType", "POLYGON_MESH")

      _ <- h5file.writeNDArray[Int]("/representer/cells", NDArray(Vector(3, model.mesh.cells.size), cellArray.toArray))
      _ <- h5file.writeNDArray[Float]("/representer/points", NDArray(Vector(3, model.mesh.points.size), pointArray.toArray))
      _ <- h5file.writeString("/modelinfo/modelBuilder-0/buildTime", Calendar.getInstance.getTime.toString)
      _ <- h5file.writeString("/modelinfo/modelBuilder-0/builderName", "This is a useless info. The stkCore did not handle Model builder info at creation time.")
      _ <- Try { h5file.close() }

    } yield ()

    maybeError
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

  private def readStandardMeshFromRepresenterGroup(h5file: HDF5File): Try[TriangleMesh] = {
    for {
      vertArray <- h5file.readNDArray[Float]("/representer/points").flatMap(vertArray =>
        if (vertArray.dims(0) != 3)
          Failure(new Exception("the representer points are not 3D points"))
        else
          Success(vertArray))
      vertMat = ndArrayToMatrix(vertArray)
      points = for (i <- 0 until vertMat.cols) yield Point3D(vertMat(0, i), vertMat(1, i), vertMat(2, i))
      cellArray <- h5file.readNDArray[Int]("/representer/cells").flatMap(cellArray =>
        if (cellArray.dims(0) != 3)
          Failure(new Exception("the representer cells are not triangles"))
        else
          Success(cellArray))
      cellMat = ndArrayToMatrix(cellArray)
      cells = for (i <- 0 until cellMat.cols) yield (TriangleCell(cellMat(0, i), cellMat(1, i), cellMat(2, i)))
      cellArray <- h5file.readNDArray[Int]("/representer/cells")
    } yield TriangleMesh(points, cells)
  }

  /*
   * reads the reference (a vtk file), which is stored as a byte array in the hdf5 file)
   */
  private def readVTKMeshFromRepresenterGroup(h5file: HDF5File): Try[TriangleMesh] = {
    for {
      rawdata <- h5file.readNDArray[Byte]("/representer/reference")
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

  def main(args: Array[String]): Unit = {
    org.statismo.stk.core.initialize
    readStatismoMeshModel(new File("c:/Users/Luethi/Downloads/ssmModel2.h5")) match {
      case Success(model) => VTKStatmodelViewer(model).startup(Array())
      case Failure(f) => f.printStackTrace()
    }
  }

}