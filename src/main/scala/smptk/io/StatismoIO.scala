package smptk
package io

import java.io.File
import scala.util.Try
import breeze.linalg.DenseVector
import breeze.linalg.DenseMatrix
import scala.util.Failure
import scala.util.Success
import scala.reflect.ClassTag
import reflect.runtime.universe.{ TypeTag, typeOf }
import smptk.mesh.TriangleCell
import smptk.mesh.TriangleMesh
import statisticalmodel.StatisticalMeshModel
import smptk.geometry._

object StatismoIO {

  def readStatismoMeshModel(file: File): Try[StatisticalMeshModel] = {
    val filename = file.getAbsolutePath()
    val h5file = HDF5Utils.openFileForReading(file)

    val datasetTypeOrFailure = h5file.readStringAttribute("/representer/", "datasetType")
    datasetTypeOrFailure match { 
      case Success("POLYGON_MESH") => {}
      case Success(datasetType) => { return Failure(new Exception(s"can only read model of datasetType POLYGON_MESH. Got $datasetType instead")) }
      case Failure(ex) => { return Failure(ex) }
    }
    
    
    val modelOrFailure = for {
     
      
      meanArray <- h5file.readNDArray[Float]("/model/mean")
      meanVector = DenseVector(meanArray.data).map(_.toDouble)
      pcaBasisArray <- h5file.readNDArray[Float]("/model/pcaBasis")
      pcaBasisMatrix = ndArrayToMatrix(pcaBasisArray)
      pcaVarianceArray <- h5file.readNDArray[Float]("/model/pcaVariance")
      pcaVarianceVector = DenseVector(pcaVarianceArray.data).map(_.toDouble)

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
      mesh = TriangleMesh(points, cells)
    } yield {


        // statismo stores the mean as the point position and not as a displaceme
      // ref. we compensate for this
      def flatten(v: IndexedSeq[Point[ThreeD]]) = DenseVector(v.flatten(pt => Array(pt(0), pt(1), pt(2))).toArray)
      val refpointsVec = flatten(mesh.points.toIndexedSeq)
      val meanDefVector = meanVector - refpointsVec

      // statismo stores the pcaBasisMatrix: each column corresponds to phi_i * sqrt(lambda_i)
      // we recover phi_i from it
      val lambdaSqrtInv = pcaVarianceVector.map(l => if (l > 1e-8) 1.0 / math.sqrt(l) else 0.0)
      StatisticalMeshModel(mesh, meanDefVector, pcaVarianceVector, pcaBasisMatrix * breeze.linalg.diag(lambdaSqrtInv))
    }

    h5file.close()
    modelOrFailure
  }

  private def ndArrayToMatrix(array: NDArray[Float])(implicit dummy: DummyImplicit, dummy2: DummyImplicit) = {
    // the data in ndarray is stored row-major, but DenseMatrix stores it column major. We therefore
    // do switch dimensions and transpose
    DenseMatrix.create(array.dims(1).toInt, array.dims(0).toInt, array.data).map(_.toDouble).t
  }

  private def ndArrayToMatrix(array: NDArray[Double])(implicit dummy: DummyImplicit) = {
    // the data in ndarray is stored row-major, but DenseMatrix stores it column major. We therefore
    // do switch dimensions and transpose
    DenseMatrix.create(array.dims(1).toInt, array.dims(0).toInt, array.data).map(_.toDouble).t
  }

  private def ndArrayToMatrix(array: NDArray[Int]) = {
    // the data in ndarray is stored row-major, but DenseMatrix stores it column major. We therefore
    // do switch dimensions and transpose

    DenseMatrix.create(array.dims(1).toInt, array.dims(0).toInt, array.data).t
  }

}