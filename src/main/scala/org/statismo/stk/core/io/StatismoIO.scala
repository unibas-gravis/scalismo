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

import ncsa.hdf.`object`._; // the common object package
import ncsa.hdf.`object`.h5._; // the HDF5 implementation
import java.util.List;

case class StatismoModelBuilder(buildTime: String, builderName: String, dataInfo: Option[Seq[(String, String)]], parameters: Option[Seq[(String, String)]])

object StatismoIO {

  def readStatismoMeshModel(file: File): Try[StatisticalMeshModel] = {
    val filename = file.getAbsolutePath()

    val modelOrFailure = for {
      h5file <- HDF5Utils.openFileForReading(file)
      datasetType <- h5file.readStringAttribute("/representer/", "datasetType")
      _ <- if (!datasetType.equals("POLYGON_MESH")) Failure(new Exception(s"can only read model of datasetType POLYGON_MESH. Got $datasetType instead"))
      else Success(())
      meanArray <- h5file.readNDArray[Float]("/model/mean")
      meanVector = DenseVector(meanArray.data)
      pcaBasisArray <- h5file.readNDArray[Float]("/model/pcaBasis")
      pcaBasisMatrix = ndArrayToMatrix(pcaBasisArray)
      pcaVarianceArray <- h5file.readNDArray[Float]("/model/pcaVariance")
      pcaVarianceVector = DenseVector(pcaVarianceArray.data)

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
      _ <- Try { h5file.close() }
    } yield {
      // statismo stores the mean as the point position and not as a displaceme
      // ref. we compensate for this
      def flatten(v: IndexedSeq[Point[ThreeD]]) = DenseVector(v.flatten(pt => Array(pt(0), pt(1), pt(2))).toArray)
      val refpointsVec = flatten(mesh.points.toIndexedSeq)
      val meanDefVector = meanVector - refpointsVec
      // statismo stores the pcaBasisMatrix: each column corresponds to phi_i * sqrt(lambda_i)
      // we recover phi_i from it
      val lambdaSqrtInv = pcaVarianceVector.map(l => if (l > 1e-8) (1.0 / math.sqrt(l)).toFloat else 0f)

      // efficient way to compute: pcaBasisMatrix * breeze.linalg.diag(lambdaSqrtInv)
      // (diag returns densematrix, so the direct computation would be very slow)
      val pcaBasisNormalized = DenseMatrix.zeros[Float](pcaBasisMatrix.rows, pcaBasisMatrix.cols)
      for (i <- 0 until pcaBasisMatrix.cols) {
        pcaBasisNormalized(::, i) := pcaBasisMatrix(::, i) * lambdaSqrtInv(i)
      }
      StatisticalMeshModel(mesh, meanDefVector, pcaVarianceVector, pcaBasisNormalized)

    }

    modelOrFailure
  }

  def writeStatismoMeshModel(
    model: StatisticalMeshModel,
    file: File,
    previousInfo: Option[Seq[StatismoModelBuilder]] = None,
    dataInfo: Option[Seq[(String, String)]] = None,
    parameters: Option[Seq[(String, String)]] = None,
    pointScalarData: Option[Seq[Double]]=None,
    pointVectorData: Option[Seq[DenseVector[Double]]]=None
   ): Try[Unit] = {

    def flatten[T](xs: Seq[Try[T]]): Try[Seq[T]] = {
      val (ss: Seq[Success[T]] @unchecked, fs: Seq[Failure[T]] @unchecked) =
        xs.partition(_.isSuccess)

      if (fs.isEmpty) Success(ss map (_.get))
      else Failure[Seq[T]](fs(0).exception) // Only keep the first failure
    }

    def writeModelInfo(info: StatismoModelBuilder, idx: Int, h5file: HDF5File) = {
      for {
        _ <- h5file.writeString("/modelinfo/modelBuilder-" + idx + "/buildTime", info.buildTime)
        _ <- h5file.writeString("/modelinfo/modelBuilder-" + idx + "/builderName", info.builderName)
        val part2 = info.dataInfo.map(dtInfSeq => flatten(dtInfSeq.map(dtInf => h5file.writeString("/modelinfo/modelBuilder-" + idx + "/datainfo/" + dtInf._1, dtInf._2))))
        res1 <- if (part2.isDefined) part2.get else Success(Seq(()))
        val part3 = info.parameters.map(paramSeq => flatten(paramSeq.map(prm => h5file.writeString("/modelinfo/modelBuilder-" + idx + "/parameters/" + prm._1, prm._2))))
        res2 <- part3.getOrElse(Success(Seq(())))
      } yield res2
    }

    //prepare the data
    val attributesSeq = Seq(("name", "itkStandardMeshRepresenter"), ("version", "0.1"), ("datasetType", "POLYGON_MESH"))
    val cellArray = model.mesh.cells.map(_.ptId1) ++ model.mesh.cells.map(_.ptId2) ++ model.mesh.cells.map(_.ptId3)
    val pts = model.mesh.points.toIndexedSeq.map(p => (p.data(0).toDouble, p.data(1).toDouble, p.data(2).toDouble))
    val pointArray = pts.map(_._1.toFloat) ++ pts.map(_._2.toFloat) ++ pts.map(_._3.toFloat)
    val discretizedMean = model.mesh.points.map(p => p + model.gp.mean(p)).toIndexedSeq.flatten(_.data)
    val pcaBasis = DenseMatrix.zeros[Float](model.mesh.points.size * model.gp.outputDim, model.gp.rank)
    for {
      (point, idx) <- model.mesh.points.toSeq.zipWithIndex
      ((lmda, phi), j) <- model.gp.eigenPairs.zipWithIndex
    } pcaBasis(idx * model.gp.outputDim until (idx + 1) * model.gp.outputDim, j) := (phi(point) * Math.sqrt(lmda)).toBreezeVector

    val maybeError = for {
      h5file <- HDF5Utils.createFile(file)
      _ <- h5file.writeArray("/model/mean", discretizedMean.toArray)
      _ <- h5file.writeArray("/model/noiseVariance", Array(0f))
      _ <- h5file.writeNDArray("/model/pcaBasis", NDArray(Array(pcaBasis.rows, pcaBasis.cols), pcaBasis.t.flatten(false).toArray))
      _ <- h5file.writeArray("/model/pcaVariance", model.gp.eigenPairs.map(p => p._1).toArray)
      _ <- h5file.writeString("/modelinfo/build-time", Calendar.getInstance.getTime.toString)
      group <- h5file.createGroup("/representer")
      _ <- flatten(attributesSeq.map(pair => h5file.writeStringAttribute(group.getFullName, pair._1, pair._2)))
      _ <- h5file.writeNDArray[Int]("/representer/cells", NDArray(Vector(3, model.mesh.cells.size), cellArray.toArray))
      _ <- h5file.writeNDArray[Float]("/representer/points", NDArray(Vector(3, model.mesh.points.size), pointArray.toArray))

      _ <- pointScalarData.map(data =>  h5file.writeNDArray[Double]("/representer/pointData/scalars", NDArray(Vector(1, data.size), data.toArray))).getOrElse(Success())      
      _ <- pointScalarData.map(data => h5file.writeIntAttribute("/representer/pointData/scalars", "datatype", 11)).getOrElse(Success())
      _ <- pointVectorData.map(data => {
        val dim = data(0).size
        val dataArray = data.map(v => for (i <- 0 until dim) yield v(i).toDouble).flatten(s => s)
        h5file.writeNDArray[Double]("/representer/pointData/vectors", NDArray(Vector(data(0).size, data.size), dataArray.toArray))
      }).getOrElse(Success())
      _ <- pointVectorData.map(data => h5file.writeIntAttribute("/representer/pointData/vectors", "datatype", 11)).getOrElse(Success())
      _ <- writeModelInfo(StatismoModelBuilder(Calendar.getInstance.getTime.toString, "Scala StatismoWriter", dataInfo, parameters), previousInfo.map(_.size).getOrElse(0), h5file)
   
      // write any previous info here
      _ <- previousInfo.map(infoList => flatten(infoList.zipWithIndex.map(pair => writeModelInfo(pair._1, pair._2, h5file)))).getOrElse(Success())
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

}