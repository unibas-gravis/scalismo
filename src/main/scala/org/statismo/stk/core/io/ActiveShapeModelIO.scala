package org.statismo.stk.core.io


import org.statismo.stk.core.statisticalmodel.{MultivariateNormalDistribution, ASMProfileDistributions, ActiveShapeModel}
import scala.util.{Success, Try}
import java.io.File
import ncsa.hdf.`object`.Group
import org.statismo.stk.core.common.UnstructuredPointsDomain
import org.statismo.stk.core.geometry.{Point}
import breeze.linalg.{DenseMatrix, DenseVector}

/**
 * Created by Luethi on 09.03.14.
 */
object ActiveShapeModelIO {


  def writeASM[FE <: ActiveShapeModel.FeatureExtractor : HDF5Write](asm : ActiveShapeModel[FE], fn : File) : Try[Unit] = {
    val featureExtractorWriter = implicitly[HDF5Write[FE]]
    for {
      statismoStatus <- StatismoIO.writeStatismoMeshModel(asm, fn)
      h5file <- HDF5Utils.openFileForWriting(fn)
      asmGroup <- h5file.createGroup("/ASMModel")
      _ <- writeIntensityDistributions(h5file, asmGroup, asm.intensityDistributions)
      _ <- featureExtractorWriter.write(asm.featureExtractor, h5file, asmGroup)
    } yield Success(())
  }



  def readASM[FE <: ActiveShapeModel.FeatureExtractor : HDF5Read](fn : File) : Try[ActiveShapeModel[FE]] = {
    val featureExtractorReader = implicitly[HDF5Read[FE]]

    for {
      shapeModel <- StatismoIO.readStatismoMeshModel(fn)
      h5file <- HDF5Utils.openFileForReading(fn)
      asmGroup <- h5file.getGroup("/ASMModel")
      intensityDistributions <- readIntensityDistributions(h5file, asmGroup)
      featureExtractor <- featureExtractorReader.read(h5file, asmGroup)
    } yield new ActiveShapeModel(shapeModel, intensityDistributions, featureExtractor)
  }


  private[this] def readIntensityDistributions(h5file : HDF5File, group: Group) : Try[ASMProfileDistributions] = {
    val groupName = group.getName()
    val ptDim = 3
    for {
      profileDim <- h5file.readInt(s"$groupName/profileDimension")
      profilePtsArray <- h5file.readArray[Float](s"$groupName/profilePoints")
      pts = profilePtsArray.grouped(ptDim).map(data => Point(data(0), data(1), data(2))).toIndexedSeq
      covArray <- h5file.readNDArray[Float](s"$groupName/cov")
      (m,n) = (covArray.dims(0).toInt, covArray.dims(1).toInt)
      covMats = covArray.data.grouped(n * n ).map(data => DenseMatrix.create(n, n, data))
      meanArray <- h5file.readArray[Float](s"$groupName/mean")
      meanVecs = meanArray.grouped(n).map(data => DenseVector(data))
    } yield {
      val dists =  meanVecs.zip(covMats).map{case(m, c) => new  MultivariateNormalDistribution(m, c)}.toArray
      ASMProfileDistributions(new UnstructuredPointsDomain(pts),dists)
  }

  }

  private[this] def writeIntensityDistributions(h5file : HDF5File, group: Group, distributions: ASMProfileDistributions) : Try[Unit] = {
    val numEntries = distributions.domain.numberOfPoints
    val distDim = if (numEntries > 0) distributions.values(0).mean.size else 0
    val ptArray  = distributions.domain.points.force.flatten(_.data).toArray
    val meanArray = distributions.values.map(_.mean.data).flatten
    val covArray = distributions.values.map(_.cov.data).flatten
    val groupName = group.getFullName
    for {
      _ <- h5file.writeInt(s"$groupName/numberOfProfilePoints", numEntries)
      _ <- h5file.writeInt(s"$groupName/profileDimension", distDim)
      _ <- h5file.writeArray(s"$groupName/profilePoints", ptArray)
      _ <- h5file.writeArray(s"$groupName/mean", meanArray)
      covNDArray = new NDArray(Array[Long](numEntries * distDim, distDim), covArray)
      _ <- h5file.writeNDArray(s"$groupName/cov", covNDArray)
    } yield ()
  }


}
