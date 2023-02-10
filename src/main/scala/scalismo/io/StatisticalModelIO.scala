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

import java.io._
import scalismo.common.{Scalar, UnstructuredPointsDomain, Vectorizer}
import scalismo.geometry.{EuclideanVector, _1D, _2D, _3D}
import scalismo.image.DiscreteImageDomain
import scalismo.io.statisticalmodel.StatismoIO
import scalismo.mesh.{LineMesh, TetrahedralMesh, TriangleMesh}
import scalismo.statisticalmodel.{DiscreteLowRankGaussianProcess, PointDistributionModel, StatisticalMeshModel}

import scala.util.Try

object StatisticalModelIO {

  /**
   * Reads and write of point distribution models with different domain types.
   * The model file type is determined based on the extension.
   * Currently only the Scalismo format (.h5) is supported.
   */
  /**
   * Reads a PDM with TriangleMesh[_2D] as the domain
   * @param file The statismo file
   * @param modelPath in the hdf5 directory
   * @return A PointDistributionModel or the Failure
   */
  def readStatisticalTriangleMeshModel2D(file: File,
                                         modelPath: String = "/"): Try[PointDistributionModel[_2D, TriangleMesh]] = {
    StatismoIO.readStatismoPDM[_2D, TriangleMesh](file, modelPath)
  }

  /**
   * Writes a PDM with TriangleMesh[_2D] as the domain
   * @param file The statismo file
   * @param modelPath in the hdf5 directory
   * @return A PointDistributionModel or the Failure
   */
  def writeStatisticalTriangleMeshModel2D(model: PointDistributionModel[_2D, TriangleMesh],
                                          file: File,
                                          modelPath: String = "/"): Try[Unit] = {
    StatismoIO.writeStatismoPDM[_2D, TriangleMesh](model, file, modelPath)
  }

  /**
   * Reads a PDM with TriangleMesh[_3D] as the domain
   * @param file The statismo file
   * @param modelPath in the hdf5 directory
   * @return A PointDistributionModel or the Failure
   */
  def readStatisticalTriangleMeshModel3D(file: File,
                                         modelPath: String = "/"): Try[PointDistributionModel[_3D, TriangleMesh]] = {
    StatismoIO.readStatismoPDM[_3D, TriangleMesh](file, modelPath)
  }

  /**
   * Writes a PDM with TriangleMesh[_3D] as the domain
   * @param file The statismo file
   * @param modelPath in the hdf5 directory
   * @return A PointDistributionModel or the Failure
   */
  def writeStatisticalTriangleMeshModel3D(model: PointDistributionModel[_3D, TriangleMesh],
                                          file: File,
                                          modelPath: String = "/"): Try[Unit] = {
    StatismoIO.writeStatismoPDM[_3D, TriangleMesh](model, file, modelPath)
  }

  /**
   * Reads a PDM with LineMesh[_2D] as the domain
   * @param file The statismo file
   * @param modelPath in the hdf5 directory
   * @return A PointDistributionModel or the Failure
   */
  def readStatisticalLineMeshModel2D(file: File,
                                     modelPath: String = "/"): Try[PointDistributionModel[_2D, LineMesh]] = {
    StatismoIO.readStatismoPDM[_2D, LineMesh](file, modelPath)
  }

  /**
   * Writes a PDM with LineMesh[_2D] as the domain
   * @param file The statismo file
   * @param modelPath in the hdf5 directory
   * @return A PointDistributionModel or the Failure
   */
  def writeStatisticalLineMeshModel2D(model: PointDistributionModel[_2D, LineMesh],
                                      file: File,
                                      modelPath: String = "/"): Try[Unit] = {
    StatismoIO.writeStatismoPDM[_2D, LineMesh](model, file, modelPath)
  }

  /**
   * Reads a PDM with TriangleMesh[_3D] as the domain
   * @param file The statismo file
   * @param modelPath in the hdf5 directory
   * @return A PointDistributionModel or the Failure
   */
  def readStatisticalLineMeshModel3D(file: File,
                                     modelPath: String = "/"): Try[PointDistributionModel[_3D, LineMesh]] = {
    StatismoIO.readStatismoPDM[_3D, LineMesh](file, modelPath)
  }

  /**
   * Writes a PDM with TriangleMesh[_3D] as the domain
   * @param file The statismo file
   * @param modelPath in the hdf5 directory
   * @return A PointDistributionModel or the Failure
   */
  def writeStatisticalLineMeshModel3D(model: PointDistributionModel[_3D, LineMesh],
                                      file: File,
                                      modelPath: String = "/"): Try[Unit] = {
    StatismoIO.writeStatismoPDM[_3D, LineMesh](model, file, modelPath)
  }

  /**
   * Reads a PDM with TetrahedralMesh[_3D] as the domain
   * @param file The statismo file
   * @param modelPath in the hdf5 directory
   * @return A PointDistributionModel or the Failure
   */
  def readStatisticalTetrahedralMeshModel3D(
    file: File,
    modelPath: String = "/"
  ): Try[PointDistributionModel[_3D, TetrahedralMesh]] = {
    StatismoIO.readStatismoPDM[_3D, TetrahedralMesh](file, modelPath)
  }

  /**
   * Writes a PDM with TetrahedralMesh[_3D] as the domain
   * @param file The statismo file
   * @param modelPath in the hdf5 directory
   * @return A PointDistributionModel or the Failure
   */
  def writeStatisticalTetrahedralMeshModel3D(model: PointDistributionModel[_3D, TetrahedralMesh],
                                             file: File,
                                             modelPath: String = "/"): Try[Unit] = {
    StatismoIO.writeStatismoPDM[_3D, TetrahedralMesh](model, file, modelPath)
  }

  /**
   * Reads a PDM with UnstructuredPointsDomain[_3D] as the domain
   * @param file The statismo file
   * @param modelPath in the hdf5 directory
   * @return A PointDistributionModel or the Failure
   */
  def readStatisticalPointModel3D(
    file: File,
    modelPath: String = "/"
  ): Try[PointDistributionModel[_3D, UnstructuredPointsDomain]] = {
    StatismoIO.readStatismoPDM[_3D, UnstructuredPointsDomain](file, modelPath)
  }

  /**
   * Writes a PDM with UnstructuredPointsDomain[_3D] as the domain
   * @param file The statismo file
   * @param modelPath in the hdf5 directory
   * @return A PointDistributionModel or the Failure
   */
  def writeStatisticalPointModel3D(model: PointDistributionModel[_3D, UnstructuredPointsDomain],
                                   file: File,
                                   modelPath: String = "/"): Try[Unit] = {
    StatismoIO.writeStatismoPDM[_3D, UnstructuredPointsDomain](model, file, modelPath)
  }

  /**
   * Reads a PDM with UnstructuredPointsDomain[_2D] as the domain
   * @param file The statismo file
   * @param modelPath in the hdf5 directory
   * @return A PointDistributionModel or the Failure
   */
  def readStatisticalPointModel2D(
    file: File,
    modelPath: String = "/"
  ): Try[PointDistributionModel[_2D, UnstructuredPointsDomain]] = {
    StatismoIO.readStatismoPDM[_2D, UnstructuredPointsDomain](file, modelPath)
  }

  /**
   * Writes a PDM with UnstructuredPointsDomain[_2D] as the domain
   * @param file The statismo file
   * @param modelPath in the hdf5 directory
   * @return A PointDistributionModel or the Failure
   */
  def writeStatisticalPointModel2D(model: PointDistributionModel[_2D, UnstructuredPointsDomain],
                                   file: File,
                                   modelPath: String = "/"): Try[Unit] = {
    StatismoIO.writeStatismoPDM[_2D, UnstructuredPointsDomain](model, file, modelPath)
  }

  /**
   * Reads a PDM with UnstructuredPointsDomain[_1D] as the domain
   * @param file The statismo file
   * @param modelPath in the hdf5 directory
   * @return A PointDistributionModel or the Failure
   */
  def readStatisticalPointModel1D(
    file: File,
    modelPath: String = "/"
  ): Try[PointDistributionModel[_1D, UnstructuredPointsDomain]] = {
    StatismoIO.readStatismoPDM[_1D, UnstructuredPointsDomain](file, modelPath)
  }

  /**
   * Writes a PDM with UnstructuredPointsDomain[_1D] as the domain
   * @param file The statismo file
   * @param modelPath in the hdf5 directory
   * @return A PointDistributionModel or the Failure
   */
  def writeStatisticalPointModel1D(model: PointDistributionModel[_1D, UnstructuredPointsDomain],
                                   file: File,
                                   modelPath: String = "/"): Try[Unit] = {
    StatismoIO.writeStatismoPDM[_1D, UnstructuredPointsDomain](model, file, modelPath)
  }

  /**
   * Conversion between old format only supporting TriangleMesh PDMs and the new general PDM format
   * @param model statisticalMeshModel
   * @return PointDistributionModel with a TriangleMesh[_3D] as the domain
   */
  private def modelConverterToPointDistribution(
    model: StatisticalMeshModel
  ): PointDistributionModel[_3D, TriangleMesh] = {
    PointDistributionModel(model.gp)
  }

  /**
   * Conversion between new general PDM format and the old format only supporting TriangleMesh PDMs
   * @param model PointDistributionModel with a TriangleMesh[_3D] as the domain
   * @return statisticalMeshModel
   */
  private def modelConverterToMeshModel(model: PointDistributionModel[_3D, TriangleMesh]): StatisticalMeshModel = {
    StatisticalMeshModel(model.reference, model.gp)
  }

  /**
   * Reads a statistical mesh model. The file type is determined
   * based on the extension. Currently on the Scalismo format (.h5)
   * is supported.
   *
   * @param file The statismo file
   * @return A StatisticalMeshModel or the Failure
   */
  def readStatisticalMeshModel(file: File, modelPath: String = "/"): Try[StatisticalMeshModel] = {
    val pModel = readStatisticalTriangleMeshModel3D(file, modelPath)
    for {
      model <- pModel
    } yield modelConverterToMeshModel(model)
  }

  /**
   * Writes a statistical mesh model. The file type is determined
   * based on the extension. Currently on the Scalismo format (.h5)
   * is supported.
   *
   * @param model The statistical model
   * @param file  The file to which the model is written
   * @return In case of Failure, the Failure is returned.
   */
  def writeStatisticalMeshModel(model: StatisticalMeshModel, file: File, modelPath: String = "/"): Try[Unit] = {
    val pModel = modelConverterToPointDistribution(model)
    writeStatisticalTriangleMeshModel3D(pModel, file, modelPath)
  }

  /**
   * Reads a model of 2D deformation fields defined on a 2D image domain
   * @param file the file from which the model is read
   * @return a 2D deformation model
   */
  def readDeformationModel2D(
    file: java.io.File
  ): Try[DiscreteLowRankGaussianProcess[_2D, DiscreteImageDomain, EuclideanVector[_2D]]] = {
    StatismoIO.readStatismoImageModel[_2D, EuclideanVector[_2D]](file, "/")
  }

  /**
   * Reads a model of 3D deformation fields defined on a 3D image domain
   * @param file the file from which the model is read
   * @return a 3D deformation model
   */
  def readDeformationModel3D(
    file: java.io.File
  ): Try[DiscreteLowRankGaussianProcess[_3D, DiscreteImageDomain, EuclideanVector[_3D]]] = {
    StatismoIO.readStatismoImageModel[_3D, EuclideanVector[_3D]](file, "/")
  }

  /**
   * Writes a model of 2D deformation fields defined on a 2D image domain
   *
   * @param gp the deformation model
   * @param file the file to which the model is written
   * @return Success if model could be read, Failure otherwise
   */
  def writeDeformationModel2D(gp: DiscreteLowRankGaussianProcess[_2D, DiscreteImageDomain, EuclideanVector[_2D]],
                              file: File): Try[Unit] = {
    StatismoIO.writeStatismoImageModel[_2D, EuclideanVector[_2D]](gp, file, "/")
  }

  /**
   * Writes a model of 3D deformation fields defined on a 3D image domain
   *
   * @param gp the deformation model
   * @param file the file to which the model is written
   * @return Success if model could be read, Failure otherwise
   */
  def writeDeformationModel3D(gp: DiscreteLowRankGaussianProcess[_3D, DiscreteImageDomain, EuclideanVector[_3D]],
                              file: File): Try[Unit] = {
    StatismoIO.writeStatismoImageModel[_3D, EuclideanVector[_3D]](gp, file, "/")
  }

  /**
   * Reads an intensity model defined on a tetrahedral mesh
   *
   * @param file the file to which the model is written
   * @return A try, where the success case holds the model, and the failure case the corresponding exception
   */
  def readVolumeMeshIntensityModel3D(
    file: File
  ): Try[DiscreteLowRankGaussianProcess[_3D, TetrahedralMesh, Float]] = {
    StatismoIO.readIntensityModel[_3D, TetrahedralMesh, Float](file, "/")
  }

  /**
   * Writes a model of 3D deformation fields defined on a 3D tetrahedral mesh
   *
   * @param gp the deformation model
   * @param file the file to which the model is written
   * @return Success if model could be read, Failure otherwise
   */
  def writeVolumeMeshIntensityModel3D(gp: DiscreteLowRankGaussianProcess[_3D, TetrahedralMesh, Float],
                                      file: File): Try[Unit] = {
    StatismoIO.writeIntensityModel[_3D, TetrahedralMesh, Float](gp, file, "/")
  }

  /**
   * Reads an intensity model defined on an image domain
   *
   * @param file the file to which the model is written
   * @return A try, where the success case holds the model, and the failure case the corresponding exception
   */
  def readImageIntensityModel3D(
    file: File
  ): Try[DiscreteLowRankGaussianProcess[_3D, DiscreteImageDomain, Float]] = {
    StatismoIO.readStatismoImageModel[_3D, Float](file, "/")
  }

  /** Writes a model of 3D deformation fields defined on a 3D image domain
   *
   * @param gp the deformation model
   * @param file the file to which the model is written
   * @return Success if model could be read, Failure otherwise
   */
  def writeImageIntensityModel3D[Domain[D] <: DiscreteImageDomain[D]](
    gp: DiscreteLowRankGaussianProcess[_3D, DiscreteImageDomain, Float],
    file: File
  ): Try[Unit] = {
    StatismoIO.writeStatismoImageModel[_3D, Float](gp, file, "/")
  }

}
