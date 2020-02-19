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

import scalismo.geometry.{_2D, _3D, EuclideanVector}
import scalismo.image.DiscreteImageDomain
import scalismo.statisticalmodel.experimental.StatisticalVolumeMeshModel
import scalismo.statisticalmodel.{DiscreteLowRankGaussianProcess, StatisticalMeshModel}

import scala.util.Try

object StatisticalModelIO {

  /**
   * Reads a statistical mesh model. The file type is determined
   * based on the extension. Currently on the Scalismo format (.h5)
   * is supported.
   *
   * @param file The statismo file
   * @return A StatisticalMeshModel or the Failure
   */
  def readStatisticalMeshModel(file: File): Try[StatisticalMeshModel] = {
    // currently, we support only the statismo format
    StatismoIO.readStatismoMeshModel(file, "/")
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
  def writeStatisticalMeshModel(model: StatisticalMeshModel, file: File): Try[Unit] = {
    // currently, we support only the statismo format
    StatismoIO.writeStatismoMeshModel(model, file, "/")
  }

  /**
   * Reads a statistical mesh volume model. The file type is determined
   * based on the extension. Currently on the Scalismo format (.h5)
   * is supported.
   *
   * @param file The statismo file
   * @return A StatisticalVolMeshModel or the Failure
   */
  def readStatisticalVolumeMeshModel(file: File): Try[StatisticalVolumeMeshModel] = {
    // currently, we support only the statismo format
    StatismoIO.readStatismoVolumeMeshModel(file, "/")
  }

  /**
   * Write a statistical mesh volume model.
   * Currently on the Scalismo format (.h5) is supported.
   *
   * @param model The volume mesh model
   * @param file The statismo file
   * @return A StatisticalMeshVolumeModel or the Failure
   */
  def writeStatisticalVolumeMeshModel(model: StatisticalVolumeMeshModel, file: File): Try[Unit] = {
    // currently, we support only the statismo format
    StatismoIO.writeStatismoVolumeMeshModel(model, file, "/")
  }

  /**
   * Reads a model of 2D deformation fields defined on a 2D image domain
   * @param file the file from which the model is read
   * @return a 2D deformation model
   */
  def readDeformationModel2D(
    file: java.io.File
  ): Try[DiscreteLowRankGaussianProcess[_2D, DiscreteImageDomain[_2D], EuclideanVector[_2D]]] = {
    StatismoIO.readStatismoImageModel[_2D, EuclideanVector[_2D]](file, "/")
  }

  /**
   * Reads a model of 3D deformation fields defined on a 3D image domain
   * @param file the file from which the model is read
   * @return a 3D deformation model
   */
  def readDeformationModel3D(
    file: java.io.File
  ): Try[DiscreteLowRankGaussianProcess[_3D, DiscreteImageDomain[_3D], EuclideanVector[_3D]]] = {
    StatismoIO.readStatismoImageModel[_3D, EuclideanVector[_3D]](file, "/")
  }

  /**
   * Writes a model of 2D deformation fields defined on a 2D image domain
   *
   * @param gp the deformation model
   * @param file the file to which the model is written
   * @return Success if model could be read, Failure otherwise
   */
  def writeDeformationModel2D(gp: DiscreteLowRankGaussianProcess[_2D, DiscreteImageDomain[_2D], EuclideanVector[_2D]],
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
  def writeDeformationModel3D(gp: DiscreteLowRankGaussianProcess[_3D, DiscreteImageDomain[_3D], EuclideanVector[_3D]],
                              file: File): Try[Unit] = {
    StatismoIO.writeStatismoImageModel[_3D, EuclideanVector[_3D]](gp, file, "/")
  }
}
