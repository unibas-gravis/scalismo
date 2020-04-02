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

import scalismo.geometry.{EuclideanVector, _2D, _3D}
import scalismo.image.DiscreteImageDomain
import scalismo.mesh.TriangleMesh
import scalismo.statisticalmodel.{DiscreteLowRankGaussianProcess, PointDistributionModel, StatisticalMeshModel}

import scala.util.Try

object StatisticalModelIO {

  private def modelConverterToPointDistribution(model: StatisticalMeshModel): PointDistributionModel[_3D, TriangleMesh] = {
    // add for loop - remove .get\
      PointDistributionModel(model.gp)
  }

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
    // currently, we support only the statismo format
    val pModel = PointDistributionModelIO.readStatisticalMeshModel(file, modelPath)
    for{
      model <- pModel
    }
    yield modelConverterToMeshModel(model)
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
    // currently, we support only the statismo format
    val pModel = modelConverterToPointDistribution(model)
    PointDistributionModelIO.writeStatisticalMeshModel(pModel, file, modelPath)
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
}
