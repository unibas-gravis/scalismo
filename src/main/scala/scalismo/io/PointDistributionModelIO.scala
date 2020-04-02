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

import scalismo.common.UnstructuredPointsDomain
import scalismo.geometry.{_1D, _2D, _3D}
import scalismo.mesh.{LineMesh, TetrahedralMesh, TriangleMesh}
import scalismo.statisticalmodel.PointDistributionModel

import scala.util.Try

object PointDistributionModelIO {

  /**
   * Reads a statistical mesh model. The file type is determined
   * based on the extension. Currently on the Scalismo format (.h5)
   * is supported.
   *
   * @param file The statismo file
   * @return A StatisticalMeshModel or the Failure
   */

  // possible depricate
  def readStatisticalMeshModel(file: File, modelPath: String = "/"): Try[PointDistributionModel[_3D, TriangleMesh]] = {
    // currently, we support only the statismo format
    StatismoIO.readStatismoPointModel[_3D, TriangleMesh](file, modelPath)
  }

  def writeStatisticalMeshModel(model: PointDistributionModel[_3D, TriangleMesh], file: File, modelPath: String = "/"): Try[Unit] = {
    StatismoIO.writeStatismoPointModel[_3D, TriangleMesh](model, file, modelPath)
  }

  def readStatisticalLineMeshModel2D(file: File, modelPath: String = "/"): Try[PointDistributionModel[_2D, LineMesh]] = {
    StatismoIO.readStatismoPointModel[_2D, LineMesh](file, modelPath)
  }

  def writeStatisticalLineMeshModel2D(model: PointDistributionModel[_2D, LineMesh], file: File, modelPath: String = "/"): Try[Unit] = {
    StatismoIO.writeStatismoPointModel[_2D, LineMesh](model, file, modelPath)
  }

  def readStatisticalLineMeshModel3D(file: File, modelPath: String = "/"): Try[PointDistributionModel[_3D, LineMesh]] = {
    StatismoIO.readStatismoPointModel[_3D, LineMesh](file, modelPath)
  }

  def writeStatisticalLineMeshModel3D(model: PointDistributionModel[_3D, LineMesh], file: File, modelPath: String = "/"): Try[Unit] = {
    StatismoIO.writeStatismoPointModel[_3D, LineMesh](model, file, modelPath)
  }

  def readStatisticalTetrahedralMeshModel3D(file: File, modelPath: String = "/"): Try[PointDistributionModel[_3D, TetrahedralMesh]] = {
    StatismoIO.readStatismoPointModel[_3D, TetrahedralMesh](file, modelPath)
  }

  def writeStatisticalTetrahedralMeshModel3D(model: PointDistributionModel[_3D, TetrahedralMesh], file: File, modelPath: String = "/"): Try[Unit] = {
    StatismoIO.writeStatismoPointModel[_3D, TetrahedralMesh](model, file, modelPath)
  }

  def readStatisticalPointModel3D(file: File, modelPath: String = "/"): Try[PointDistributionModel[_3D, UnstructuredPointsDomain]] = {
    StatismoIO.readStatismoPointModel[_3D, UnstructuredPointsDomain](file, modelPath)
  }

  def writeStatisticalPointModel3D(model: PointDistributionModel[_3D, UnstructuredPointsDomain], file: File, modelPath: String = "/"): Try[Unit] = {
    StatismoIO.writeStatismoPointModel[_3D, UnstructuredPointsDomain](model, file, modelPath)
  }

  def readStatisticalPointModel2D(file: File, modelPath: String = "/"): Try[PointDistributionModel[_2D, UnstructuredPointsDomain]] = {
    StatismoIO.readStatismoPointModel[_2D, UnstructuredPointsDomain](file, modelPath)
  }

  def writeStatisticalPointModel2D(model: PointDistributionModel[_2D, UnstructuredPointsDomain], file: File, modelPath: String = "/"): Try[Unit] = {
    StatismoIO.writeStatismoPointModel[_2D, UnstructuredPointsDomain](model, file, modelPath)
  }

  def readStatisticalPointModel1D(file: File, modelPath: String = "/"): Try[PointDistributionModel[_1D, UnstructuredPointsDomain]] = {
    StatismoIO.readStatismoPointModel[_1D, UnstructuredPointsDomain](file, modelPath)
  }

  def writeStatisticalPointModel1D(model: PointDistributionModel[_1D, UnstructuredPointsDomain], file: File, modelPath: String = "/"): Try[Unit] = {
    StatismoIO.writeStatismoPointModel[_1D, UnstructuredPointsDomain](model, file, modelPath)
  }


}
