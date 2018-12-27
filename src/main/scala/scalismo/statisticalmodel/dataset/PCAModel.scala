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

package scalismo.statisticalmodel.dataset

import scalismo.statisticalmodel.{ GaussianProcess, StatisticalMeshModel }
import scalismo.geometry._
import scalismo.utils.Random

import scala.util.Try

/**
 * Implements utility functions for building a [[StatisticalMeshModel]] from a [[DataCollection]] containing a reference and items in correspondence.
 * This object is deprecated and its methods moved to StatisticalMeshModel. The object will be removed in future versions.
 */
@deprecated("Functionality of this object has been moved to StatisticalMeshModel object. This object wil be removed in future versions.", "0.10.0")
object PCAModel {

  /**
   *  Adds a bias model to the given pca model
   */
  @deprecated("Use method in StatisticalMeshModel object instead. This method and containing object wil be removed in future versions.", "0.10.0")
  def augmentModel(pcaModel: StatisticalMeshModel, biasModel: GaussianProcess[_3D, SpatialVector[_3D]], numBasisFunctions: Int)(implicit rng: Random): StatisticalMeshModel = {
    StatisticalMeshModel.augmentModel(pcaModel, biasModel, numBasisFunctions)
  }

  /**
   * Returns a PCA model with given reference mesh and a set of items in correspondence.
   * All points of the reference mesh are considered for computing the PCA
   */
  @deprecated("Use method in StatisticalMeshModel object instead. This method and containing object wil be removed in future versions.", "0.10.0")
  def buildModelFromDataCollection(dc: DataCollection): Try[StatisticalMeshModel] = StatisticalMeshModel.createUsingPCA(dc)

}