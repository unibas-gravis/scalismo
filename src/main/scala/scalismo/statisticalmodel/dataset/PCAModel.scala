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

import scalismo.common.VectorField
import scalismo.numerics.{ Sampler, FixedPointsUniformMeshSampler3D }
import scalismo.statisticalmodel.{ GaussianProcess, LowRankGaussianProcess, StatisticalMeshModel }
import scalismo.geometry._

import scala.util.Failure
import scala.util.Success
import scala.util.Try

/**
 * Implements utility functions for building a [[StatisticalMeshModel]] from a [[DataCollection]] containing a reference and items in correspondence
 */
object PCAModel {

  type MeshSampler = Sampler[_3D]

  /**
   *  Adds a bias model to the given pca model
   */
  def augmentModel(pcaModel: StatisticalMeshModel, biasModel: GaussianProcess[_3D, _3D], numBasisFunctions: Int): StatisticalMeshModel = {

    val modelGP = pcaModel.gp.interpolateNearestNeighbor
    val newMean = modelGP.mean + biasModel.mean
    val newCov = modelGP.cov + biasModel.cov
    val newGP = GaussianProcess(newMean, newCov)
    val sampler = FixedPointsUniformMeshSampler3D(pcaModel.referenceMesh, 2 * numBasisFunctions, 42)
    val newLowRankGP = LowRankGaussianProcess.approximateGP(newGP, sampler, numBasisFunctions)
    StatisticalMeshModel(pcaModel.referenceMesh, newLowRankGP)
  }

  /**
   * Returns a PCA model with given reference mesh and a set of items in correspondence.
   * All points of the reference mesh are considered for computing the PCA
   */
  def buildModelFromDataCollection(dc: DataCollection): Try[StatisticalMeshModel] = {
    if (dc.size < 3) return Failure(new Throwable(s"A data collection with at least 3 transformations is required to build a PCA Model (only ${dc.size} were provided)"))

    val fields = dc.dataItems.map{ i =>
        VectorField[_3D, _3D](i.transformation.domain, p => i.transformation(p) - p)
    }
    Success(StatisticalMeshModel.createWithPCA(dc.reference, fields))
  }
}