package org.statismo.stk.core.dataset

import scala.util.Failure
import scala.util.Success
import scala.util.Try

import org.statismo.stk.core.geometry.Point
import org.statismo.stk.core.geometry._3D
import org.statismo.stk.core.mesh.TriangleMesh
import org.statismo.stk.core.numerics.FixedPointsUniformMeshSampler3D
import org.statismo.stk.core.numerics.Sampler
import org.statismo.stk.core.registration.Transformation
import org.statismo.stk.core.statisticalmodel.LowRankGaussianProcess
import org.statismo.stk.core.statisticalmodel.SpecializedLowRankGaussianProcess
import org.statismo.stk.core.statisticalmodel.StatisticalMeshModel

/**
 * Implements utility functions for building a [[StatisticalMeshModel]] from a [[DataCollection]] containing a reference and items in correspondence 
 * */
object PCAModel {

  type MeshSampler = Sampler[_3D]

  
  /**
   *  Adds a bias model to the given pca model
   */
  private [dataset] def augmentModel(pcaModel: StatisticalMeshModel, biasModel: LowRankGaussianProcess[_3D]): StatisticalMeshModel = {
    // TODO, this method could be generally useful.

    val pcaModelSpec = pcaModel.gp match {
      case specializedGp: SpecializedLowRankGaussianProcess[_3D] => pcaModel
      case gp => new StatisticalMeshModel(pcaModel.mesh, gp.specializeForPoints(pcaModel.mesh.points.toIndexedSeq))
    }
    val biasModelSpec = biasModel match {
      case specializedGp: SpecializedLowRankGaussianProcess[_3D] => biasModel
      case gp => gp.specializeForPoints(pcaModel.mesh.points.toIndexedSeq)

    }

    val newMean = (x: Point[_3D]) => pcaModelSpec.gp.mean(x) + biasModelSpec.mean(x)
    val newCov = pcaModelSpec.gp.cov + biasModelSpec.cov
    val numBasisFunctions = pcaModelSpec.gp.rank + biasModelSpec.rank
    val nystromSampler = FixedPointsUniformMeshSampler3D(pcaModelSpec.mesh, 2 * numBasisFunctions, 42)  
    val newGP = LowRankGaussianProcess.createLowRankGaussianProcess(pcaModelSpec.gp.domain, nystromSampler, newMean, newCov, numBasisFunctions)
    new StatisticalMeshModel(pcaModelSpec.mesh, newGP)
  }
  
  /**
   * A sampler that samples all the points of a mesh
   */

  private case class DefaultPCAMeshSampler(mesh: TriangleMesh) extends MeshSampler {
    override val numberOfPoints = mesh.numberOfPoints
    val p = 1.0 / mesh.area // should be replaced with real mesh volume
    override val volumeOfSampleRegion = mesh.area
    override def sample = mesh.points.toIndexedSeq.map(pt => (pt, p))
  }

  
  /**
   * Returns a PCA model with given reference mesh and a set of items in correspondence
   * 
   * @param dc DataCollection containing registered dataset
   * @param sampler A sampler to select points to be considered for computing the PCA 
   */
  def buildModelFromDataCollection(dc: DataCollection, sampler: MeshSampler): Try[StatisticalMeshModel] = {
    buildModelFromTransformations(dc.reference, dc.dataItems.map(_.transformation), Some(sampler))
  }

  /**
   * Returns a PCA model with given reference mesh and a set of items in correspondence.  
   * All points of the reference mesh are considered for computing the PCA 
   */
  def buildModelFromDataCollection(dc: DataCollection): Try[StatisticalMeshModel] = {
    buildModelFromTransformations(dc.reference, dc.dataItems.map(_.transformation), None)
  }


  private def buildModelFromTransformations(referenceMesh: TriangleMesh, transformations: Seq[Transformation[_3D]], maybeSampler: Option[MeshSampler]): Try[StatisticalMeshModel] = {
    if (transformations.size < 3) return Failure(new Throwable(s"We need to have at least 3 transformations to build a PCA Model (${transformations.size} provied"))
    val sampler = maybeSampler.getOrElse(DefaultPCAMeshSampler(referenceMesh))   
    val gp = LowRankGaussianProcess.createLowRankGPFromTransformations(referenceMesh.boundingBox, transformations, sampler)
    Success(new StatisticalMeshModel(referenceMesh, gp))
  }





  
}