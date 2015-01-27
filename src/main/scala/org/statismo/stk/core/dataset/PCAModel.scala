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
import org.statismo.stk.core.statisticalmodel.StatisticalMeshModel
import org.statismo.stk.core.statisticalmodel.DiscreteLowRankGaussianProcess
import org.statismo.stk.core.statisticalmodel.GaussianProcess
import org.statismo.stk.core.kernels.MatrixValuedPDKernel

/**
 * Implements utility functions for building a [[StatisticalMeshModel]] from a [[DataCollection]] containing a reference and items in correspondence 
 * */
object PCAModel {

  type MeshSampler = Sampler[_3D]

  
  /**
   *  Adds a bias model to the given pca model
   */
  private [dataset] def augmentModel(pcaModel: StatisticalMeshModel, biasModel: LowRankGaussianProcess[_3D, _3D]): StatisticalMeshModel = {
    
    val modelGP = pcaModel.gp.interpolate(500)          
    val newMean = (x: Point[_3D]) => modelGP.mean(x) + biasModel.mean(x)     
    val newKLBasis = modelGP.klBasis ++ biasModel.klBasis
    val newGP = new LowRankGaussianProcess(pcaModel.referenceMesh.boundingBox, newMean, newKLBasis)   
    val newDiscreteGP = newGP.discretize(pcaModel.referenceMesh.points.toIndexedSeq)
     
    StatisticalMeshModel(pcaModel.referenceMesh, newDiscreteGP)
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