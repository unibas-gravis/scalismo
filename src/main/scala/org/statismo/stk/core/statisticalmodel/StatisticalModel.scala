package org.statismo.stk.core
package statisticalmodel

import org.statismo.stk.core.mesh.TriangleMesh
import breeze.linalg.{ DenseVector, DenseMatrix }
import org.statismo.stk.core.common.ImmutableLRU
import org.statismo.stk.core.geometry._

/**
 * A StatisticalMeshModel, as it is currently defined, is a mesh, together with a Gaussian process defined (at least) on the bounding box of the mesh
 */
class StatisticalMeshModel(val mesh: TriangleMesh, val gp: LowRankGaussianProcess[ThreeD]) {

  private val meshBB = mesh.boundingBox
  private val gpDomain = gp.domain
  require(meshBB.origin(0) >= gpDomain.origin(0)
    && meshBB.origin(1) >= gpDomain.origin(1)
    && meshBB.origin(2) >= gpDomain.origin(2)
    && meshBB.extent(0) <= gpDomain.extent(0)
    && meshBB.extent(1) <= gpDomain.extent(1)
    && meshBB.extent(2) <= gpDomain.extent(2))  
  
  def posterior(trainingData : IndexedSeq[(Point[ThreeD], Vector[ThreeD])], sigma2 : Double , meanOnly : Boolean = false) : StatisticalMeshModel = {
    val posteriorGP = GaussianProcess.regression(gp, trainingData, sigma2, meanOnly) 
    new StatisticalMeshModel(mesh, posteriorGP)
  }
  
}

object StatisticalMeshModel {
  
  /** create a new statisticalMeshModel from a mesh and the mean, pca components defined for each mesh point. 
   * This process is then extended to the full bounding box of the mesh, where each point of the bounding box has the value
   * of its closest point on the mesh
   */ 
  def apply(mesh: TriangleMesh, meanVec: DenseVector[Float], pcaVariance: DenseVector[Float], phiMat: DenseMatrix[Float]) = {

    require(mesh.numberOfPoints * 3 == meanVec.size)
    require(meanVec.size == phiMat.rows)
    require(pcaVariance.size == phiMat.cols)

    val numPCAComponents = phiMat.cols

    @volatile
    var closestPointCache = ImmutableLRU[Point[ThreeD], (Point[ThreeD], Int)](1000)

    def findClosestPointMemoized(pt: Point[ThreeD]) = {
      val (maybeClosestPt, newClosestPointCache) = closestPointCache.get(pt)
      maybeClosestPt.getOrElse {
    	  val closestPtWithId = mesh.findClosestPoint(pt)
    	  closestPointCache = (closestPointCache + (pt, closestPtWithId))._2 // ignore evicted key        
    	  closestPtWithId
      }
    }

    
    def mean(pt: Point[ThreeD]): Vector3D = {
      val (closestPt, closestPtId) = findClosestPointMemoized(pt)
      Vector3D(meanVec(closestPtId * 3), meanVec(closestPtId * 3 + 1), meanVec(closestPtId *3 + 2))
    }

    def phi(i : Int)(pt: Point[ThreeD]): Vector[ThreeD] = {
      val (closestPt, closestPtId) = findClosestPointMemoized(pt)
      Vector3D(phiMat(closestPtId  * 3, i), phiMat(closestPtId  * 3 + 1, i), phiMat(closestPtId  * 3 + 2, i)) 
    }

    val eigenPairs = (0 until numPCAComponents) map (i => (pcaVariance(i), phi(i)_))
    val gp = new LowRankGaussianProcess3D(mesh.boundingBox, mean, eigenPairs) 
    val lambdas = pcaVariance.toArray.toIndexedSeq
    
    // the most common use case is, that we evaluate the process as the mehs points. Hence we specialize on them
    val specializedGP = new SpecializedLowRankGaussianProcess(gp, mesh.points.toIndexedSeq, meanVec, lambdas, phiMat)
    new StatisticalMeshModel(mesh, specializedGP)
  }
}