package smptk
package statisticalmodel

import smptk.mesh.TriangleMesh
import smptk.image.Geometry._
import breeze.linalg.{ DenseVector, DenseMatrix }
import smptk.common.ImmutableLRU

/**
 * A StatisticalMeshModel, as it is currently defined, is a mesh, together with a Gaussian process defined (at least) on the bounding box of the mesh
 */
class StatisticalMeshModel(val mesh: TriangleMesh, val gp: LowRankGaussianProcess3D) {

  private val meshBB = mesh.boundingBox
  private val gpDomain = gp.domain
  require(meshBB.origin(0) <= gpDomain.origin(0)
    && meshBB.origin(1) <= gpDomain.origin(1)
    && meshBB.origin(2) <= gpDomain.origin(2)
    && meshBB.extent(0) >= gpDomain.extent(0)
    && meshBB.extent(1) >= gpDomain.extent(1)
    && meshBB.extent(2) >= gpDomain.extent(2))  
  
}

object StatisticalMeshModel {
  
  /** create a new statisticalMeshModel from a mesh and the mean, pca components defined for each mesh point. 
   * This process is then extended to the full bounding box of the mesh, where each point of the bounding box has the value
   * of its closest point on the mesh
   */ 
  def apply(mesh: TriangleMesh, meanVec: DenseVector[Double], pcaVariance: DenseVector[Double], phiMat: DenseMatrix[Double]) = {

    require(mesh.domain.numberOfPoints * 3 == meanVec.size)
    require(meanVec.size == phiMat.rows)
    require(pcaVariance.size == phiMat.cols)

    val numPCAComponents = phiMat.cols

    @volatile
    var closestPointCache = ImmutableLRU[CoordVector3D[Double], (CoordVector3D[Double], Int)](1000)

    def findClosestPointMemoized(pt: CoordVector3D[Double]) = {
      val (maybeClosestPt, newClosestPointCache) = closestPointCache.get(pt)
      maybeClosestPt.getOrElse {
    	  val closestPtWithId = mesh.findClosestPoint(pt)
    	  closestPointCache = (closestPointCache + (pt, closestPtWithId))._2 // ignore evicted key        
    	  closestPtWithId
      }
    }

    
    def mean(pt: CoordVector3D[Double]): DenseVector[Double] = {
      val (closestPt, closestPtId) = findClosestPointMemoized(pt)
      meanVec(closestPtId * 3 until (closestPtId + 1) * 3)
    }

    def phi(i : Int)(pt: CoordVector3D[Double]): DenseVector[Double] = {
      val (closestPt, closestPtId) = findClosestPointMemoized(pt)
      DenseVector(phiMat(closestPtId  * 3, i), phiMat(closestPtId  * 3 + 1, i), phiMat(closestPtId  * 3 + 2, i)) 
    }

    val eigenPairs = (0 until numPCAComponents) map (i => (pcaVariance(i), phi(i)_))

    new StatisticalMeshModel(mesh, new LowRankGaussianProcess3D(mesh.boundingBox, 3, mean, eigenPairs))
  }
}