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

  def posterior(trainingData: IndexedSeq[(Point[ThreeD], Vector[ThreeD])], sigma2: Double, meanOnly: Boolean = false): StatisticalMeshModel = {
    val posteriorGP = GaussianProcess.regression(gp, trainingData, sigma2, meanOnly)
    new StatisticalMeshModel(mesh, posteriorGP)
  }

  /**
   * return the mean mesh represented by this gp
   */
  def mean : TriangleMesh = {
    val zeroVec = DenseVector.zeros[Float](gp.rank) 
    instance(zeroVec)
  }
  
  /**
   *  draw a random sample
   */
  def sample: TriangleMesh = {
    gp match {
      case sgp: SpecializedLowRankGaussianProcess[ThreeD] => {
        val dfsAtPoints = sgp.sampleAtPoints
        TriangleMesh(dfsAtPoints.map { case (x, df) => x + df }, mesh.cells)
      }
      case _ => {
        val df = gp.sample
        mesh.warp((x: Point[ThreeD]) => x + df(x))

      }
    }
  }

  /**
   * draw an instance with the given coefficients
   */
  def instance(coeffs: DenseVector[Float]): TriangleMesh = {
    gp match {
      case sgp: SpecializedLowRankGaussianProcess[ThreeD] => {
        val dfsAtPoints = sgp.instanceAtPoints(coeffs)
        TriangleMesh(dfsAtPoints.map { case (x, df) => x + df }, mesh.cells)
      }
      case _ => {
        val df = gp.instance(coeffs)
        mesh.warp((x: Point[ThreeD]) => x + df(x))

      }
    }
  }

}

object StatisticalMeshModel {

  /**
   * create a new statisticalMeshModel from a mesh and the mean, pca components defined for each mesh point.
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
      Vector3D(meanVec(closestPtId * 3), meanVec(closestPtId * 3 + 1), meanVec(closestPtId * 3 + 2))
    }

    def phi(i: Int)(pt: Point[ThreeD]): Vector[ThreeD] = {
      val (closestPt, closestPtId) = findClosestPointMemoized(pt)
      Vector3D(phiMat(closestPtId * 3, i), phiMat(closestPtId * 3 + 1, i), phiMat(closestPtId * 3 + 2, i))
    }

    val eigenPairs = (0 until numPCAComponents) map (i => (pcaVariance(i), phi(i)_))
    val gp = new LowRankGaussianProcess3D(mesh.boundingBox, mean, eigenPairs)
    val lambdas = pcaVariance.toArray.toIndexedSeq

    // the most common use case is, that we evaluate the process as the mehs points. Hence we specialize on them
    val specializedGP = new SpecializedLowRankGaussianProcess(gp, mesh.points.toIndexedSeq, meanVec, lambdas, phiMat)
    new StatisticalMeshModel(mesh, specializedGP)
  }
}