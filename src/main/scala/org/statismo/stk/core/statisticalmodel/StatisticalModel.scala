package org.statismo.stk.core
package statisticalmodel

import org.statismo.stk.core.mesh.TriangleMesh
import breeze.linalg.{ DenseVector, DenseMatrix }
import org.statismo.stk.core.common.ImmutableLRU
import org.statismo.stk.core.geometry._
import org.statismo.stk.core.registration.{ RigidTransformation3D, Transformation, RigidTransformationSpace3D }

/**
 * A StatisticalMeshModel, as it is currently defined, is a mesh, together with a Gaussian process defined (at least) on the bounding box of the mesh
 */
class StatisticalMeshModel(val mesh: TriangleMesh, val gp: LowRankGaussianProcess[_3D]) {

  def posterior(trainingData: IndexedSeq[(Point[_3D], Vector[_3D])], sigma2: Double, meanOnly: Boolean = false): StatisticalMeshModel = {
    val posteriorGP = GaussianProcess.regression(gp, trainingData, sigma2, meanOnly)
    new StatisticalMeshModel(mesh, posteriorGP)
  }

  /**
   * return the mean mesh represented by this gp
   */
  def mean: TriangleMesh = {
    val zeroVec = DenseVector.zeros[Float](gp.rank)
    instance(zeroVec)
  }

  /**
   *  draw a random sample
   */
  def sample: TriangleMesh = {
    gp match {
      case sgp: SpecializedLowRankGaussianProcess[_3D] => {
        val dfsAtPoints = sgp.sampleAtPoints
        TriangleMesh(dfsAtPoints.map { case (x, df) => x + df }, mesh.cells)
      }
      case _ => {
        val df = gp.sample
        mesh.warp((x: Point[_3D]) => x + df(x))

      }
    }
  }

  /**
   * draw an instance with the given coefficients
   */
  def instance(coeffs: DenseVector[Float]): TriangleMesh = {
    gp match {
      case sgp: SpecializedLowRankGaussianProcess[_3D] => {
        val dfsAtPoints = sgp.instanceAtPoints(coeffs)
        TriangleMesh(dfsAtPoints.map { case (x, df) => x + df }, mesh.cells)
      }
      case _ => {
        val df = gp.instance(coeffs)
        mesh.warp((x: Point[_3D]) => x + df(x))
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
    var closestPointCache = ImmutableLRU[Point[_3D], (Point[_3D], Int)](1000)

    def findClosestPointMemoized(pt: Point[_3D]) = {
        val (maybeClosestPt, newClosestPointCache) = closestPointCache.get(pt)
        maybeClosestPt.getOrElse {
          val closestPtWithId = mesh.findClosestPoint(pt)
          closestPointCache = (closestPointCache + (pt, closestPtWithId))._2 // ignore evicted key        
          closestPtWithId
        }
      }

    def mean(pt: Point[_3D]): Vector[_3D] = {
        val (closestPt, closestPtId) = findClosestPointMemoized(pt)
      Vector(meanVec(closestPtId * 3), meanVec(closestPtId * 3 + 1), meanVec(closestPtId * 3 + 2))
      }

    def phi(i: Int)(pt: Point[_3D]): Vector[_3D] = {
        val (closestPt, closestPtId) = findClosestPointMemoized(pt)
      Vector(phiMat(closestPtId * 3, i), phiMat(closestPtId * 3 + 1, i), phiMat(closestPtId * 3 + 2, i))
      }

      val eigenPairs = (0 until numPCAComponents) map (i => (pcaVariance(i), phi(i)_))
      val gp = new LowRankGaussianProcess3D(mesh.boundingBox, mean, eigenPairs)
      val lambdas = pcaVariance.toArray.toIndexedSeq

      // the most common use case is, that we evaluate the process as the mehs points. Hence we specialize on them
      val specializedGP = new SpecializedLowRankGaussianProcess(gp, mesh.points.toIndexedSeq, meanVec, lambdas, phiMat)
      new StatisticalMeshModel(mesh, specializedGP)
    }

    /**
     * create a statisticalMeshModel which is transformed by the given rigid transform
     * TODO - Part of this functionality should be moved into the GP. But for this we would have to define
     * a proper domain-warp concept!
     */
    def transform(model: StatisticalMeshModel, rigidTransform: RigidTransformation3D): StatisticalMeshModel = {
      val invTransform = rigidTransform.inverse
      val gp = model.gp
      val (lambdas, phis) = gp.eigenPairs.unzip
      val newRef = model.mesh.warp(rigidTransform)

    def newMean(pt : Point[_3D]) : Vector[_3D] = {
        val ptOrigGp = invTransform(pt)
        rigidTransform(ptOrigGp + gp.mean(ptOrigGp)) - rigidTransform(ptOrigGp)
      }

      val newPhis = phis.map(phi => {
      def newPhi(pt: Point[_3D]): Vector[_3D] = {
          val ptOrigGp = invTransform(pt)
          rigidTransform(ptOrigGp + phi(ptOrigGp)) - pt
        }
        newPhi _
      })

      val newEigenpairs = lambdas.zip(newPhis)
      val newGp = new LowRankGaussianProcess3D(newRef, newMean, newEigenpairs)

      new StatisticalMeshModel(newRef, newGp)
    }

    def changeMesh(model: StatisticalMeshModel, newRef: TriangleMesh): StatisticalMeshModel = {
      val gp = model.gp
      val (lambdas, phis) = gp.eigenPairs.unzip
      
      def correspondingPointOnOldMesh(newMeshPt: Point[_3D]): Point[_3D] = {
        val ptId = newRef.findClosestPoint(newMeshPt)._2
        model.mesh.points(ptId)
      }

      def newMean(pt: Point[_3D]): Vector[_3D] = {
        val ptOnOldRef = correspondingPointOnOldMesh(pt)
        ptOnOldRef + gp.mean(ptOnOldRef) - pt
      }

      val newPhis = phis.map(phi => {
        def newPhi(pt: Point[_3D]): Vector[_3D] = {
          val ptOnOldRef = correspondingPointOnOldMesh(pt)
          phi(ptOnOldRef)
        }
        newPhi _
      })

      val newEigenpairs = lambdas.zip(newPhis)
      val newGp = new LowRankGaussianProcess3D(newRef, newMean, newEigenpairs)
      new StatisticalMeshModel(newRef, newGp)
    }
  }

