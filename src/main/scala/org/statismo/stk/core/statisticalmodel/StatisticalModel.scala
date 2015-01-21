package org.statismo.stk.core
package statisticalmodel

import org.statismo.stk.core.mesh.TriangleMesh
import breeze.linalg.{DenseVector, DenseMatrix}
import org.statismo.stk.core.common.{PointData, FiniteDiscreteDomain, ImmutableLRU}
import org.statismo.stk.core.geometry._
import org.statismo.stk.core.registration.RigidTransformation


/**
 * A StatisticalMeshModel, as it is currently defined, is a mesh, together with a Gaussian process defined (at least) on the bounding box of the mesh
 */
case class StatisticalMeshModel(val referenceMesh: TriangleMesh, meanVector : DenseVector[Float], lambdas : DenseVector[Float], basisMatrix : DenseMatrix[Float])
{

  val gp = new DiscreteLowRankGaussianProcess[_3D, _3D](referenceMesh, meanVector, lambdas, basisMatrix)

  def mean : TriangleMesh = {
    val newPoints = gp.mean.pointsWithValues.map{case (pt, v) => pt + v}
    TriangleMesh(newPoints.toIndexedSeq, referenceMesh.cells)
  }


  def sample = {
    val newPoints = gp.sample.pointsWithValues.map{case (pt, v) => pt + v}
    TriangleMesh(newPoints.toIndexedSeq, referenceMesh.cells)

  }

  def instance(c : DenseVector[Float]) : TriangleMesh = {
    val newPoints = gp.instance(c).pointsWithValues.map{case (pt, v) => pt + v}
    TriangleMesh(newPoints.toIndexedSeq, referenceMesh.cells)
  }

  def marginal(ptId : Int)  = gp.marginal(ptId)

  def coefficients(trainingData : IndexedSeq[(Int, Point[_3D])], sigma2 : Double) : DenseVector[Float] = ??? /*{
    val d = referenceMesh(3)
    val tdWithVectors = trainingData.map{case (ptId, pt) => (ptId, pt - referenceMesh(ptId))}
    coefficients(tdWithVectors, sigma2)
  }
*/
  def posterior(trainingData: IndexedSeq[(Point[_3D], Vector[_3D])], sigma2: Double, meanOnly: Boolean = false): StatisticalMeshModel = ??? /*{
    val posteriorGP = GaussianProcess.regression(gp, trainingData, sigma2, meanOnly)
    new StatisticalMeshModel(mesh, posteriorGP)
  }
  */

}

object StatisticalMeshModel {

    def apply(referenceMesh: TriangleMesh, gp: LowRankGaussianProcess[_3D, _3D]): StatisticalMeshModel = {
      val points = referenceMesh.points.toSeq

      // precompute all the at the given points
      val (gpLambdas, gpPhis) = gp.eigenPairs.unzip
      val m = DenseVector.zeros[Float](points.size * gp.outputDimensionality)
      for (xWithIndex <- points.zipWithIndex.par) {
        val (x, i) = xWithIndex
        m(i * gp.outputDimensionality until (i + 1) * gp.outputDimensionality) := gp.mean(x).toBreezeVector
      }

      val U = DenseMatrix.zeros[Float](points.size * gp.outputDimensionality, gp.rank)
j      for (xWithIndex <- points.zipWithIndex.par; (phi_j, j) <- gpPhis.zipWithIndex) {
        val (x, i) = xWithIndex
        val v = phi_j(x)
        U(i * gp.outputDimensionality until (i + 1) * gp.outputDimensionality, j) := phi_j(x).toBreezeVector
      }

      val lambdas = new DenseVector[Float](gpLambdas.toArray)
      new StatisticalMeshModel(referenceMesh, m, lambdas, U)
    }




  /**
   * create a statisticalMeshModel which is transformed by the given rigid transform
   * TODO - Part of this functionality should be moved into the GP. But for this we would have to define
   * a proper domain-warp concept!
   */


  def transform(model: StatisticalMeshModel, rigidTransform: RigidTransformation[_3D]): StatisticalMeshModel =  ??? /*{
    val invTransform = rigidTransform.inverse
    val gp = model.gp

    val newRef = model.mesh.warp(rigidTransform)

    def newMean(pt: Point[_3D]): Vector[_3D] = {
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
    val newGp = new DiscreteLowRankGaussianProcess[_3D, _3D](newRef, newMean, newEigenpairs)

    new StatisticalMeshModel(newRef, newGp)
  }
  */

  def changeMesh(model: StatisticalMeshModel, newRef: TriangleMesh): StatisticalMeshModel = ??? /*{
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
    val newGp = new LowRankGaussianProcess3D(newRef.boundingBox, newMean, newEigenpairs)
    new StatisticalMeshModel(newRef, newGp)
  } */
}

