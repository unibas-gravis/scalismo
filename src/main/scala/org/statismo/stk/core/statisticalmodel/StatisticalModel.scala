package org.statismo.stk.core
package statisticalmodel

import org.statismo.stk.core.mesh.TriangleMesh
import breeze.linalg.{DenseVector, DenseMatrix}
import org.statismo.stk.core.common.{VectorPointData, PointData, FiniteDiscreteDomain, ImmutableLRU}
import org.statismo.stk.core.geometry._
import org.statismo.stk.core.registration.RigidTransformation


/**
 * A StatisticalMeshModel, as it is currently defined, is a mesh, together with a Gaussian process defined (at least) on the bounding box of the mesh
 */
case class StatisticalMeshModel private (val referenceMesh: TriangleMesh, val gp : DiscreteLowRankGaussianProcess[_3D, _3D])
{

  val rank = gp.rank

  def mean : TriangleMesh = warpReference(gp.mean)

  def cov(ptId1 : Int, ptId2 : Int) = gp.cov(ptId1, ptId2)

  def sample = warpReference(gp.sample)

  def instance(c : DenseVector[Float]) : TriangleMesh = warpReference(gp.instance(c))

  def marginal(ptId : Int)  = gp.marginal(ptId)

  def coefficients(trainingData : IndexedSeq[(Int, Point[_3D])], sigma2 : Double) : DenseVector[Float] = {
    val trainingDataWithDisplacements = trainingData.map{case (id, targetPoint) => (id, targetPoint - referenceMesh(id))}
    gp.coefficients(trainingDataWithDisplacements, sigma2)
  }

  def posterior(trainingData: IndexedSeq[(Int, Point[_3D])], sigma2: Double): StatisticalMeshModel = {
    val trainingDataWithDisplacements = trainingData.map{case (id, targetPoint) => (id, targetPoint - referenceMesh(id))}
    val posteriorGp = gp.posterior(trainingDataWithDisplacements, sigma2)
    new StatisticalMeshModel(referenceMesh, posteriorGp)
  }


  def transform(rigidTransform: RigidTransformation[_3D]): StatisticalMeshModel =  {
    val newRef = referenceMesh.warp(rigidTransform)

    val newMean : DenseVector[Float] = {
      val newMeanVecs = for ((pt, meanAtPoint) <- gp.mean.pointsWithValues) yield {
        rigidTransform(pt + meanAtPoint) - rigidTransform(pt)
      }
      val data = newMeanVecs.map(_.data).flatten.toArray
      DenseVector(data)
    }

    val newBasisMat = DenseMatrix.zeros[Float](gp.basisMatrix.rows, gp.basisMatrix.cols)

    for (i <- 0 until gp.rank) {
      val newIthBasis = for ((pt, basisAtPoint) <- gp.basis(i).pointsWithValues) yield {
        rigidTransform(pt + basisAtPoint) - rigidTransform(pt)
      }
      val data = newIthBasis.map(_.data).flatten.toArray
      newBasisMat(::, i) := DenseVector(data)
    }
    val newGp = new DiscreteLowRankGaussianProcess[_3D, _3D](gp.domain, newMean, gp.variance, newBasisMat)

    new StatisticalMeshModel(newRef, newGp)

  }

  /** Changes the reference using the given transform
    */
  def changeReference(t : Point[_3D] => Point[_3D]): StatisticalMeshModel = {

    val newRef = referenceMesh.warp(t)
    val newMean = gp.mean.pointsWithValues.map{case(refPt, meanVec) => (refPt - t(refPt)) + meanVec}
    val newMeanVec = DenseVector(newMean.map(_.data).flatten.toArray)
    val newGp = new DiscreteLowRankGaussianProcess[_3D, _3D](newRef, newMeanVec, gp.variance, gp.basisMatrix)
    new StatisticalMeshModel(newRef, newGp)
  }


  private def warpReference(vectorPointData: VectorPointData[_3D, _3D]) = {
    val newPoints = vectorPointData.pointsWithValues.map{case (pt, v) => pt + v}
    new TriangleMesh(newPoints.toIndexedSeq, referenceMesh.cells, Some(referenceMesh.cellMap))
  }


}

object StatisticalMeshModel {

    def apply(referenceMesh: TriangleMesh, gp: LowRankGaussianProcess[_3D, _3D]): StatisticalMeshModel = {
      val discreteGp = DiscreteLowRankGaussianProcess(referenceMesh, gp)
      new StatisticalMeshModel(referenceMesh, discreteGp)
    }


  def apply(referenceMesh : TriangleMesh, meanVector : DenseVector[Float], lambdas : DenseVector[Float], basisMatrix : DenseMatrix[Float]) = {
    val gp = new DiscreteLowRankGaussianProcess[_3D, _3D](referenceMesh, meanVector, lambdas, basisMatrix)
    new StatisticalMeshModel(referenceMesh, gp)
  }





}

