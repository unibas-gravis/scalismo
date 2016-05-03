package scalismo.GeneralGaussianField

import breeze.linalg.{ DenseMatrix, DenseVector }
import scalismo.common._
import scalismo.geometry._
import scalismo.geometry.NDSpace
import scalismo.numerics.PivotedCholesky.NumberOfEigenfunctions
import scalismo.numerics.{ PivotedCholesky, Sampler }
import scalismo.statisticalmodel.MultivariateNormalDistribution

//import scala.collection.immutable.IndexedSeq
import scala.collection.mutable

//trait Domain[A] {
//  def isDefinedAt(a: A): Boolean
//}
//
//
//class ContinuousDomain[A]()
//  extends Domain[A] {
//  override def isDefinedAt(a: A): Boolean = ???
//}
//
//object ContinuousDomain {
//  def calculateBoxDomain[A]: (DiscreteDomain[A]) => ContinuousDomain[A] = ???
//}
//
//
//case class DiscreteDomain[A]( pointSeq: IndexedSeq[A] )
//  extends Domain[A] {
//  override def isDefinedAt(a: A): Boolean = pointSeq.contains(a)
//
//  def numberOfPoints: Int = pointSeq.length
//
//  def points: Iterator[A] = pointSeq.iterator
//
//  def point(id: PointId): A = pointSeq(id.id)
//
//  def pointId(a: A): PointId = PointId(pointSeq.indexOf(a))
//
//}

//class UnstructuredPointsDomain( pointSeq: IndexedSeq[Point[_3D]])
//  extends DiscreteDomain[Point[_3D]]( pointSeq ) {
//  private[this] lazy val kdTreeMap = KDTreeMap.fromSeq(pointSeq.zipWithIndex)
//  private[this] lazy val pointIDMap = pointSeq.zipWithIndex.map { case (pt, id) => (pt, PointId(id)) }.toMap
//
//  override def isDefinedAt( pt: Point[_3D] ) = pointIDMap.contains(pt)
//
//  override def pointId(pt: Point[_3D]): Option[PointId] = {
//    pointIDMap.get(pt)
//  }
//
//  def findClosestPoint(pt: Point[_3D]): (PointId,Point[_3D]) = {
//    def kdtreeLookup(pt: Point[_3D]): (PointId,Point[_3D]) = {
//      val (nearestPt, nearestInd) = kdTreeMap.findNearest(pt, n = 1).head
//      (PointId(nearestInd),nearestPt)
//    }
//
//    // first we check if the point is part of the domain (i.e. we get a pointId for the point).
//    // if not, we do a KDtree lookup, which is more expensive
//    pointId(pt) match {
//      case Some(id) => (id, pt)
//      case None => kdtreeLookup(pt)
//    }
//  }
//
//
//}

//case class Field[D, Value]( domain: Domain[D],
//                            f: Point[D] => Value) {
//
//}
//
//
//class ContinuousField[D, Value]( domain: RealSpace[D],
//                                 f: Point[D] => Value ) {
//  def apply(p: Point[D]): Value = {
//    if (!domain.isDefinedAt(p)) throw new IllegalArgumentException(s"Point $p is outside the domain")
//    f(p)
//  }
//}
//
//
//class DiscreteField[D, Value](domain: DiscreteDomain[D],
//                              f: Point[D] => Value) {
//  def apply(p: Point[D]): Value = {
//    if (!domain.isDefinedAt(p)) throw new IllegalArgumentException(s"Point $p is outside the domain")
//    f(p)
//  }
//}

class ContinuousMatrixValuedKernel[D <: Dim: NDSpace](val domain: Domain[D],
    val k: (Point[D], Point[D]) => DenseMatrix[Double],
    val outputDim: Int) {
  def apply(i: Point[D], j: Point[D]): DenseMatrix[Double] = {
    if (domain.isDefinedAt(i) && domain.isDefinedAt(j))
      k(i, j)
    else {
      if (!this.domain.isDefinedAt(i)) {
        throw new IllegalArgumentException(s"$i is outside of the domain")
      } else {
        throw new IllegalArgumentException(s"$j is outside of the domain")
      }
    }
  }
}

object ContinuousMatrixValuedKernel {

  def computeKernelMatrix[D <: Dim: NDSpace](xs: IndexedSeq[Point[D]], k: ContinuousMatrixValuedKernel[D]): DenseMatrix[Double] = {

    val d = k.outputDim

    val dim = xs.size * d
    val K = DenseMatrix.zeros[Double](dim, dim)

    var i = 0
    while (i < xs.size) {
      var j = i
      while (j < xs.size) {

        val kxixj = k(xs(i), xs(j))
        var di = 0
        while (di < d) {
          var dj = 0
          while (dj < d) {
            K(i * d + di, j * d + dj) = kxixj(di, dj)
            K(j * d + dj, i * d + di) = kxixj(di, dj)
            dj += 1
          }
          di += 1
        }
        j += 1
      }
      i += 1
    }

    K
  }

  /**
   * for every domain point x in the list, we compute the kernel vector
   * kx = (k(x, x1), ... k(x, xm))
   * since the kernel is matrix valued, kx is actually a matrix
   *
   * !! Hack - We currently return a double matrix, with the only reason that matrix multiplication (further down) is
   * faster (breeze implementation detail). This should be replaced at some point
   */
  def computeKernelVectorFor[D <: Dim: NDSpace](x: Point[D], xs: IndexedSeq[Point[D]], k: ContinuousMatrixValuedKernel[D]): DenseMatrix[Double] = {
    val d = k.outputDim

    val kxs = DenseMatrix.zeros[Double](d, xs.size * d)

    var j = 0
    while (j < xs.size) {
      var di = 0
      val kxxj = k(x, xs(j))
      while (di < d) {
        var dj = 0
        while (dj < d) {
          kxs(di, j * d + dj) = kxxj(di, dj)
          dj += 1
        }
        di += 1
      }
      j += 1
    }

    kxs
  }

}

class DiscreteMatrixValuedKernel[D <: Dim: NDSpace](val domain: DiscreteDomain[D],
    val k: (PointId, PointId) => DenseMatrix[Double],
    val outputDim: Int) {
  def isDefinedAt(i: PointId) = {
    (i.id >= 0) && (i.id < domain.numberOfPoints)
  }

  def apply(i: PointId, j: PointId): DenseMatrix[Double] = {
    if (isDefinedAt(i) && isDefinedAt(j))
      k(i, j)
    else {
      if (!isDefinedAt(i)) {
        throw new IllegalArgumentException(s"$i is outside of the domain")
      } else {
        throw new IllegalArgumentException(s"$j is outside of the domain")
      }
    }
  }

  /**
   * return the matrix representation of this kernel.
   * (This is a covariance matrix, consisting of blocks of size DO times DO)
   */
  def asBreezeMatrix: DenseMatrix[Double] = {
    val d = outputDim
    val xs = domain.points.toIndexedSeq

    val K = DenseMatrix.zeros[Double](xs.size * d, xs.size * d)
    val xiWithIndex = xs.zipWithIndex.par
    val xjWithIndex = xs.zipWithIndex
    for { i <- xs.indices; j <- 0 to i } {
      val kxixj = k(PointId(i), PointId(j))
      var di = 0
      while (di < d) {
        var dj = 0
        while (dj < d) {
          K(i * d + di, j * d + dj) = kxixj(di, dj)
          K(j * d + dj, i * d + di) = K(i * d + di, j * d + dj)
          dj += 1
        }
        di += 1
      }
    }
    K
  }

}

object DiscreteMatrixValuedKernel {
  private def basisMatrixToCov[D <: Dim: NDSpace](domain: DiscreteDomain[D],
    variance: DenseVector[Double],
    basisMatrix: DenseMatrix[Double]) = {

    val outputDimensionen = implicitly[NDSpace[D]].dimensionality
    def cov(ptId1: PointId, ptId2: PointId): DenseMatrix[Double] = {

      val eigenMatrixForPtId1 = basisMatrix(ptId1.id * outputDimensionen until (ptId1.id + 1) * outputDimensionen, ::)
      val eigenMatrixForPtId2 = basisMatrix(ptId2.id * outputDimensionen until (ptId2.id + 1) * outputDimensionen, ::)
      //val covValue = eigenMatrixForPtId1 * breeze.linalg.diag(stddev :* stddev) * eigenMatrixForPtId2.t

      // same as commented line above, but just much more efficient (as breeze does not have diag matrix,
      // the upper command does a lot of  unnecessary computations
      val covValue = DenseMatrix.zeros[Double](outputDimensionen, outputDimensionen)

      for (i <- (0 until outputDimensionen).par) {
        val ind1 = ptId1.id * outputDimensionen + i
        var j = 0
        while (j < outputDimensionen) {
          val ind2 = ptId2.id * outputDimensionen + j
          var k = 0
          var valueIJ = 0.0
          while (k < basisMatrix.cols) {
            valueIJ += basisMatrix(ind1, k) * basisMatrix(ind2, k) * variance(k)
            k += 1
          }
          covValue(i, j) = valueIJ
          j += 1
        }
      }

      covValue
    }

    new DiscreteMatrixValuedKernel(domain, cov, outputDimensionen)
  }
}

case class DiscreteGaussianField[D <: Dim: NDSpace, Value](mean: DiscreteField[D, Value],
    cov: DiscreteMatrixValuedKernel[D],
    representer: Vectorizer[Value],
    domain: UnstructuredPointsDomain[D]) {
  private val innerDim = representer.dim
  private val outerDim = domain.numberOfPoints
  private val dim = outerDim * innerDim

  private val meanVec: DenseVector[Double] = {
    val points = mean.data.toIndexedSeq
    representer.vectorize(points)
  }

  private val covMat: DenseMatrix[Double] = {
    val C = DenseMatrix.zeros[Double](dim, dim)
    val pt = domain.points.toIndexedSeq.zipWithIndex
    for (i <- pt; j <- pt) {
      val c = cov(PointId(i._2), PointId(j._2))
      for (x <- 0 until innerDim; y <- 0 until innerDim) {
        C(i._2 * innerDim + x, j._2 * innerDim + y) = c(x, y)
      }
    }
    C
  }

  private val mvd: MultivariateNormalDistribution = MultivariateNormalDistribution(meanVec, covMat)

  def sample(): DiscreteField[D, Value] = {
    val sample = mvd.sample()
    val vals = sample.toArray.grouped(innerDim).map(a => representer.unvectorize(DenseVector(a))).toIndexedSeq
    new DiscreteField(mean.domain, vals)
  }

  def marginalAtPoint(pt: Point[D])(implicit creator: CreateUnstructuredPointsDomain[D]): DiscreteGaussianField[D, Value] = {
    val newDomain = UnstructuredPointsDomain(IndexedSeq(pt))
    val newMeanField = new DiscreteField[D, Value](newDomain, IndexedSeq(mean(domain.findClosestPoint(pt).id)))
    val newCov = new DiscreteMatrixValuedKernel[D](newDomain, (pt: PointId, j: PointId) => cov(pt, pt), cov.outputDim)
    DiscreteGaussianField[D, Value](newMeanField, newCov, representer, newDomain)
  }

  def project(sample: DiscreteField[D, Value]): DiscreteField[D, Value] = ???

  def logpdf(sample: DiscreteField[D, Value]): Double = ???

}

trait FunctionInterpolator[D <: Dim, Value] {
  def apply(domain: Domain[D], f: (Point[D]) => Value): (Point[D]) => Value
}

trait CovarianceInterpolator[D <: Dim, Value] {
  def apply(domain: Domain[D], f: (Point[D], Point[D]) => Value): (Point[D], Point[D]) => DenseMatrix[Double]
}

object DiscreteGaussianField {

  def regression[D <: Dim: NDSpace, Value](gp: DiscreteGaussianField[D, Value],
    observations: IndexedSeq[(D, Value, MultivariateNormalDistribution)]): DiscreteGaussianField[D, Value] = {
    ???
  }

  def interpolate[D <: Dim: NDSpace, Value](gp: DiscreteGaussianField[D, Value]): GaussianField[D, Value] = {
    val newField = gp.mean.interpolateNearestNeighbor()
    val newCov = (pt1: Point[D], pt2: Point[D]) => {
      val closestPt1 = gp.domain.findClosestPoint(pt1).id
      val closestPt2 = gp.domain.findClosestPoint(pt2).id
      gp.cov(closestPt1, closestPt2)
    }
    val newKernel = new ContinuousMatrixValuedKernel[D](new RealSpace[D], newCov, gp.dim)
    GaussianField[D, Value](newField, newKernel, gp.representer, new RealSpace[D])
  }

}

case class GaussianField[D <: Dim: NDSpace, Value](mean: Field[D, Value],
    cov: ContinuousMatrixValuedKernel[D],
    representer: Vectorizer[Value],
    domain: RealSpace[D]) {
  //  def sample(): A => Value

  def sampleAtPoint(i: Point[D]): Value = {
    require(domain.isDefinedAt(i))
    val vmean = representer.vectorize(mean(i))
    val dist = MultivariateNormalDistribution(vmean, cov(i, i))
    representer.unvectorize(dist.sample())
  }

  def marginalAtPoint(i: Point[D])(implicit creator: CreateUnstructuredPointsDomain[D]): DiscreteGaussianField[D, Value] = {
    val domain = UnstructuredPointsDomain[D](IndexedSeq(i))
    new DiscreteGaussianField[D, Value](
      new DiscreteField[D, Value](domain, IndexedSeq(mean(i))),
      new DiscreteMatrixValuedKernel[D](domain, (_, _) => cov(i, i), cov.outputDim),
      representer,
      domain)
  }

  def posterior(observations: IndexedSeq[(Point[D], Value)], noise: Double): GaussianField[D, Value] = {
    val noiseMVN = MultivariateNormalDistribution(DenseVector.zeros[Double](representer.dim), DenseMatrix.eye[Double](representer.dim) * noise)
    val noisyObservations = observations.map(obs => (obs._1, obs._2, noiseMVN))
    GaussianField.regression[D, Value](this, noisyObservations)
  }

  def posterior(noisyObservations: IndexedSeq[(Point[D], Value, MultivariateNormalDistribution)]): GaussianField[D, Value] = {
    GaussianField.regression[D, Value](this, noisyObservations)
  }

}

object GaussianField {

  def regression[D <: Dim: NDSpace, Value](gp: GaussianField[D, Value],
    observations: IndexedSeq[(Point[D], Value, MultivariateNormalDistribution)]): GaussianField[D, Value] = {
    val outputDim = gp.representer.dim
    val (obsA, obsValues, obsUncertainty) = observations.unzip3

    val mVec = gp.representer.vectorize(obsA.map(i => gp.mean(i)))
    val yVec = gp.representer.vectorize(obsValues)
    val fVec = yVec - mVec

    val K = ContinuousMatrixValuedKernel.computeKernelMatrix[D](obsA, gp.cov)
    for ((errorDist, i) <- obsUncertainty.zipWithIndex) {
      K(i * outputDim until (i + 1) * outputDim, i * outputDim until (i + 1) * outputDim) += errorDist.cov
    }

    val K_inv = breeze.linalg.inv(K)

    def xstar(x: Point[D]) = {
      ContinuousMatrixValuedKernel.computeKernelVectorFor[D](x, obsA, gp.cov)
    }

    def posteriorMean(x: Point[D]): Value = {
      gp.representer.unvectorize((xstar(x) * K_inv) * fVec)
    }

    val posteriorKernel = new ContinuousMatrixValuedKernel[D](
      gp.domain,
      (x: Point[D], y: Point[D]) => {
        gp.cov(x, y) - DenseMatrix((xstar(x) * K_inv * xstar(y).t).data)
      },
      gp.cov.outputDim
    )

    new GaussianField[D, Value](Field[D, Value](gp.domain, posteriorMean _), posteriorKernel, gp.representer, gp.domain)
  }

}
