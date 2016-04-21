package scalismo.GeneralGaussianProcess

import breeze.linalg.{DenseMatrix, DenseVector}
import scalismo.common._
import scalismo.geometry._3D
import scalismo.geometry.Point
import scalismo.geometry.Vector
import scalismo.kernels.MatrixValuedPDKernel
import scalismo.mesh.kdtree.KDTreeMap
import scalismo.statisticalmodel.MultivariateNormalDistribution

import scala.collection.immutable.IndexedSeq
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


case class MatrixValuedKernel[D]( domain: Domain[D],
                                  k: (Point[D], Point[D]) => DenseMatrix[Double],
                                  outputDim: Int) {

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


class ContinuousMatrixValuedKernel[D]( val domain: Domain[D],
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


class DiscreteMatrixValuedKernel[D]( val domain: DiscreteDomain[D],
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

object Kernel {

  def computeKernelMatrix[D](xs: IndexedSeq[Point[D]], k: MatrixValuedKernel[D]): DenseMatrix[Double] = {

    val d = k.outputDim

    val K = DenseMatrix.zeros[Double](xs.size * d, xs.size * d)

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
  def computeKernelVectorFor[D](x: Point[D], xs: IndexedSeq[Point[D]], k: MatrixValuedKernel[D]): DenseMatrix[Double] = {
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


trait Vectorizer[Value] {
  def dim: Int

  def vectorize( v: Value ) : DenseVector[Double]
  def unvectorize( d: DenseVector[Double] ) : Value

  def vectorize( vs: IndexedSeq[Value] ): DenseVector[Double] = {
    val fullDim = vs.length*dim
    val M = DenseVector.zeros[Double](fullDim)
    val pt = vs.zipWithIndex
    for( i <- pt) {
      val m = vectorize(i._1)
      for( x <- 0 until dim) {
        M(i._2*dim+x) = m(x)
      }
    }
    M
  }

}


case class DiscreteGaussianProcess[D, Value] ( mean: DiscreteField[D,Value],
                                               cov: DiscreteMatrixValuedKernel[D],
                                               representer: Vectorizer[Value],
                                               domain: UnstructuredPointsDomain[D] )
{
  private val innerDim = representer.dim
  private val outerDim = domain.numberOfPoints
  private val dim = outerDim*innerDim

  private val meanVec: DenseVector[Double] = {
    val points = domain.points.map(p => mean(p)).toIndexedSeq
    representer.vectorize(points)
  }

  private val covMat: DenseMatrix[Double] = {
    val C = DenseMatrix.zeros[Double](dim,dim)
    val pt = domain.points.toIndexedSeq.zipWithIndex
    for( i <- pt; j <- pt) {
      val c = cov(i._1,j._1)
      for( x <- 0 until innerDim; y <- 0 until innerDim) {
        C(i._2*innerDim+x, j._2*innerDim+y) = c(x,y)
      }
    }
    C
  }

  private val mvd: MultivariateNormalDistribution = MultivariateNormalDistribution(meanVec,covMat)

  def sample(): DiscreteField[D, Value] = {
    val sample = mvd.sample()
    val vals = sample.toArray.grouped(innerDim).map(a => representer.unvectorize(DenseVector(a))).toIndexedSeq
    new DiscreteField(mean.domain,vals)
  }

  def marginalAtPoint( pt: Point[D] ) : DiscreteGaussianProcess[D,Value] = {
    val newDomain = UnstructuredPointsDomain[D](IndexedSeq(pt))
    val newMeanField = new DiscreteField[D,Value](newDomain,IndexedSeq(mean(domain.findClosestPoint(pt).id)))
    val newCov = new DiscreteMatrixValuedKernel[D](newDomain,(pt:Point[D],j:Point[D]) => cov(pt,pt),cov.outputDim)
    DiscreteGaussianProcess[D,Value](newMeanField,newCov,representer,newDomain)
  }

  def project( sample: DiscreteField[D,Value] ): DiscreteField[D,Value] = ???

  def logpdf( sample: DiscreteField[D,Value] ): Double = ???

}

trait FunctionInterpolator[D,Value] {
  def apply ( domain: Domain[D], f: (Point[D]) => Value): (Point[D]) => Value
}

trait CovarianceInterpolator[D,Value] {
  def apply ( domain: Domain[D], f: (Point[D],Point[D]) => Value): (Point[D],Point[D]) => DenseMatrix[Double]
}

object DiscreteGaussianProcess {

  def regression[D,Value]( gp: DiscreteGaussianProcess[D,Value],
                               observations: IndexedSeq[(D,Value,MultivariateNormalDistribution)]
                             ) : DiscreteGaussianProcess[D,Value] = {
    ???
  }



  def interpolate[D,Value]( gp: DiscreteGaussianProcess[D,Value],
                                interpolateFunction: FunctionInterpolator[D,Value],
                                interpolateCovariance: CovarianceInterpolator[D,Value] ): GaussianProcess[D,Value] = {
    val newField = gp.mean.interpolateNearestNeighbor()
    val newKernel = new MatrixValuedPDKernel[D, Value] {
      override val domain = newDomain

      override def k(pt1: Point[D], pt2: Point[D]): SquareMatrix[DO] = {
        val closestPtId1 = self.domain.findClosestPoint(pt1).id
        val closestPtId2 = self.domain.findClosestPoint(pt2).id
        cov(closestPtId1, closestPtId2)
      }
    }
    GaussianProcess[D,Value](newField, newKernel, gp.representer, RealSpace )
  }

}


case class GaussianProcess[A, Value] ( mean: Field[A,Value],
                                       cov: ContinuousMatrixValuedKernel[A],
                                       representer: Vectorizer[Value],
                                       domain: ContinuousDomain[A] )
{
//  def sample(): A => Value

  def sampleAtPoint(i: A) : Value = {
    require(domain.isDefinedAt(i))
    val vmean = representer.vectorize(mean(i))
    val dist = MultivariateNormalDistribution(vmean, cov(i,i))
    representer.unvectorize(dist.sample())
  }

  def marginalAtPoint(i: A) : DiscreteGaussianProcess[A,Value] = {
    val domain = DiscreteDomain[A](IndexedSeq(i))
    new DiscreteGaussianProcess[A,Value](
      new DiscreteField(domain,_=>mean(i)),
      new DiscreteMatrixValuedKernel[A](domain,(_,_)=>cov(i,i),cov.outputDim),
      representer,
      domain)
  }

  def posterior( observations: IndexedSeq[(A, Value)], noise: Double) : GaussianProcess[A,Value] = {
    val noiseMVN = MultivariateNormalDistribution(DenseVector.zeros[Double](representer.dim),DenseMatrix.eye[Double](representer.dim)*noise)
    val noisyObservations = observations.map( obs => (obs._1,obs._2,noiseMVN))
    GaussianProcess.regression[A,Value](this,noisyObservations)
  }

  def posterior( noisyObservations: IndexedSeq[(A, Value, MultivariateNormalDistribution)] ) : GaussianProcess[A,Value] = {
    GaussianProcess.regression[A,Value](this,noisyObservations)
  }

}


//object GaussianProcess {
//
//  def regression[A,Value]( gp: GaussianProcess[A,Value],
//                               observations: IndexedSeq[(A,Value,MultivariateNormalDistribution)]
//                             ): GaussianProcess[A,Value] = {
//    val outputDim = gp.representer.dim
//    val (obsA, obsValues, obsUncertainty) = observations.unzip3
//
//    val mVec = gp.representer.vectorize(obsA.map(i => gp.mean(i)))
//    val yVec = gp.representer.vectorize(obsValues)
//    val fVec = yVec - mVec
//
//    val K = Kernel.computeKernelMatrix(obsA, gp.cov)
//    for ((errorDist, i) <- obsUncertainty.zipWithIndex) {
//      K(i * outputDim until (i + 1) * outputDim, i * outputDim until (i + 1) * outputDim) += errorDist.cov
//    }
//
//    val K_inv = breeze.linalg.inv(K)
//
//    def xstar(x: A) = { Kernel.computeKernelVectorFor[A](x, obsA, gp.cov) }
//
//    def posteriorMean(x: A): Value = {
//      gp.representer.unvectorize( (xstar(x)*K_inv)*fVec )
//    }
//
//    val posteriorKernel = new ContinuousMatrixValuedKernel[A](
//      gp.domain,
//      (x: A,y: A) => {
//        gp.cov(x, y) - DenseMatrix((xstar(x) * K_inv * xstar(y).t).data)
//      },
//      gp.cov.outputDim
//    )
//
//    new GaussianProcess[A, Value]( Field[A,Value](gp.domain, posteriorMean _), posteriorKernel, gp.representer, gp.domain)
//  }
//
//}
