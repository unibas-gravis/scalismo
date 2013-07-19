package smptk
package statisticalmodel

import image.CoordVector
import image.Geometry.{ CoordVector1D, CoordVector2D, CoordVector3D }
import common.BoxedRegion
import breeze.linalg.{ DenseVector, DenseMatrix, linspace }
import breeze.stats.distributions.Gaussian
import smptk.common.DiscreteDomain
import smptk.common.{ BoxedRegion1D, BoxedRegion2D, BoxedRegion3D }
import smptk.numerics.{ UniformSampler1D, UniformSampler2D, UniformSampler3D }
import smptk.numerics.Sampler
import smptk.io.MeshIO
import smptk.mesh.TriangleMesh
import smptk.image.Utils
import smptk.mesh.TriangleMeshDomain
import java.io.File
import smptk.numerics.UniformSampler1D
import smptk.numerics.UniformSampler
import smptk.kernels._
import smptk.numerics.UniformSampler1D
import breeze.plot.{ plot, Figure }
import smptk.image.DiscreteImageDomain
import smptk.common.ImmutableLRU
import scala.collection.immutable.HashMap

case class GaussianProcess[CV[A] <: CoordVector[A]](val domain: BoxedRegion[CV], val mean: CV[Double] => DenseVector[Double], val cov: PDKernel[CV]) {

  type PointSample = IndexedSeq[CV[Double]]

  def sample: (PointSample => IndexedSeq[DenseVector[Double]]) = { (xs: PointSample) =>
    {
      val n = xs.size
      val d = cov.outputDim
      val meanVec = DenseVector.zeros[Double](n * d)
      for (i <- 0 until n; di <- 0 until d) meanVec(i * d + di) = mean(xs(i))(di)
      val covMatrix = Kernel.computeKernelMatrix(xs, cov)
      val noise = breeze.linalg.diag(DenseVector.ones[Double](xs.size)) * 1e-6 // gaussian noise for stability 
      val lMat = breeze.linalg.cholesky(covMatrix + noise)
      val u = for (_ <- 0 until xs.size) yield breeze.stats.distributions.Gaussian(0, 1).draw()
      val uVec = DenseVector(u.toArray)
      val sampleVec = meanVec + lMat * uVec // the sample as a long vector

      // now we group it 
      // TODO make me more efficient and elegant
      val sampleSeq = sampleVec.toArray.toIndexedSeq
      val pointSampleValues = sampleSeq.grouped(d).map(ptVec => DenseVector(ptVec.toArray))
      pointSampleValues.toIndexedSeq
    }
  }
}

case class LowRankGaussianProcessConfiguration[CV[A] <: CoordVector[A]](
  val domain: BoxedRegion[CV],
  val mean: CV[Double] => DenseVector[Double],
  val cov: PDKernel[CV],
  val numBasisFunctions: Int,
  val numPointsForNystrom: Int)

trait LowRankGaussianProcess[CV[A] <: CoordVector[A]] {

  val domain: BoxedRegion[CV]
  val outputDim: Int
  val mean: CV[Double] => DenseVector[Double]
  val eigenPairs: IndexedSeq[(Double, CV[Double] => DenseVector[Double])]

  def rank = eigenPairs.size

  def instance(alpha: DenseVector[Double]): CV[Double] => DenseVector[Double] = {
    require(eigenPairs.size == alpha.size)
    x =>
      {
        val deformationsAtX = (0 until eigenPairs.size).map(i => {
          val (lambda_i, phi_i) = eigenPairs(i)
          phi_i(x) * alpha(i) * math.sqrt(lambda_i)
        })
        deformationsAtX.foldLeft(mean(x))(_ + _)
      }
  }

  def sample: CV[Double] => DenseVector[Double] = {
    val coeffs = for (_ <- 0 until eigenPairs.size) yield Gaussian(0, 1).draw()
    instance(DenseVector(coeffs.toArray))
  }

  def jacobian(p: DenseVector[Double]) = { x: CV[Double] =>
    val dim = x.dimensionality
    val J = DenseMatrix.zeros[Double](dim, eigenPairs.size)
    (0 until eigenPairs.size).map(i => {
      val (lambda_i, phi_i) = eigenPairs(i)
      J(::, i) := phi_i(x) * math.sqrt(lambda_i)
    })
    J
  }

  /**
   * returns a new gaussian process, where the values for the points are
   * pre-computed and hence fast to obtain
   */
  def specializeForPoints(points: IndexedSeq[CV[Double]]) = {
    new SpecializedLowRankGaussianProcess(this, points)

  }
}

class SpecializedLowRankGaussianProcess[CV[A] <: CoordVector[A]](gp: LowRankGaussianProcess[CV], points: IndexedSeq[CV[Double]])
  extends LowRankGaussianProcess[CV] {

  private val (gpLambdas, gpPhis) = gp.eigenPairs.unzip
  private val pointToIdxMap = points.zipWithIndex.toMap
  private val (meanVec, lambdas, eigenMatrix) = precomputeGPAtPoints

  private val stddev = DenseVector(lambdas.map(math.sqrt).toArray)
  private val Q = eigenMatrix * breeze.linalg.diag(stddev)
  private val phis = (0 until lambdas.size).map(i => phiAtPoint(i)_)

  override val domain = gp.domain
  override val outputDim = gp.outputDim

  override val eigenPairs = lambdas.zip(phis)
  override val mean = meanAtPoint _

  def instanceAtPoints(alpha: DenseVector[Double]): IndexedSeq[(CV[Double], DenseVector[Double])] = {
    require(eigenPairs.size == alpha.size)
    val instVal = Q * alpha + meanVec
    val ptVals = for (v <- instVal.toArray.grouped(outputDim)) yield DenseVector(v)
    points.zip(ptVals.toIndexedSeq)
  }

  def sampleAtPoints: IndexedSeq[(CV[Double], DenseVector[Double])] = {
    val coeffs = for (_ <- 0 until eigenPairs.size) yield Gaussian(0, 1).draw()
    instanceAtPoints(DenseVector(coeffs.toArray))
  }

  private def meanAtPoint(pt: CV[Double]) = {
    pointToIdxMap.get(pt) match {
      case Some(ptId) => meanVec(ptId * outputDim until (ptId + 1) * outputDim)
      case None => gp.mean(pt)
    }
  }

  private def phiAtPoint(i: Int)(pt: CV[Double]) = {
    pointToIdxMap.get(pt) match {
      case Some(ptId) => eigenMatrix(ptId * gp.outputDim until (ptId + 1) * gp.outputDim, i)
      case None => gpPhis(i)(pt)
    }
  }

  // precompute mean and basis vectors for the given points
  private def precomputeGPAtPoints = {

    val m = DenseVector.zeros[Double](points.size * gp.outputDim)
    for ((x, i) <- points.zipWithIndex.par) {
      m(i * gp.outputDim until (i + 1) * gp.outputDim) := gp.mean(x)
    }

    val U = DenseMatrix.zeros[Double](points.size * gp.outputDim, gp.rank)
    for ((x, i) <- points.zipWithIndex.par; (phi_j, j) <- gpPhis.zipWithIndex) {
      val v = phi_j(x)
      U(i * gp.outputDim until (i + 1) * gp.outputDim, j) := phi_j(x)
    }

    (m, gpLambdas, U)
  }

}

class LowRankGaussianProcess1D(
  val domain: BoxedRegion[CoordVector1D],
  val outputDim: Int,
  val mean: CoordVector1D[Double] => DenseVector[Double],
  val eigenPairs: IndexedSeq[(Double, CoordVector1D[Double] => DenseVector[Double])])
  extends LowRankGaussianProcess[CoordVector1D] {}

class LowRankGaussianProcess2D(
  val domain: BoxedRegion[CoordVector2D],
  val outputDim: Int,
  val mean: CoordVector2D[Double] => DenseVector[Double],
  val eigenPairs: IndexedSeq[(Double, CoordVector2D[Double] => DenseVector[Double])])
  extends LowRankGaussianProcess[CoordVector2D] {}

class LowRankGaussianProcess3D(
  val domain: BoxedRegion[CoordVector3D],
  val outputDim: Int,
  val mean: CoordVector3D[Double] => DenseVector[Double],
  val eigenPairs: IndexedSeq[(Double, CoordVector3D[Double] => DenseVector[Double])])
  extends LowRankGaussianProcess[CoordVector3D] {}

object GaussianProcess {

  def createLowRankGaussianProcess1D(configuration: LowRankGaussianProcessConfiguration[CoordVector1D]) = {
    def uniformSampler = UniformSampler1D()
    val eigenPairs = Kernel.computeNystromApproximation(configuration.cov, configuration.domain, configuration.numBasisFunctions, configuration.numPointsForNystrom, uniformSampler)
    new LowRankGaussianProcess1D(configuration.domain, configuration.cov.outputDim, configuration.mean, eigenPairs)
  }

  def createLowRankGaussianProcess2D(configuration: LowRankGaussianProcessConfiguration[CoordVector2D]) = {
    def uniformSampler = UniformSampler2D()
    val eigenPairs = Kernel.computeNystromApproximation(configuration.cov, configuration.domain, configuration.numBasisFunctions, configuration.numPointsForNystrom, uniformSampler)
    new LowRankGaussianProcess2D(configuration.domain, configuration.cov.outputDim, configuration.mean, eigenPairs)
  }

  def createLowRankGaussianProcess3D(configuration: LowRankGaussianProcessConfiguration[CoordVector3D]) = {
    def uniformSampler = UniformSampler3D()
    val eigenPairs = Kernel.computeNystromApproximation(configuration.cov, configuration.domain, configuration.numBasisFunctions, configuration.numPointsForNystrom, uniformSampler)
    new LowRankGaussianProcess3D(configuration.domain, configuration.cov.outputDim, configuration.mean, eigenPairs)
  }

  // Gaussian process regression for a low rank gaussian process
  def regression[CV[A] <: CoordVector[A]](gp: LowRankGaussianProcess[CV], trainingData: IndexedSeq[(CV[Double], DenseVector[Double])], sigma2: Double, meanOnly: Boolean = false) = {

    def flatten(v: IndexedSeq[DenseVector[Double]]) = DenseVector(v.flatten(_.toArray).toArray)

    val (xs, ys) = trainingData.unzip

    val yVec = flatten(ys)
    val meanValues = xs.map(gp.mean)
    val mVec = flatten(meanValues)

    val d = gp.outputDim
    val (lambdas, phis) = gp.eigenPairs.unzip

    // TODO that the dimensionality is okay
    val Q = DenseMatrix.zeros[Double](trainingData.size * d, phis.size)
    for ((x_i, i) <- xs.zipWithIndex; (phi_j, j) <- phis.zipWithIndex) {
      Q(i * d until i * d + d, j) := phi_j(x_i) * math.sqrt(lambdas(j))
    }

    val M = Q.t * Q + DenseMatrix.eye[Double](phis.size) * sigma2
    val Minv = breeze.linalg.pinv(M)
    val mean_coeffs = Minv * Q.t * (yVec - mVec)

    val mean_p = gp.instance(mean_coeffs)

    if (meanOnly == true) {
      val emptyEigenPairs = IndexedSeq[(Double, CV[Double] => DenseVector[Double])]()
      new LowRankGaussianProcess[CV] {
        val domain = gp.domain
        val outputDim = gp.outputDim
        val mean = mean_p
        val eigenPairs = emptyEigenPairs
      }
    } else {
      val D = breeze.linalg.diag(DenseVector(lambdas.map(math.sqrt(_)).toArray))
      val Sigma = D * Minv * D * sigma2
      val (innerU, innerD2, _) = breeze.linalg.svd(Sigma)

      @volatile
      var phisAtXCache = ImmutableLRU[CV[Double], DenseMatrix[Double]](1000)

      def phip(i: Int)(x: CV[Double]) = { // should be phi_p but _ is treated as partial function
        val (maybePhisAtX, newPhisAtXCache) = phisAtXCache.get(x)
        val phisAtX = maybePhisAtX.getOrElse {
          val newPhisAtX = {
            val innerPhisAtx = DenseMatrix.zeros[Double](d, phis.size)
            for ((phi_j, j) <- phis.zipWithIndex) {
              innerPhisAtx(0 until d, j) := phi_j(x)
            }
            innerPhisAtx
          }
          phisAtXCache = (phisAtXCache + (x, newPhisAtX))._2 // ignore evicted key
          newPhisAtX
        }
        phisAtX * innerU(::, i)
      }

      val phis_p = for (i <- 0 until phis.size) yield (x => phip(i)(x))
      val lambdas_p = innerD2.toArray.toIndexedSeq
      new LowRankGaussianProcess[CV] {
        val domain = gp.domain
        val outputDim = gp.outputDim
        val mean = mean_p
        val eigenPairs = lambdas_p.zip(phis_p)
      }
    }
  }

  // Gaussian process regression for a standard gaussian process
  def regression[CV[A] <: CoordVector[A]](gp: GaussianProcess[CV], trainingData: IndexedSeq[(CV[Double], DenseVector[Double])], sigma2: Double) = {

    def flatten(v: IndexedSeq[DenseVector[Double]]) = DenseVector(v.flatten(_.toArray).toArray)
    val d = gp.cov.outputDim
    val (xs, ys) = trainingData.unzip
    val yVec = flatten(ys)
    val meanValues = xs.map(gp.mean(_))
    val mVec = flatten(meanValues)
    val kxx = Kernel.computeKernelMatrix(xs, gp.cov)

    val kinv = breeze.linalg.inv(kxx + DenseMatrix.eye[Double](kxx.cols) * sigma2)

    def mp(x: CV[Double]): DenseVector[Double] = {
      val kxs = Kernel.computeKernelVectorFor(x, xs, gp.cov)
      gp.mean(x) + (kxs * (kinv * (yVec - mVec)))

    }

    val kp = new PDKernel[CV] {
      def apply(x1: CV[Double], x2: CV[Double]): DenseMatrix[Double] = {
        val kx1xs = Kernel.computeKernelVectorFor(x1, xs, gp.cov)
        val kx2xs = Kernel.computeKernelVectorFor(x2, xs, gp.cov)
        gp.cov(x1, x2) - (kx1xs * (kinv * kx2xs.t))
      }
      def outputDim = d
    }

    GaussianProcess[CV](gp.domain, mp, kp)
  }

  def main(args: Array[String]) {

    val cov = UncorrelatedKernelND(GaussianKernel3D(100) * 100., 3)
    val mesh = MeshIO.readHDF5(new File("/tmp/mesh.h5")).get
    val meshPoints = mesh.domain.points
    val region = mesh.boundingBox
    println("region: " + region)
    val gpConfiguration = LowRankGaussianProcessConfiguration[CoordVector3D](
      region,
      (x: CoordVector3D[Double]) => DenseVector(0., 0., 0.),
      cov,
      20,
      300)
    val gp = GaussianProcess.createLowRankGaussianProcess3D(gpConfiguration)

    val specializedGP = gp.specializeForPoints(meshPoints.toIndexedSeq)
    for (i <- 0 until 10) {

      val s = System.currentTimeMillis()

      val ptSamples = specializedGP.sampleAtPoints
      val newPoints = for ((pt, samplePt) <- ptSamples) yield {
        CoordVector3D(pt(0) + samplePt(0), pt(1) + samplePt(1), pt(2) + samplePt(2))
      }
      val newMesh = TriangleMesh(TriangleMeshDomain(newPoints.toIndexedSeq, mesh.domain.cells))
      println("time in ms " +(System.currentTimeMillis() - s))
      //val vtkpd = Utils.meshToVTKMesh(newMesh)
      //Utils.showVTK(vtkpd)
    }

  }
}