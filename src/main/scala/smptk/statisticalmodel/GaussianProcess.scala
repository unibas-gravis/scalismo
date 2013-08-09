package smptk
package statisticalmodel

import common.BoxedDomain
import breeze.linalg.{ DenseVector, DenseMatrix, linspace }
import breeze.stats.distributions.Gaussian
import smptk.common.DiscreteDomain
import smptk.common.{ BoxedDomain1D, BoxedDomain2D, BoxedDomain3D }
import smptk.numerics.{ UniformSampler1D, UniformSampler2D, UniformSampler3D }
import smptk.io.MeshIO
import smptk.mesh.TriangleMesh
import smptk.image.Utils
import java.io.File
import smptk.numerics.UniformSampler1D
import smptk.kernels._
import smptk.numerics.UniformSampler1D
import breeze.plot.{ plot, Figure }
import smptk.image.DiscreteImageDomain
import smptk.common.ImmutableLRU
import scala.collection.immutable.HashMap
import smptk.geometry._
import smptk.numerics.Sampler
import smptk.numerics.UniformSampler3D


case class GaussianProcess[D <: Dim](val domain: BoxedDomain[D], val mean: Point[D] => DenseVector[Double], val cov: MatrixValuedPDKernel[D]) {

  type PointSample = IndexedSeq[Point[D]]

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

case class LowRankGaussianProcessConfiguration[D <: Dim](
  val domain: BoxedDomain[D],
  val sampler : Sampler[D, Point[D]],  
  val mean: Point[D] => DenseVector[Double],
  val cov: MatrixValuedPDKernel[D],
  val numBasisFunctions: Int,
  val numPointsForNystrom: Int  )

trait LowRankGaussianProcess[D <: Dim] {

  val domain: BoxedDomain[D]
  val outputDim: Int
  val mean: Point[D] => DenseVector[Double]
  val eigenPairs: IndexedSeq[(Double, Point[D] => DenseVector[Double])]

  def rank = eigenPairs.size

  def instance(alpha: DenseVector[Double]): Point[D] => DenseVector[Double] = {
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

  def sample: Point[D] => DenseVector[Double] = {
    val coeffs = for (_ <- 0 until eigenPairs.size) yield Gaussian(0, 1).draw()
    instance(DenseVector(coeffs.toArray))
  }

  def jacobian(p: DenseVector[Double]) = { x: Point[D] =>
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
  def specializeForPoints(points: IndexedSeq[Point[D]]) = {
    new SpecializedLowRankGaussianProcess(this, points)

  }
}

class SpecializedLowRankGaussianProcess[D <: Dim](gp: LowRankGaussianProcess[D], points: IndexedSeq[Point[D]])
  extends LowRankGaussianProcess[D] {

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

  def instanceAtPoints(alpha: DenseVector[Double]): IndexedSeq[(Point[D], DenseVector[Double])] = {
    require(eigenPairs.size == alpha.size)
    val instVal = Q * alpha + meanVec
    val ptVals = for (v <- instVal.toArray.grouped(outputDim)) yield DenseVector(v)
    points.zip(ptVals.toIndexedSeq)
  }

  def sampleAtPoints: IndexedSeq[(Point[D], DenseVector[Double])] = {
    val coeffs = for (_ <- 0 until eigenPairs.size) yield Gaussian(0, 1).draw()
    instanceAtPoints(DenseVector(coeffs.toArray))
  }

  private def meanAtPoint(pt: Point[D]) = {
    pointToIdxMap.get(pt) match {
      case Some(ptId) => meanVec(ptId * outputDim until (ptId + 1) * outputDim)
      case None => gp.mean(pt)
    }
  }

  private def phiAtPoint(i: Int)(pt: Point[D]) = {
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
  val domain: BoxedDomain[OneD],
  val outputDim: Int,
  val mean: Point[OneD] => DenseVector[Double],
  val eigenPairs: IndexedSeq[(Double, Point[OneD] => DenseVector[Double])])
  extends LowRankGaussianProcess[OneD] {}

class LowRankGaussianProcess2D(
  val domain: BoxedDomain[TwoD],
  val outputDim: Int,
  val mean: Point[TwoD] => DenseVector[Double],
  val eigenPairs: IndexedSeq[(Double, Point[TwoD] => DenseVector[Double])])  
  extends LowRankGaussianProcess[TwoD] {}

class LowRankGaussianProcess3D(
  val domain: BoxedDomain[ThreeD],
  val outputDim: Int,
  val mean: Point[ThreeD] => DenseVector[Double],
  val eigenPairs: IndexedSeq[(Double, Point[ThreeD] => DenseVector[Double])])
  extends LowRankGaussianProcess[ThreeD] {}

object GaussianProcess {

  def createLowRankGaussianProcess1D(configuration: LowRankGaussianProcessConfiguration[OneD]) = {
    val eigenPairs = Kernel.computeNystromApproximation(configuration.cov, configuration.numBasisFunctions, configuration.numPointsForNystrom, configuration.sampler)
    new LowRankGaussianProcess1D(configuration.domain, configuration.cov.outputDim, configuration.mean, eigenPairs)
  }

  def createLowRankGaussianProcess2D(configuration: LowRankGaussianProcessConfiguration[TwoD]) = {
    val eigenPairs = Kernel.computeNystromApproximation(configuration.cov, configuration.numBasisFunctions, configuration.numPointsForNystrom, configuration.sampler)
    new LowRankGaussianProcess2D(configuration.domain, configuration.cov.outputDim, configuration.mean, eigenPairs)
  }

  def createLowRankGaussianProcess3D(configuration: LowRankGaussianProcessConfiguration[ThreeD]) = {
    val eigenPairs = Kernel.computeNystromApproximation(configuration.cov, configuration.numBasisFunctions, configuration.numPointsForNystrom, configuration.sampler)
    new LowRankGaussianProcess3D(configuration.domain, configuration.cov.outputDim, configuration.mean, eigenPairs)
  }

  // Gaussian process regression for a low rank gaussian process
  def regression[D <: Dim](gp: LowRankGaussianProcess[D], trainingData: IndexedSeq[(Point[D], DenseVector[Double])], sigma2: Double, meanOnly: Boolean = false) = {

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
      val emptyEigenPairs = IndexedSeq[(Double, Point[D] => DenseVector[Double])]()
      new LowRankGaussianProcess[D] {
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
      var phisAtXCache = ImmutableLRU[Point[D], DenseMatrix[Double]](1000)

      def phip(i: Int)(x: Point[D]) = { // should be phi_p but _ is treated as partial function
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
      new LowRankGaussianProcess[D] {
        val domain = gp.domain
        val outputDim = gp.outputDim
        val mean = mean_p
        val eigenPairs = lambdas_p.zip(phis_p)
      }
    }
  }

  // Gaussian process regression for a standard gaussian process
  def regression[D <: Dim](gp: GaussianProcess[D], trainingData: IndexedSeq[(Point[D], DenseVector[Double])], sigma2: Double) = {

    def flatten(v: IndexedSeq[DenseVector[Double]]) = DenseVector(v.flatten(_.toArray).toArray)
    val d = gp.cov.outputDim
    val (xs, ys) = trainingData.unzip
    val yVec = flatten(ys)
    val meanValues = xs.map(gp.mean(_))
    val mVec = flatten(meanValues)
    val kxx = Kernel.computeKernelMatrix(xs, gp.cov)

    val kinv = breeze.linalg.inv(kxx + DenseMatrix.eye[Double](kxx.cols) * sigma2)

    def mp(x: Point[D]): DenseVector[Double] = {
      val kxs = Kernel.computeKernelVectorFor(x, xs, gp.cov)
      gp.mean(x) + (kxs * (kinv * (yVec - mVec)))

    }

    val kp = new MatrixValuedPDKernel[D] {
      def apply(x1: Point[D], x2: Point[D]): DenseMatrix[Double] = {
        val kx1xs = Kernel.computeKernelVectorFor(x1, xs, gp.cov)
        val kx2xs = Kernel.computeKernelVectorFor(x2, xs, gp.cov)
        gp.cov(x1, x2) - (kx1xs * (kinv * kx2xs.t))
      }
      def outputDim = d
    }

    GaussianProcess[D](gp.domain, mp, kp)
  }

  def main(args: Array[String]) {

    val cov = UncorrelatedKernelND(GaussianKernel3D(100), 3) * 100
    val mesh = MeshIO.readHDF5(new File("/tmp/mesh.h5")).get
    val meshPoints = mesh.points
    val region = mesh.boundingBox
    println("region: " + region)
    val gpConfiguration = LowRankGaussianProcessConfiguration[ThreeD](
      region,
      UniformSampler3D(region),
      (x: Point[ThreeD]) => DenseVector(0.0, 0.0, 0.0),
      cov,
      20,
      300)
    val gp = GaussianProcess.createLowRankGaussianProcess3D(gpConfiguration)

    val specializedGP = gp.specializeForPoints(meshPoints.toIndexedSeq)
    for (i <- 0 until 10) {

      val s = System.currentTimeMillis()

      val ptSamples = specializedGP.sampleAtPoints
      val newPoints = for ((pt, samplePt) <- ptSamples) yield {
        Point3D(pt(0) + samplePt(0), pt(1) + samplePt(1), pt(2) + samplePt(2))
      }
      val newMesh = TriangleMesh(newPoints.toIndexedSeq, mesh.cells)
      println("time in ms " +(System.currentTimeMillis() - s))
      //val vtkpd = Utils.meshToVTKMesh(newMesh)
      //Utils.showVTK(vtkpd)
    }

  }
}