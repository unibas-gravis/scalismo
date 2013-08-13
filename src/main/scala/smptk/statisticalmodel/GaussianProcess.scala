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

case class GaussianProcess[D <: Dim: DimTraits](val domain: BoxedDomain[D], val mean: Point[D] => Vector[D], val cov: MatrixValuedPDKernel[D, D]) {
  val dimTraits = implicitly[DimTraits[D]]
  type PointSample = IndexedSeq[Point[D]]

  def sample: (PointSample => IndexedSeq[Vector[D]]) = { (xs: PointSample) =>
    {
      val n = xs.size
      val d = cov.outputDim
      val meanVec = DenseVector.zeros[Double](n * d)
      for (i <- 0 until n; di <- 0 until d) meanVec(i * d + di) = mean(xs(i))(di)
      val covMatrix = Kernel.computeKernelMatrix(xs, cov)
      val noise = breeze.linalg.diag(DenseVector.ones[Float](xs.size)) * 1e-6f // gaussian noise for stability 
      val lMat = breeze.linalg.cholesky((covMatrix + noise).map(_.toDouble))
      val u = for (_ <- 0 until xs.size) yield breeze.stats.distributions.Gaussian(0, 1).draw()
      val uVec = DenseVector(u.toArray)
      val sampleVec = (meanVec + lMat * uVec).map(_.toFloat) // the sample as a long vector

      // now we group it 
      // TODO make me more efficient and elegant
      val sampleSeq = sampleVec.toArray.toIndexedSeq
      val pointSampleValues = sampleSeq.grouped(d).map(ptVec => dimTraits.createVector(ptVec.toArray))
      pointSampleValues.toIndexedSeq
    }
  }
}

case class LowRankGaussianProcessConfiguration[D <: Dim](
  val domain: BoxedDomain[D],
  val sampler: Sampler[D, Point[D]],
  val mean: Point[D] => Vector[D],
  val cov: MatrixValuedPDKernel[D, D],
  val numBasisFunctions: Int,
  val numPointsForNystrom: Int)

abstract class LowRankGaussianProcess[D <: Dim: DimTraits] {

  val domain: BoxedDomain[D]
  val outputDim: Int
  val mean: Point[D] => Vector[D]
  val eigenPairs: IndexedSeq[(Float, Point[D] => Vector[D])]

  def rank = eigenPairs.size

  def instance(alpha: DenseVector[Float]): Point[D] => Vector[D] = {
    require(eigenPairs.size == alpha.size)
    x =>
      {
        val deformationsAtX = (0 until eigenPairs.size).map(i => {
          val (lambda_i, phi_i) = eigenPairs(i)
          phi_i(x) * alpha(i) * math.sqrt(lambda_i).toFloat
        })
        deformationsAtX.foldLeft(mean(x))(_ + _)
      }
  }

  def sample: Point[D] => Vector[D] = {
    val coeffs = for (_ <- 0 until eigenPairs.size) yield Gaussian(0, 1).draw().toFloat
    instance(DenseVector(coeffs.toArray))
  }

  def jacobian(p: DenseVector[Float]) = { x: Point[D] =>
    val dim = x.dimensionality
    val J = DenseMatrix.zeros[Float](dim, eigenPairs.size)
    (0 until eigenPairs.size).map(i => {
      val (lambda_i, phi_i) = eigenPairs(i)
      J(::, i) := (phi_i(x) * math.sqrt(lambda_i).toFloat).toBreezeVector
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

class SpecializedLowRankGaussianProcess[D <: Dim: DimTraits](gp: LowRankGaussianProcess[D], points: IndexedSeq[Point[D]])
  extends LowRankGaussianProcess[D] {

  val dimTraits = implicitly[DimTraits[D]]

  private val (gpLambdas, gpPhis) = gp.eigenPairs.unzip
  private val pointToIdxMap = points.zipWithIndex.toMap
  private val (meanVec, lambdas, eigenMatrix) = precomputeGPAtPoints

  private val stddev = DenseVector(lambdas.map(x => math.sqrt(x).toFloat).toArray)
  private val phis = (0 until lambdas.size).map(i => phiAtPoint(i)_)

  override val domain = gp.domain
  override val outputDim = gp.outputDim

  override val eigenPairs = lambdas.zip(phis)
  override val mean = meanAtPoint _

  def instanceAtPoints(alpha: DenseVector[Float]): IndexedSeq[(Point[D], Vector[D])] = {
    require(eigenPairs.size == alpha.size)
    // (this corresponds to eigenMatrix * diag(stddef) * alpha + meanVec, but is more efficient
    val instVal = eigenMatrix * (stddev :* alpha) + meanVec
    val ptVals = for (v <- instVal.toArray.grouped(outputDim)) yield dimTraits.createVector(v)
    points.zip(ptVals.toIndexedSeq)
  }

  def sampleAtPoints: IndexedSeq[(Point[D], Vector[D])] = {
    val coeffs = for (_ <- 0 until eigenPairs.size) yield Gaussian(0, 1).draw().toFloat
    instanceAtPoints(DenseVector(coeffs.toArray))
  }

  private def meanAtPoint(pt: Point[D]): Vector[D] = {
    pointToIdxMap.get(pt) match {
      case Some(ptId) => {
        // we need the copy here, as otherwise vec.data will be the array of the  
        // original vector (from which we extracted a slice)
        val vec = meanVec(ptId * outputDim until (ptId + 1) * outputDim).copy
        dimTraits.createVector(vec.data)
      }
      case None => gp.mean(pt)
    }
  }

  private def phiAtPoint(i: Int)(pt: Point[D]): Vector[D] = {
    val dimTraits = implicitly[DimTraits[D]]
    pointToIdxMap.get(pt) match {
      case Some(ptId) => {
        // we need the copy here, as otherwise vec.data will be the array of the  
        // original vector (from which we extracted a slice)
        val df = eigenMatrix(ptId * gp.outputDim until (ptId + 1) * gp.outputDim, i).copy
        dimTraits.createVector(df.data)
      }
      case None => gpPhis(i)(pt)
    }
  }

  // precompute mean and basis vectors for the given points
  private def precomputeGPAtPoints = {

    val m = DenseVector.zeros[Float](points.size * gp.outputDim)
    for ((x, i) <- points.zipWithIndex.par) {
      m(i * gp.outputDim until (i + 1) * gp.outputDim) := gp.mean(x).toBreezeVector
    }

    val U = DenseMatrix.zeros[Float](points.size * gp.outputDim, gp.rank)
    for ((x, i) <- points.zipWithIndex.par; (phi_j, j) <- gpPhis.zipWithIndex) {
      val v = phi_j(x)
      U(i * gp.outputDim until (i + 1) * gp.outputDim, j) := phi_j(x).toBreezeVector
    }

    (m, gpLambdas, U)
  }

}

class LowRankGaussianProcess1D(
  val domain: BoxedDomain[OneD],
  val outputDim: Int,
  val mean: Point[OneD] => Vector[OneD],
  val eigenPairs: IndexedSeq[(Float, Point[OneD] => Vector[OneD])])
  extends LowRankGaussianProcess[OneD] {}

class LowRankGaussianProcess2D(
  val domain: BoxedDomain[TwoD],
  val outputDim: Int,
  val mean: Point[TwoD] => Vector[TwoD],
  val eigenPairs: IndexedSeq[(Float, Point[TwoD] => Vector[TwoD])])
  extends LowRankGaussianProcess[TwoD] {}

class LowRankGaussianProcess3D(
  val domain: BoxedDomain[ThreeD],
  val outputDim: Int,
  val mean: Point[ThreeD] => Vector[ThreeD],
  val eigenPairs: IndexedSeq[(Float, Point[ThreeD] => Vector[ThreeD])])
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
  def regression[D <: Dim: DimTraits](gp: LowRankGaussianProcess[D], trainingData: IndexedSeq[(Point[D], Vector[D])], sigma2: Double, meanOnly: Boolean = false) = {

    val dimTraits = implicitly[DimTraits[D]]
    def flatten(v: IndexedSeq[Vector[D]]) = DenseVector(v.flatten(_.data).toArray)

    val (xs, ys) = trainingData.unzip

    val yVec = flatten(ys)
    val meanValues = xs.map(gp.mean)
    val mVec = flatten(meanValues)

    val d = gp.outputDim
    val (lambdas, phis) = gp.eigenPairs.unzip

    // TODO that the dimensionality is okay
    val Q = DenseMatrix.zeros[Double](trainingData.size * d, phis.size)
    for ((x_i, i) <- xs.zipWithIndex; (phi_j, j) <- phis.zipWithIndex) {
      Q(i * d until i * d + d, j) := phi_j(x_i).toBreezeVector.map(_.toDouble) * math.sqrt(lambdas(j))
    }

    val M = Q.t * Q + DenseMatrix.eye[Double](phis.size) * sigma2
    val Minv = breeze.linalg.pinv(M)
    val mean_coeffs = (Minv * Q.t).map(_.toFloat) * (yVec - mVec)

    val mean_p = gp.instance(mean_coeffs)

    if (meanOnly == true) {
      val emptyEigenPairs = IndexedSeq[(Float, Point[D] => Vector[D])]()
      new LowRankGaussianProcess[D] {
        val domain = gp.domain
        val outputDim = gp.outputDim
        val mean = mean_p
        val eigenPairs = emptyEigenPairs
      }
    } else {
      val D = breeze.linalg.diag(DenseVector(lambdas.map(math.sqrt(_)).toArray))
      val Sigma = D * Minv * D * sigma2
      val (innerUDbl, innerD2, _) = breeze.linalg.svd(Sigma)
      val innerU = innerUDbl.map(_.toFloat)
      @volatile
      var phisAtXCache = ImmutableLRU[Point[D], DenseMatrix[Float]](1000)

      def phip(i: Int)(x: Point[D]): Vector[D] = { // should be phi_p but _ is treated as partial function
        val (maybePhisAtX, newPhisAtXCache) = phisAtXCache.get(x)
        val phisAtX = maybePhisAtX.getOrElse {
          val newPhisAtX = {
            val innerPhisAtx = DenseMatrix.zeros[Float](d, gp.rank)
            var j = 0;
            while (j < phis.size) {
              val phi_j = phis(j)
              innerPhisAtx(0 until d, j) := phi_j(x).toBreezeVector
              j += 1
            }
            innerPhisAtx
          }
          phisAtXCache = (phisAtXCache + (x, newPhisAtX))._2 // ignore evicted key
          newPhisAtX
        }
        val vec = phisAtX * innerU(::, i)
        dimTraits.createVector(vec.data)
      }

      val phis_p = for (i <- 0 until phis.size) yield (x => phip(i)(x))
      val lambdas_p = innerD2.toArray.map(_.toFloat).toIndexedSeq
      new LowRankGaussianProcess[D] {
        val domain = gp.domain
        val outputDim = gp.outputDim
        val mean = mean_p
        val eigenPairs = lambdas_p.zip(phis_p)
      }
    }
  }

  // Gaussian process regression for a standard gaussian process
  def regression[D <: Dim: DimTraits](gp: GaussianProcess[D], trainingData: IndexedSeq[(Point[D], Vector[D])], sigma2: Double) = {

    val dimTraits = implicitly[DimTraits[D]]
    def flatten(v: IndexedSeq[Vector[D]]) = DenseVector(v.flatten(_.data).toArray)
    val d = gp.cov.outputDim
    val (xs, ys) = trainingData.unzip
    val yVec = flatten(ys)
    val meanValues = xs.map(gp.mean(_))
    val mVec = flatten(meanValues)
    val kxx = Kernel.computeKernelMatrix(xs, gp.cov)

    val kinv = (breeze.linalg.inv(kxx + DenseMatrix.eye[Float](kxx.cols) * sigma2.toFloat)).map(_.toFloat)

    def mp(x: Point[D]): Vector[D] = {
      val kxs = Kernel.computeKernelVectorFor(x, xs, gp.cov)
      val meandf = kxs * (kinv * (yVec - mVec))
      gp.mean(x) + dimTraits.createVector(meandf.data)

    }

    val kp: MatrixValuedPDKernel[D, D] = new MatrixValuedPDKernel[D, D] {
      def apply(x1: Point[D], x2: Point[D]): MatrixNxN[D] = {
        val kx1xs = Kernel.computeKernelVectorFor(x1, xs, gp.cov)
        val kx2xs = Kernel.computeKernelVectorFor(x2, xs, gp.cov)
        val cov = (kx1xs * (kinv * kx2xs.t))
        gp.cov(x1, x2) - dimTraits.createMatrixNxN(cov.data)
      }
    }

    GaussianProcess[D](gp.domain, mp, kp)
  }

  def main(args: Array[String]) {

    val cov = UncorrelatedKernel3x3(GaussianKernel3D(100)) * 100
    val mesh = MeshIO.readHDF5(new File("/tmp/mesh.h5")).get
    val meshPoints = mesh.points
    val region = mesh.boundingBox

    val gpConfiguration = LowRankGaussianProcessConfiguration[ThreeD](
      region,
      UniformSampler3D(region),
      (x: Point[ThreeD]) => Vector3D(0f, 0f, 0f),
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
      println("time in ms " + (System.currentTimeMillis() - s))
      //val vtkpd = Utils.meshToVTKMesh(newMesh)
      //Utils.showVTK(vtkpd)
    }

  }
}