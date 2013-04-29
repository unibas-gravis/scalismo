package smptk
package registration

import image.CoordVector
import image.Geometry.{CoordVector1D, CoordVector2D, CoordVector3D}
import image.DiscreteImageDomain
import breeze.linalg.DenseVector
import breeze.linalg.DenseMatrix

case class GaussianProcess[CV[A] <: CoordVector[A]](val domain: DiscreteImageDomain[CV], val mean: CV[Double] => DenseVector[Double], val cov: PDKernel[CV]) {

  type PointSample = IndexedSeq[CV[Double]]

  def sample: (PointSample => DenseVector[Double]) = { (xs: PointSample) =>
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
      meanVec + lMat * uVec
    }
  }
}



object GaussianProcess { 
  
        
  def regression[CV[A] <: CoordVector[A]](gp : GaussianProcess[CV], trainingData: IndexedSeq[(CV[Double], DenseVector[Double])], sigma2: Double)  = {

    def flatten(v : IndexedSeq[DenseVector[Double]]) = DenseVector(v.flatten(_.toArray).toArray)
    val d = gp.cov.outputDim
    val (xs, ys) = trainingData.unzip  
    val yVec= flatten(ys)
    val meanValues = xs.map(gp.mean(_))
    val mVec = flatten(meanValues)
    val kxx = Kernel.computeKernelMatrix(xs, gp.cov)

    val kinv = breeze.linalg.inv(kxx + DenseMatrix.eye[Double](kxx.cols) * sigma2)

    def mp(x: CV[Double]): DenseVector[Double] = {
      val kxs = Kernel.computeKernelVectorFor(x, xs, gp.cov)      
      gp.mean(x) + (kxs * (kinv * (yVec - mVec)))

    }

    val kp = new PDKernel[CV] {
      def apply(x1: CV[Double], x2: CV[Double]) : DenseMatrix[Double] = {
        val kx1xs = Kernel.computeKernelVectorFor(x1, xs, gp.cov)
        val kx2xs = Kernel.computeKernelVectorFor(x2, xs, gp.cov)
        gp.cov(x1, x2) - (kx1xs * (kinv * kx2xs.t))
      }
      def outputDim = d
    }
   GaussianProcess(gp.domain, mp, kp)
  }
}