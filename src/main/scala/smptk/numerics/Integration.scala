package smptk.numerics

import scala.language.higherKinds
import smptk.image._
import breeze.linalg.DenseVector
import smptk.image.Image._
import smptk.image.DiscreteImageDomain
import smptk.common.BoxedRegion
import smptk.common.BoxedRegion1D
import smptk.common.BoxedRegion2D
import smptk.image.Geometry.CoordVector1D
import smptk.image.Geometry.CoordVector2D
import smptk.image.Geometry.CoordVector3D

trait Sampler[CV[A] <: CoordVector[A]] {  
  def sample(boxedRegion: BoxedRegion[CV], numberOfPoints: Int): IndexedSeq[CV[Double]]
}

trait UniformSampler[CV[A] <: CoordVector[A]] extends Sampler[CV]
trait RandomSampler[CV[A] <: CoordVector[A]] extends Sampler[CV]

case class UniformSampler1D extends UniformSampler[CoordVector1D] {
  def sample(boxedRegion: BoxedRegion[CoordVector1D], numberOfPoints: Int = 300) = {
    val step = (boxedRegion.extent(0) - boxedRegion.origin(0)) / numberOfPoints.toDouble
    for (i <- 0 until numberOfPoints) yield CoordVector1D[Double](boxedRegion.origin(0) + i * step)
  }

}

case class UniformSampler2D extends UniformSampler[CoordVector2D] {
  def sample(region: BoxedRegion[CoordVector2D], numberOfPoints: Int = 300) = {
    val nbPerDim = math.sqrt(numberOfPoints).floor.toInt
    val step0 = (region.extent(0) - region.origin(0)) / nbPerDim
    val step1 = (region.extent(1) - region.origin(1)) / nbPerDim
    for (i <- 0 until nbPerDim; j <- 0 until nbPerDim) yield CoordVector2D[Double](region.origin(0) + i * step0, region.origin(1) + j * step1)
  }
}

case class UniformSampler3D extends UniformSampler[CoordVector3D] {
  def sample(region: BoxedRegion[CoordVector3D], numberOfPoints: Int = 300) = {
    val nbPerDim = math.cbrt(numberOfPoints).floor.toInt
    val step0 = (region.extent(0) - region.origin(0)) / nbPerDim
    val step1 = (region.extent(1) - region.origin(1)) / nbPerDim
    val step2 = (region.extent(2) - region.origin(2)) / nbPerDim
    
    for (i <- 0 until nbPerDim; j <- 0 until nbPerDim; k <- 0 until nbPerDim) 
      yield CoordVector3D[Double](region.origin(0) + i * step0, region.origin(1) + j * step1, region.origin(2) + k * step2)
  }
}



case class UniformDistributionRandomSampler1D extends RandomSampler[CoordVector1D] {
  def sample(region: BoxedRegion[CoordVector1D], numberOfPoints: Int = 300) = {
    val distr = breeze.stats.distributions.Uniform(region.origin(0), region.extent(0))
    (0 until numberOfPoints).map(i => CoordVector1D(distr.draw))
  }
}

case class UniformDistributionRandomSampler2D extends RandomSampler[CoordVector2D] {
  def sample(region: BoxedRegion[CoordVector2D], numberOfPoints: Int = 300) = {
    val distrDim1 = breeze.stats.distributions.Uniform(region.origin(0), region.extent(0))
    val distrDim2 = breeze.stats.distributions.Uniform(region.origin(1), region.extent(1))

    (0 until numberOfPoints).map(i => CoordVector2D(distrDim1.draw, distrDim2.draw))
  }
}

case class UniformDistributionRandomSampler3D extends RandomSampler[CoordVector3D] {
  def sample(region: BoxedRegion[CoordVector3D], numberOfPoints: Int = 300) = {
    val distrDim1 = breeze.stats.distributions.Uniform(region.origin(0), region.extent(0))
    val distrDim2 = breeze.stats.distributions.Uniform(region.origin(1), region.extent(1))
    val distrDim3 = breeze.stats.distributions.Uniform(region.origin(2), region.extent(2))
    
    (0 until numberOfPoints).map(i => CoordVector3D(distrDim1.draw, distrDim2.draw, distrDim3.draw))
  }
}


case class SampleOnceSampler[CV[A] <: CoordVector[A]](sampler: Sampler[CV]) extends Sampler[CV] {
 
  var points : IndexedSeq[CV[Double]] = IndexedSeq()
  def sample(boxedRegion: BoxedRegion[CV], numberOfPoints: Int): IndexedSeq[CV[Double]] = { 
    if(points.size != numberOfPoints) points 
    else {
      points = sampler.sample(boxedRegion, numberOfPoints)
      points
    }
  }
    
  def sampledPoints = points
}

case class IntegratorConfiguration[CV[A] <: CoordVector[A]](sampler: Sampler[CV], numberOfPoints : Int)

case class Integrator[CV[A] <: CoordVector[A]](configuration: IntegratorConfiguration[CV]) {

  def sampler = configuration.sampler

  def integrateScalar(img: ContinuousScalarImage[CV], integrationRegion: BoxedRegion[CV]): Double = {
    integrateScalar(img.liftPixelValue, integrationRegion)
  }

  def integrateScalar(f: Function1[CV[Double], Option[Double]], integrationRegion: BoxedRegion[CV]): Double = {
    val sampleValues = configuration.sampler.sample(integrationRegion, configuration.numberOfPoints).par.map(f)

    val sum = sampleValues.map(_.getOrElse(0.)).sum
    val ndVolume = integrationRegion.volume

    sum * ndVolume / (configuration.numberOfPoints - 1).toDouble
  }

  def integrateVector(img: ContinuousVectorImage[CV], integrationRegion: BoxedRegion[CV]): DenseVector[Double] = {

    val sampleValues = configuration.sampler.sample(integrationRegion, configuration.numberOfPoints).par.map(img.liftPixelValue)
    val ndVolume = integrationRegion.volume;

    val zeroVector = DenseVector.zeros[Double](img.pixelDimensionality)
    val sum: DenseVector[Double] = sampleValues.map(_.getOrElse(zeroVector)).foldLeft(zeroVector)((a, b) => { a + b })
    sum * ndVolume / (configuration.numberOfPoints - 1).toDouble

  }
}


