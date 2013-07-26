package smptk.numerics


import smptk.image._
import breeze.linalg.DenseVector
import smptk.geometry._
import smptk.image.DiscreteImageDomain
import smptk.common.BoxedDomain
import smptk.common.BoxedDomain1D
import smptk.common.BoxedDomain2D

trait Sampler[D <: Dim] {  
  def sample(boxedRegion: BoxedDomain[D], numberOfPoints: Int): IndexedSeq[Point[D]]
}

trait UniformSampler[D <: Dim] extends Sampler[D]
trait RandomSampler[D <: Dim] extends Sampler[D]

case class UniformSampler1D extends UniformSampler[OneD] {
  def sample(boxedRegion: BoxedDomain[OneD], numberOfPoints: Int = 300) = {
    val step = (boxedRegion.extent(0) - boxedRegion.origin(0)) / numberOfPoints.toDouble
    for (i <- 0 until numberOfPoints) yield Point1D(boxedRegion.origin(0) + i * step)
  }

}

case class UniformSampler2D extends UniformSampler[TwoD] {
  def sample(region: BoxedDomain[TwoD], numberOfPoints: Int = 300) = {
    val nbPerDim = math.sqrt(numberOfPoints).floor.toInt
    val step0 = (region.extent(0) - region.origin(0)) / nbPerDim
    val step1 = (region.extent(1) - region.origin(1)) / nbPerDim
    
    for (i <- 0 until nbPerDim; j <- 0 until nbPerDim) yield Point2D(region.origin(0) + i * step0, region.origin(1) + j * step1)
  }
}

case class UniformSampler3D extends UniformSampler[ThreeD] {
  def sample(region: BoxedDomain[ThreeD], numberOfPoints: Int = 300) = {
    val nbPerDim = math.cbrt(numberOfPoints).floor.toInt
    val step0 = (region.extent(0) - region.origin(0)) / nbPerDim
    val step1 = (region.extent(1) - region.origin(1)) / nbPerDim
    val step2 = (region.extent(2) - region.origin(2)) / nbPerDim
    
    for (i <- 0 until nbPerDim; j <- 0 until nbPerDim; k <- 0 until nbPerDim) 
      yield Point3D(region.origin(0) + i * step0, region.origin(1) + j * step1, region.origin(2) + k * step2)
  }
}



case class UniformDistributionRandomSampler1D extends RandomSampler[OneD] {
  def sample(region: BoxedDomain[OneD], numberOfPoints: Int = 300) = {
    val distr = breeze.stats.distributions.Uniform(region.origin(0), region.extent(0))
    (0 until numberOfPoints).map(i => Point1D(distr.draw))
  }
}

case class UniformDistributionRandomSampler2D extends RandomSampler[TwoD] {
  def sample(region: BoxedDomain[TwoD], numberOfPoints: Int = 300) = {
    val distrDim1 = breeze.stats.distributions.Uniform(region.origin(0), region.extent(0))
    val distrDim2 = breeze.stats.distributions.Uniform(region.origin(1), region.extent(1))
 
    (0 until numberOfPoints).map(i => Point2D(distrDim1.draw, distrDim2.draw))
  }
}

case class UniformDistributionRandomSampler3D extends RandomSampler[ThreeD] {
  def sample(region: BoxedDomain[ThreeD], numberOfPoints: Int = 300) = {
    val distrDim1 = breeze.stats.distributions.Uniform(region.origin(0), region.extent(0))
    val distrDim2 = breeze.stats.distributions.Uniform(region.origin(1), region.extent(1))
    val distrDim3 = breeze.stats.distributions.Uniform(region.origin(2), region.extent(2))
    
    (0 until numberOfPoints).map(i => Point3D(distrDim1.draw, distrDim2.draw, distrDim3.draw))
  }
}


case class SampleOnceSampler[D <: Dim](sampler: Sampler[D]) extends Sampler[D] {
 
  var points : IndexedSeq[Point[D]] = IndexedSeq()
  def sample(boxedRegion: BoxedDomain[D], numberOfPoints: Int): IndexedSeq[Point[D]] = { 
    
    if(points.size == numberOfPoints) {
      points 
    }
    else {
      points = sampler.sample(boxedRegion, numberOfPoints)
      points
    }
  }
    
  def sampledPoints = points
}

case class IntegratorConfiguration[D <: Dim](sampler: Sampler[D], numberOfPoints : Int)

case class Integrator[D <: Dim](configuration: IntegratorConfiguration[D]) {

  def sampler = configuration.sampler

  def integrateScalar(img: ContinuousScalarImage[D], integrationRegion: BoxedDomain[D]): Double = {
    integrateScalar(img.liftPixelValue, integrationRegion)
  }

  def integrateScalar(f: Function1[Point[D], Option[Double]], integrationRegion: BoxedDomain[D]): Double = {
        println("before integration vector")
    val sampleValues = configuration.sampler.sample(integrationRegion, configuration.numberOfPoints).par.map(f)
    println("number of samples integrated " +sampleValues.size)
    val sum = sampleValues.map(_.getOrElse(0.)).sum
    val ndVolume = integrationRegion.volume

    sum * ndVolume / (configuration.numberOfPoints - 1).toDouble
  }

  def integrateVector(img: ContinuousVectorImage[D], integrationRegion: BoxedDomain[D]): DenseVector[Double] = {

        println("before integration vector")
    val sampleValues = configuration.sampler.sample(integrationRegion, configuration.numberOfPoints).par.map(img.liftPixelValue)
    println("number of samples integrated " +sampleValues.size)
    val ndVolume = integrationRegion.volume;


    val zeroVector = DenseVector.zeros[Double](img.pixelDimensionality)
    val sum: DenseVector[Double] = sampleValues.map(_.getOrElse(zeroVector)).foldLeft(zeroVector)((a, b) => { a + b })
    sum * ndVolume / (configuration.numberOfPoints - 1).toDouble

  }
}


