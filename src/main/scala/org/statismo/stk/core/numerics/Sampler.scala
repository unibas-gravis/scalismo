package org.statismo.stk.core
package numerics

import org.statismo.stk.core.common.BoxedDomain
import org.statismo.stk.core.geometry._
import org.statismo.stk.core.common.BoxedDomain2D
import org.statismo.stk.core.common.BoxedDomain1D
import org.statismo.stk.core.common.BoxedDomain3D

/** sample generator typeclass */
trait Sampler[D <: Dim, +Pt <: Point[D]] {
  /**
   * sample n points (x_1, ... x_n), yielding an sequence of (x_i, p(x_i)), i=1..n , p is the probbility density function
   *  according to which the points are sampled
   */
  def sample(numberOfPoints: Int): IndexedSeq[(Pt, Double)]

  def volumeOfSampleRegion: Double
}

case class UniformSampler1D(val domain: BoxedDomain1D)  extends Sampler[OneD, Point1D]{

  def volumeOfSampleRegion = domain.volume
  val p = 1.0 / domain.volume

  override def sample(numberOfPoints: Int) = {
    val step = (domain.extent(0) - domain.origin(0)) / numberOfPoints.toFloat
    for (i <- 0 until numberOfPoints) yield (Point1D(domain.origin(0) + i * step), p)
  }

}

case class UniformSampler2D(val domain: BoxedDomain2D) extends Sampler[TwoD, Point2D] {

  val p = 1.0 / domain.volume
  def volumeOfSampleRegion = domain.volume
  // TODO this actually samples less points than it is supposed to . 
  // Check how we can fix it.
  override def sample(numberOfPoints: Int) = {
    if (math.pow(math.floor(math.sqrt(numberOfPoints)), 2) < numberOfPoints ) {
      throw new Exception("The number of points for sampling needs to be a power of 2")
    }
    val nbPerDim = math.sqrt(numberOfPoints).floor.toInt
    val step0 = (domain.extent(0) - domain.origin(0)) / nbPerDim
    val step1 = (domain.extent(1) - domain.origin(1)) / nbPerDim
    for (i <- 0 until nbPerDim; j <- 0 until nbPerDim) yield (Point2D(domain.origin(0) + i * step0, domain.origin(1) + j * step1), p)
  }
}

case class UniformSampler3D(val domain: BoxedDomain3D) extends Sampler[ThreeD, Point3D] {
  val p = 1.0 / domain.volume
  def volumeOfSampleRegion = domain.volume
  override def sample(numberOfPoints: Int) = {
    if (math.pow(math.floor(math.cbrt(numberOfPoints)), 3) < numberOfPoints ) {
      throw new Exception("The number of points for sampling needs to be a power of 3")
    }
    
    val nbPerDim = math.cbrt(numberOfPoints).floor.toInt
    val step0 = (domain.extent(0) - domain.origin(0)) / nbPerDim
    val step1 = (domain.extent(1) - domain.origin(1)) / nbPerDim
    val step2 = (domain.extent(2) - domain.origin(2)) / nbPerDim

    for (i <- 0 until nbPerDim; j <- 0 until nbPerDim; k <- 0 until nbPerDim)
      yield (Point3D(domain.origin(0) + i * step0, domain.origin(1) + j * step1, domain.origin(2) + k * step2), p)
  }
}

case class UniformDistributionRandomSampler1D(val domain: BoxedDomain1D) extends Sampler[OneD, Point1D] {
  def volumeOfSampleRegion = domain.volume
  val p = 1.0 / domain.volume
  override def sample(numberOfPoints: Int) = {
    val distr = breeze.stats.distributions.Uniform(domain.origin(0), domain.extent(0))
    (0 until numberOfPoints).map(i => (Point1D(distr.draw.toFloat), p))
  }
}

case class UniformDistributionRandomSampler2D(val domain: BoxedDomain[TwoD]) extends Sampler[TwoD, Point2D] {
  def volumeOfSampleRegion = domain.volume
  val p = 1.0 / domain.volume
  override def sample(numberOfPoints: Int) = {
    val distrDim1 = breeze.stats.distributions.Uniform(domain.origin(0), domain.extent(0))
    val distrDim2 = breeze.stats.distributions.Uniform(domain.origin(1), domain.extent(1))

    (0 until numberOfPoints).map(i => (Point2D(distrDim1.draw.toFloat, distrDim2.draw.toFloat), p))
  }
}

case class UniformDistributionRandomSampler3D(val domain: BoxedDomain[ThreeD]) extends Sampler[ThreeD, Point3D] {
  def volumeOfSampleRegion = domain.volume
  val p = 1.0 / domain.volume
  def sample(numberOfPoints: Int) = {
    val distrDim1 = breeze.stats.distributions.Uniform(domain.origin(0), domain.extent(0))
    val distrDim2 = breeze.stats.distributions.Uniform(domain.origin(1), domain.extent(1))
    val distrDim3 = breeze.stats.distributions.Uniform(domain.origin(2), domain.extent(2))

    (0 until numberOfPoints).map(i => (Point3D(distrDim1.draw.toFloat, distrDim2.draw.toFloat, distrDim3.draw.toFloat), p))
  }
}

case class SampleOnceSampler[D <: Dim, Pt <: Point[D]](val sampler: Sampler[D, Pt]) extends Sampler[D, Pt] {
  def volumeOfSampleRegion = sampler.volumeOfSampleRegion
  var sampleValues: IndexedSeq[(Pt, Double)] = IndexedSeq()
  override def sample(numberOfPoints: Int): IndexedSeq[(Pt, Double)] = {

    if (sampleValues.size == numberOfPoints) {
      sampleValues
    } else {
      sampleValues = sampler.sample(numberOfPoints)
      sampleValues
    }
  }
}



//}
