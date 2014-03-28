package org.statismo.stk.core
package numerics

import org.statismo.stk.core.common.BoxedDomain
import org.statismo.stk.core.geometry._
import org.statismo.stk.core.common.BoxedDomain2D
import org.statismo.stk.core.common.BoxedDomain1D
import org.statismo.stk.core.common.BoxedDomain3D
import org.statismo.stk.core.mesh.TriangleMesh
import breeze.stats.distributions.RandBasis
import org.statismo.stk.core.statisticalmodel.GaussianProcess

/** sample generator typeclass */
trait Sampler[D <: Dim, +Pt <: Point[D]] {
  
  val numberOfPoints : Int
  /**
   * sample n points (x_1, ... x_n), yielding an sequence of (x_i, p(x_i)), i=1..n , p is the probbility density function
   *  according to which the points are sampled
   */
  def sample: IndexedSeq[(Pt, Double)]

  def volumeOfSampleRegion: Double
}

case class UniformSampler1D(val domain: BoxedDomain1D, val numberOfPoints: Int)  extends Sampler[OneD, Point1D]{

  def volumeOfSampleRegion = domain.volume
  val p = 1.0 / domain.volume

  override def sample = {
    val step = (domain.extent(0) - domain.origin(0)) / numberOfPoints.toFloat
    for (i <- 0 until numberOfPoints) yield (Point1D(domain.origin(0) + i * step), p)
  }

}

case class UniformSampler2D(val domain: BoxedDomain[TwoD], val numberOfPoints: Int) extends Sampler[TwoD, Point2D] {

  val p = 1.0 / domain.volume
  def volumeOfSampleRegion = domain.volume
  // TODO this actually samples less points than it is supposed to . 
  // Check how we can fix it.
  override def sample = {
    if (math.pow(math.floor(math.sqrt(numberOfPoints)), 2) < numberOfPoints ) {
      throw new Exception("The number of points for sampling needs to be a power of 2")
    }
    val nbPerDim = math.sqrt(numberOfPoints).floor.toInt
    val step0 = (domain.extent(0) - domain.origin(0)) / nbPerDim
    val step1 = (domain.extent(1) - domain.origin(1)) / nbPerDim
    for (i <- 0 until nbPerDim; j <- 0 until nbPerDim) yield (Point2D(domain.origin(0) + i * step0, domain.origin(1) + j * step1), p)
  }
}

case class UniformSampler3D(val domain: BoxedDomain3D, val numberOfPoints: Int) extends Sampler[ThreeD, Point3D] {
  val p = 1.0 / domain.volume
  def volumeOfSampleRegion = domain.volume
  override def sample = {
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

case class UniformDistributionRandomSampler1D(val domain: BoxedDomain1D, val numberOfPoints: Int) extends Sampler[OneD, Point1D] {
  def volumeOfSampleRegion = domain.volume
  val p = 1.0 / domain.volume
  override def sample = {
    val distr = breeze.stats.distributions.Uniform(domain.origin(0), domain.extent(0))
    (0 until numberOfPoints).map(i => (Point1D(distr.draw.toFloat), p))
  }
}

case class UniformDistributionRandomSampler2D(val domain: BoxedDomain[TwoD], val numberOfPoints: Int) extends Sampler[TwoD, Point2D] {
  def volumeOfSampleRegion = domain.volume
  val p = 1.0 / domain.volume
  override def sample = {
    val distrDim1 = breeze.stats.distributions.Uniform(domain.origin(0), domain.extent(0))
    val distrDim2 = breeze.stats.distributions.Uniform(domain.origin(1), domain.extent(1))

    (0 until numberOfPoints).map(i => (Point2D(distrDim1.draw.toFloat, distrDim2.draw.toFloat), p))
  }
}

case class UniformDistributionRandomSampler3D(val domain: BoxedDomain[ThreeD], val numberOfPoints: Int) extends Sampler[ThreeD, Point3D] {
  def volumeOfSampleRegion = domain.volume
  val p = 1.0 / domain.volume
  def sample = {
    val distrDim1 = breeze.stats.distributions.Uniform(domain.origin(0), domain.extent(0))
    val distrDim2 = breeze.stats.distributions.Uniform(domain.origin(1), domain.extent(1))
    val distrDim3 = breeze.stats.distributions.Uniform(domain.origin(2), domain.extent(2))

    (0 until numberOfPoints).map(i => (Point3D(distrDim1.draw.toFloat, distrDim2.draw.toFloat, distrDim3.draw.toFloat), p))
  }
}

case class SampleOnceSampler[D <: Dim, Pt <: Point[D]](val sampler: Sampler[D, Pt]) extends Sampler[D, Pt] {
  
  val numberOfPoints = sampler.numberOfPoints
  def volumeOfSampleRegion = sampler.volumeOfSampleRegion
  var sampleValues: IndexedSeq[(Pt, Double)] = IndexedSeq()
  override def sample: IndexedSeq[(Pt, Double)] = {

    if (sampleValues.size != 0) {
      sampleValues
    } else {
      sampleValues = sampler.sample
      sampleValues
    }
  }
}

case class RandomMeshSampler3D(mesh: TriangleMesh, val numberOfPoints: Int, seed: Int) extends Sampler[ThreeD, Point[ThreeD]] {

  val p = 1.0 / mesh.area // should be replaced with real mesh volume
  val volumeOfSampleRegion = mesh.area
  def sample = {
    val points = mesh.points.force
    val m = new breeze.stats.random.MersenneTwister(seed = seed)
    val distrDim1 = breeze.stats.distributions.Uniform(0, mesh.numberOfPoints)(new RandBasis(m))
    val pts = (0 until numberOfPoints).map(i => (points(distrDim1.draw().toInt), p))
    pts
  }
}

case class PointsWithLikelyCorrespondenceSampler(gp : GaussianProcess[ThreeD], refmesh: TriangleMesh, targetMesh : TriangleMesh, maxMd : Double) extends Sampler[ThreeD, Point[ThreeD]] {

//  val meanPts = refmesh.points.map(gp.mean(_).toPoint)
  val meanPts = refmesh.points.map {x: Point[ThreeD] => x + gp.mean(x)}
  val ptsWithDist = refmesh.points.zipWithIndex.par
    .map {case (refPt, refPtId)  => {
    val (closestTgtPt, _) = targetMesh.findClosestPoint(meanPts(refPtId))
    (refPt, gp.marginal(refPt).mahalanobisDistance((closestTgtPt - refPt).toBreezeVector))
  }

  }

  val pts = ptsWithDist
    .filter{ case (refPt, dist) => dist <  maxMd }
    .map{case (refPt, dist)=> (refPt, 1.0)}
    .map{case (refPt, dist)=> (refPt, 1.0)}
    .toIndexedSeq

  override val volumeOfSampleRegion = 1.0
  override val numberOfPoints = pts.size
  override def sample = { println(s"Sampled: $numberOfPoints"); pts }

}

case class FixedPointsUniformMeshSampler3D(mesh: TriangleMesh, val numberOfPoints: Int, seed: Int) extends Sampler[ThreeD, Point[ThreeD]] {
  val p = 1.0 / mesh.area
  val volumeOfSampleRegion = mesh.area
  val areas = mesh.cells.map(mesh.computeTriangleArea)
  val min = breeze.stats.DescriptiveStats.percentile(areas, 0.005)

  val ratios = areas.map(s => (s / min).ceil.toInt)
  
  if(ratios.max > 100000)
	  throw new Exception("Mesh traingulation is unregular. Differences in triangle areas are too big.")

  val sum = ratios.sum
  scala.util.Random.setSeed(seed)

  val replicatedCells = new Array[Int](ratios.sum)
  var offset = 0
  ratios.zipWithIndex.foreach(pair => {
    val cellID = pair._2
    val mult = pair._1
    for (i <- 0 until mult) {
      replicatedCells(offset) = cellID
      offset = offset + 1
    }
  })

  val replicatedCellsSize = replicatedCells.size
  
  
  val samplePoints = for (i <- 0 until numberOfPoints) yield {
    val drawnIndex = scala.util.Random.nextInt(replicatedCellsSize)
    val cellidx = replicatedCells(drawnIndex)
    mesh.samplePointInTriangleCell(mesh.cells(cellidx))
  }
  assert(samplePoints.size == numberOfPoints)

  def sample = {
    samplePoints.map(pt => (pt, p))
  }
}

case class FixedPointsMeshSampler3D(mesh: TriangleMesh, val numberOfPoints: Int, seed: Int) extends Sampler[ThreeD, Point[ThreeD]] {

  val volumeOfSampleRegion = mesh.area
  val p = 1.0 / mesh.area

  scala.util.Random.setSeed(seed)
  val meshPoints = mesh.points.force
  val samplePoints = for (i <- 0 until numberOfPoints) yield {
    val idx = scala.util.Random.nextInt(mesh.numberOfPoints)
    meshPoints(idx)
  }
  assert(samplePoints.size == numberOfPoints)

  def sample = {
    samplePoints.map(pt => (pt, p))
  }
}


//}
