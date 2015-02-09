package scalismo.mesh

import scalismo.common.BoxDomain
import scalismo.geometry.{_3D, Point}
import scalismo.numerics.UniformSampler
import scalismo.registration.LandmarkRegistration

/**
 * Implements utility methods for evaluating similarity of [[TriangleMesh]] instances  
 * 
 * */
object MeshMetrics {
 
  /**
   * For each point of the first mesh, this method computes the distance to the closest point on the 
   * second mesh and returns the average over all points 
   * */
  
  def avgDistance(m1: TriangleMesh, m2: TriangleMesh): Double = {

    val dists = for (ptM1 <- m1.points) yield {
      val (cpM2, _) = m2.findClosestPoint(ptM1)
      (ptM1 - cpM2).norm
    }
    dists.sum / m1.numberOfPoints
  }
  
  
  /**
   * Returns the average mesh distance after performing a rigid alignment between the two meshes.
   * All mesh points are used for the rigid alignment, therefore both meshes must be in correspondence  
   * */
  def procrustesDistance(m1: TriangleMesh, m2: TriangleMesh): Double = {
    require(m1.numberOfPoints == m2.numberOfPoints)

    val landmarks = m1.points.toIndexedSeq zip m2.points.toIndexedSeq
    val x = LandmarkRegistration.rigid3DLandmarkRegistration(landmarks)
    val m1w = m1.transform(x.transform)
    avgDistance(m1w, m2)
  }


 /**
   * Returns the Hausdorff distance between the two meshes
   * */
  def hausdorffDistance(m1: TriangleMesh, m2: TriangleMesh): Double = {
    def allDistsBetweenMeshes(mm1: TriangleMesh, mm2: TriangleMesh): Iterator[Double] = {
      for (ptM1 <- mm1.points) yield {
        val (cpM2, _) = mm2.findClosestPoint(ptM1)
        (ptM1 - cpM2).norm
      }
    }
    
    val d1 = allDistsBetweenMeshes(m1, m2)
    val d2 = allDistsBetweenMeshes(m2, m1)
    
    Math.max(d1.max, d2.max)
    
  }

  /**
   * Computes a binary image for each mesh and returns the Dice Coefficient between the two images 
   * */
  def diceCoefficient(m1: TriangleMesh, m2 : TriangleMesh) : Double = { 
    val imgA = Mesh.meshToBinaryImage(m1)
    val imgB = Mesh.meshToBinaryImage(m2)
    
    def minPoint(pt1 : Point[_3D], pt2 : Point[_3D]) = Point(math.min(pt1(0), pt2(0)), math.min(pt1(1), pt2(1)), math.min(pt1(2), pt2(2)))  
    def maxPoint(pt1 : Point[_3D], pt2 : Point[_3D]) = Point(math.max(pt1(0), pt2(0)), math.max(pt1(1), pt2(1)), math.max(pt1(2), pt2(2)))
    
    val box1 = m1.boundingBox
    val box2 = m2.boundingBox
    val evaluationRegion = BoxDomain[_3D](minPoint(box1.origin, box2.origin), maxPoint(box1.oppositeCorner, box2.oppositeCorner))
    
    val sampler = UniformSampler[_3D](evaluationRegion, 10000)
    val samplePts = sampler.sample.map(_._1)
    
    val numSamplesInA = samplePts.map(imgA).sum
    val numSamplesInB = samplePts.map(imgB).sum
    val AIntersectB = (imgA + imgB) andThen ((v : Float) => if (v > 1 + 1e-5 ) 1f else 0f)      

    val numSamplesInAIB = samplePts.map(AIntersectB).sum
    2 * numSamplesInAIB / (numSamplesInA + numSamplesInB)
  }
  
}
