package scalismo.mesh.kdtree

import scalismo.common.BoxDomain
import scalismo.geometry.{Point, _3D}
import scalismo.mesh.TriangleMesh
import scalismo.numerics.UniformSampler
import scalismo.registration.LandmarkRegistration
import scalismo.tetramesh.TetrahedralMesh
import scalismo.utils.Random

object MeshVolumeMetrics {

  /**
    * For each point of the first mesh volume, this method computes the shortest distance to the surface of the
    * second mesh and returns the average over all points
    */

  def avgDistance(m1: TetrahedralMesh[_3D], m2: TetrahedralMesh[_3D]): Double = {

    val dists = for (ptM1 <- m1.pointSet.points) yield {
      val cpM2 = m2.pointSet.findClosestPoint(ptM1).point
      (ptM1 - cpM2).norm
    }
    dists.sum / m1.pointSet.numberOfPoints
  }

  /**
    * Returns the average mesh volume distance after performing a rigid alignment between the two meshes.
    * All mesh points are used for the rigid alignment, therefore both meshes must be in correspondence
    */
  def procrustesDistance(m1: TetrahedralMesh[_3D], m2: TetrahedralMesh[_3D]): Double = {
    require(m1.pointSet.numberOfPoints == m2.pointSet.numberOfPoints)

    val landmarks = m1.pointSet.points.toIndexedSeq zip m2.pointSet.points.toIndexedSeq
    val t = LandmarkRegistration.rigid3DLandmarkRegistration(landmarks, Point(0, 0, 0))
    val m1w = m1.transform(t)
    avgDistance(m1w, m2)
  }

  /**
    * Returns the Hausdorff distance between the two meshes
    */
  def hausdorffDistance(m1: TetrahedralMesh[_3D], m2: TetrahedralMesh[_3D]): Double = {
    def allDistsBetweenMeshes(mm1: TetrahedralMesh[_3D], mm2: TetrahedralMesh[_3D]): Iterator[Double] = {
      for (ptM1 <- mm1.pointSet.points) yield {
        val cpM2 = mm2.pointSet.findClosestPoint(ptM1).point
        (ptM1 - cpM2).norm
      }
    }

    val d1 = allDistsBetweenMeshes(m1, m2)
    val d2 = allDistsBetweenMeshes(m2, m1)

    Math.max(d1.max, d2.max)

  }

  /**
    * Computes a binary image for each mesh and returns the Dice Coefficient between the two images
    */
 /* def diceCoefficient(m1: TetrahedralMesh[_3D], m2: TetrahedralMesh[_3D])(implicit rand: Random): Double = {
    val imgA = m1.operations.toBinaryImage
    val imgB = m2.operations.toBinaryImage

    def minPoint(pt1: Point[_3D], pt2: Point[_3D]) = Point(math.min(pt1(0), pt2(0)), math.min(pt1(1), pt2(1)), math.min(pt1(2), pt2(2)))
    def maxPoint(pt1: Point[_3D], pt2: Point[_3D]) = Point(math.max(pt1(0), pt2(0)), math.max(pt1(1), pt2(1)), math.max(pt1(2), pt2(2)))

    val box1 = m1.pointSet.boundingBox
    val box2 = m2.pointSet.boundingBox
    val evaluationRegion = BoxDomain(minPoint(box1.origin, box2.origin), maxPoint(box1.oppositeCorner, box2.oppositeCorner))

    val sampler = UniformSampler[_3D](evaluationRegion, 10000)
    val samplePts = sampler.sample().map(_._1)

    val numSamplesInA = samplePts.map(imgA).sum
    val numSamplesInB = samplePts.map(imgB).sum
    val AIntersectB = (imgA + imgB) andThen ((v: Float) => if (v > 1 + 1e-5) 1f else 0f)

    val numSamplesInAIB = samplePts.map(AIntersectB).sum
    2 * numSamplesInAIB / (numSamplesInA + numSamplesInB)
  }*/

}
