package org.statismo.stk.core.mesh

import org.statismo.stk.core.common.RealSpace
import org.statismo.stk.core.image.{DifferentiableScalarImage, ScalarImage}
import org.statismo.stk.core.geometry.{ Point, _3D, Vector }

object Mesh {

  /**
   * Creates a new ContinuousScalarImage  defined on R^^3, which is the distance transform of the mesh
   */
  def meshToDistanceImage(mesh: TriangleMesh): ScalarImage[_3D] = {

    def dist(pt: Point[_3D]): Float = {
      val (closestPt, _) = mesh.findClosestPoint(pt)
      Math.sqrt(Math.pow(closestPt(0) - pt(0), 2) + Math.pow(closestPt(1) - pt(1), 2) + Math.pow(closestPt(2) - pt(2), 2)).toFloat
    }
    def grad(pt: Point[_3D]) = {
      val (closestPt, _) = mesh.findClosestPoint(pt)
      val grad = Vector(pt(0) - closestPt(0), pt(1) - closestPt(1), pt(2) - closestPt(2))
      grad * (1.0 / grad.norm)
    }
    DifferentiableScalarImage(RealSpace[_3D], (pt: Point[_3D]) => dist(pt), (pt: Point[_3D]) => grad(pt))
  }

  def meshToBinaryImage(mesh: TriangleMesh): ScalarImage[_3D] = {
    def inside(pt: Point[_3D]): Short = {
      val closestMeshPt = mesh.findClosestPoint(pt)._1
      val dotprod = mesh.normalAtPoint(closestMeshPt) dot (closestMeshPt - pt)
      if (dotprod > 0.0) 1 else 0
    }

    ScalarImage(RealSpace[_3D], (pt: Point[_3D]) => inside(pt))
  }

  /**
   * Clip all the points in a mesh that satisfy the given predicate
   */

  def clipMesh(mesh: TriangleMesh, clipPointPredicate: Point[_3D] => Boolean) : TriangleMesh = {

    // predicate tested at the beginning, once.
    val remainingPoints =  mesh.points.toIndexedSeq.par.filter{ !clipPointPredicate(_)}.zipWithIndex.toMap
    val pts = mesh.points.toIndexedSeq
    
    val remainingPointTriplet = mesh.cells.par.map {
      cell => 
        val points = cell.pointIds.map(pts)
        (points, points.map(p => remainingPoints.get(p).isDefined).reduce(_ && _))
    }.filter(_._2).map(_._1)
    

    val points = remainingPointTriplet.flatten.distinct
    val pt2Id = points.zipWithIndex.toMap
    val cells = remainingPointTriplet.map { case vec => TriangleCell(pt2Id(vec(0)), pt2Id(vec(1)), pt2Id(vec(2))) }

    TriangleMesh(points.toIndexedSeq, cells.toIndexedSeq)
  }

}