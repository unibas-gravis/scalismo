package org.statismo.stk.core.mesh

import org.statismo.stk.core.image.ContinuousScalarImage3D
import breeze.linalg.DenseVector
import org.statismo.stk.core.geometry.{ Point, ThreeD }
import org.statismo.stk.core.common.RealSpace3D
import org.statismo.stk.core.geometry.Vector3D
import scala.collection.immutable.SortedSet
import scala.collection.immutable.HashSet
import scala.collection.immutable.HashSet.HashSet1
import scala.collection.immutable.HashSet.HashSet1
import scala.collection.mutable.HashMap
import org.statismo.stk.core.geometry.Point3D

object Mesh {

  /**
   * Creates a new ContinuousScalarImage  defined on R^3, which is the distance transform of the mesh
   */
  def meshToDistanceImage(mesh: TriangleMesh): ContinuousScalarImage3D = {

    def dist(pt: Point[ThreeD]): Float = {
      val (closestPt, _) = mesh.findClosestPoint(pt)
      Math.sqrt(Math.pow(closestPt(0) - pt(0), 2) + Math.pow(closestPt(1) - pt(1), 2) + Math.pow(closestPt(2) - pt(2), 2)).toFloat
    }
    def grad(pt: Point[ThreeD]) = {
      val (closestPt, _) = mesh.findClosestPoint(pt)
      val grad = Vector3D(pt(0) - closestPt(0), pt(1) - closestPt(1), pt(2) - closestPt(2))
      grad * (1.0 / grad.norm)
    }
    ContinuousScalarImage3D(RealSpace3D, (pt: Point[ThreeD]) => dist(pt), Some((pt: Point[ThreeD]) => grad(pt)))
  }

  def meshToBinaryImage(mesh: TriangleMesh): ContinuousScalarImage3D = {
    def inside(pt: Point[ThreeD]): Short = {
      val closestMeshPt = mesh.findClosestPoint(pt)._1
      val dotprod = mesh.normalAtPoint(closestMeshPt) dot (closestMeshPt - pt)
      if (dotprod > 0.0) 1 else 0
    }

    ContinuousScalarImage3D(RealSpace3D, (pt: Point[ThreeD]) => inside(pt), None)
  }

  /**
   * Clip all the points in a mesh that satisfy the given predicate
   */

  def clipMesh(mesh: TriangleMesh, clipPointPredicate: Point[ThreeD] => Boolean) : TriangleMesh = {

    val remainingPointTriplet = for {
      cell <- mesh.cells
      val points = cell.pointIds.map(mesh.points)
      val validCell = points.map(p => !clipPointPredicate(p)).reduce(_ && _)
      if (validCell)
    } yield { points.toIndexedSeq }

    val points = remainingPointTriplet.flatten.distinct
    val pt2Id = points.zipWithIndex.toMap
    val cells = remainingPointTriplet.map { case vec => TriangleCell(pt2Id(vec(0)), pt2Id(vec(1)), pt2Id(vec(2))) }

    TriangleMesh(points, cells)
  }

}