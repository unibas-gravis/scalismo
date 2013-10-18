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

  /**
   * Clip all the points in a mesh that satisfy the given predicate 
   */
  def clipMesh(mesh: TriangleMesh, clipPointPredicate: Point[ThreeD] => Boolean): TriangleMesh = {

    val ptsWithId = mesh.points.zipWithIndex
    val ptsWithIdMap = ptsWithId.toMap
    val (ptsWithIndexToClip, ptsWithIndexToKeep) = ptsWithId.partition { case (pt, id) => clipPointPredicate(pt) }

    val ptIdsToClip = ptsWithIndexToClip.map(_._2).toSet
    val ptsToKeep = ptsWithIndexToKeep.map(_._1)
    val ptsWithNewIndexToKeep = ptsToKeep.zipWithIndex

    // compute a map that holds for every point a tuple the tuple (oldPtId, newPtId). This is later used
    // to correct the cellIds after clipping
    val ptIdMapping = {
      for ((pt, newPtId) <- ptsWithNewIndexToKeep) yield {
        val oldPtId = ptsWithIdMap.get(pt).get
        (pt, (oldPtId, newPtId))
      }
    }.toMap
 
    def newPtIdForOldPtId(ptId: Int): Int = {
      val pt = ptsWithId(ptId)._1
      val (oldPtId, newPtId) = ptIdMapping(pt)
      newPtId
    }
   
    // get all the cells that should be clipped
    val (cellsToClip, cellsToKeep) = mesh.cells.partition(cell => ptIdsToClip.contains(cell.ptId1) || ptIdsToClip.contains(cell.ptId2) || ptIdsToClip.contains(cell.ptId3))

    // rewrite point ids in cells
    val newCellsToKeep = for (cell <- cellsToKeep.par) yield {
      new TriangleCell(newPtIdForOldPtId(cell.ptId1), newPtIdForOldPtId(cell.ptId2), newPtIdForOldPtId(cell.ptId3))
    }
    TriangleMesh(ptsWithIndexToKeep.map { case (pt, ptId) => pt }.toIndexedSeq, newCellsToKeep.toIndexedSeq)
  }

}