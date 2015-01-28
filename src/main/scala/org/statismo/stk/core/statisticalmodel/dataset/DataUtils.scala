package org.statismo.stk.core.statisticalmodel
package dataset

import java.io.File

import scala.util.Failure
import scala.util.Success
import scala.util.Try

import org.statismo.stk.core.geometry.Point
import org.statismo.stk.core.geometry._3D
import org.statismo.stk.core.io.MeshIO
import org.statismo.stk.core.mesh.TriangleMesh
import org.statismo.stk.core.registration.Transformation


private object DataUtils {
  /**
   *  Partitions a list os possible transformation (tries) into those that succeeded and those who failed
   */
  def partitionSuccAndFailedTries[A](tries: Seq[Try[A]]): (Seq[A], Seq[Throwable]) = {
    val (s, f) = tries.partition(_.isSuccess)
    val throwables = for (failed <- f) yield {
      failed match {
        case Failure(t) => t
        case _ => new Throwable("This will never happen")
      }
    }
    val transforms = s.map(_.get)
    (transforms, throwables)
  }

 
  /**
   * Create a transformation from a mesh. The transformation maps from the reference mesh to the corresponding target point.
   */
  def meshToTransformation(refMesh: TriangleMesh, targetMesh: TriangleMesh): Try[Transformation[_3D]] = {
    if (refMesh.numberOfPoints != targetMesh.numberOfPoints)
      Failure(new Throwable(s"reference and target mesh do not have the same number of points (${refMesh.numberOfPoints} != ${targetMesh.numberOfPoints}"))
    else {
      val t = new Transformation[_3D] {
        val targetPts = targetMesh.points.toIndexedSeq
        override def apply(x: Point[_3D]): Point[_3D] = {
          val (_, ptId) = refMesh.findClosestPoint(x)
          targetPts(ptId)
        }
      }
      Success(t)
    }
  }

}