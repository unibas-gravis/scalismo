package smptk.mesh

import smptk.image.ContinuousScalarImage3D
import breeze.linalg.DenseVector
import smptk.geometry.{Point, ThreeD}
import smptk.common.RealSpace3D


object Mesh {

  /**
   * Creates a new ContinuousScalarImage  defined on R^3, which is the distance transform of the mesh
   */
  def meshToDistanceImage(mesh: TriangleMesh): ContinuousScalarImage3D = {

    def dist(pt: Point[ThreeD]) = {
      val (closestPt, _) = mesh.findClosestPoint(pt)
      Math.sqrt(Math.pow(closestPt(0) - pt(0), 2) + Math.pow(closestPt(1) - pt(1), 2) + Math.pow(closestPt(2) - pt(2), 2))
    }
    def grad(pt: Point[ThreeD]) = {
      val (closestPt, _) = mesh.findClosestPoint(pt)
      val grad = DenseVector(pt(0) - closestPt(0) , pt(1) - closestPt(1) , pt(2) - closestPt(2) )
      grad / breeze.linalg.norm(grad, 2)
    }
    ContinuousScalarImage3D(RealSpace3D, (pt: Point[ThreeD]) => dist(pt), Some((pt: Point[ThreeD]) => grad(pt)))
  }

}