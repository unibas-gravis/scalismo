package smptk.mesh

import smptk.image.ContinuousScalarImage3D
import smptk.image.Geometry.CoordVector3D
import breeze.linalg.DenseVector

object Mesh {

  /**
   * Creates a new ContinuousScalarImage  defined on R^3, which is the distance transform of the mesh
   */
  def meshToDistanceImage(mesh: TriangleMesh): ContinuousScalarImage3D = {

    def dist(pt: CoordVector3D[Double]) = {
      val (closestPt, _) = mesh.findClosestPoint(pt)
      Math.sqrt(Math.pow(closestPt(0) - pt(0), 2) + Math.pow(closestPt(1) - pt(1), 2) + Math.pow(closestPt(2) - pt(2), 2))
    }
    def grad(pt: CoordVector3D[Double]) = {
      val (closestPt, _) = mesh.findClosestPoint(pt)
      val grad = DenseVector(pt(0) - closestPt(0) , pt(1) - closestPt(1) , pt(2) - closestPt(2) )
      grad / breeze.linalg.norm(grad, 2)
    }
    ContinuousScalarImage3D((pt: CoordVector3D[Double]) => true, (pt: CoordVector3D[Double]) => dist(pt), Some((pt: CoordVector3D[Double]) => grad(pt)))
  }

}