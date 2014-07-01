package org.statismo.stk.core.mesh

import scala.language.implicitConversions
import org.statismo.stk.core.io.MeshIO
import org.statismo.stk.core.geometry._
import java.io.File
import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import org.statismo.stk.core.registration.RotationSpace3D
import breeze.linalg.DenseVector
import org.statismo.stk.core.registration.ProductTransformationSpace
import org.statismo.stk.core.registration.ScalingSpace3D
import org.statismo.stk.core.registration.ScalingSpace3D
import org.statismo.stk.core.registration.RotationSpace3D
import org.statismo.stk.core.image.Resample
import org.statismo.stk.core.common.BoxedDomain3D
import org.statismo.stk.core.io.ImageIO
import org.statismo.stk.core.image.DiscreteImageDomain3D
import org.statismo.stk.core.image.DiscreteImageDomain3D

class MeshTests extends FunSpec with ShouldMatchers {

  implicit def doubleToFloat(d: Double) = d.toFloat

  org.statismo.stk.core.initialize()

  describe("a mesh") {
    val path = getClass().getResource("/facemesh.h5").getPath
    val facemesh = MeshIO.readHDF5(new File(path)).get

    it("finds the right closest points for all the points that define the mesh") {

      for ((pt, id) <- facemesh.points.zipWithIndex) {
        val (closestPt, closestId) = facemesh.findClosestPoint(pt)
        assert(closestPt === pt)
        assert(closestId === id)
      }
    }
    it("finds the right closest point for a point that is not defined on the mesh") {
      val pts = IndexedSeq(Point3D(0.0, 0.0, 0.0), Point3D(1.0, 1.0, 1.0), Point3D(1.0, 1.0, 5.0))
      val cells = IndexedSeq(TriangleCell(0, 1, 2))
      val mesh = TriangleMesh(pts, cells)

      val newPt = Point3D(1.1, 1.1, 4)
      val (closestPt, closestPtId) = mesh.findClosestPoint(newPt)
      assert(closestPtId === 2)
      assert(closestPt === pts(2))

    }
    it("computes its area correctly for a triangle") {
      val pts: IndexedSeq[Point3D] = IndexedSeq((0.0, 0.0, 0.0), (0.0, 1.0, 0.0), (1.0, 0.0, 0.0))
      val cells = IndexedSeq(TriangleCell(0, 1, 2))
      val mesh = TriangleMesh(pts, cells)

      val R = RotationSpace3D((0.0, 0.0, 0.0))(DenseVector(0.3, 0.4, 0.1))
      val s = ScalingSpace3D()(DenseVector(2.0))
      val transformedMesh = mesh.warp(R).warp(s)
      mesh.area should be(0.5 plusOrMinus 1e-8)
      transformedMesh.area should be(4.0f * mesh.area plusOrMinus 1e-5) // scaling by two gives 4 times the area 
    }

    // ignored until more meaningful test (It's normal that more points are deleted)
    ignore("can be clipped") {
      def ptIdSmallerThan100(pt: Point[ThreeD]) = facemesh.findClosestPoint(pt)._2 < 100
      val clippedMesh = Mesh.clipMesh(facemesh, ptIdSmallerThan100 _)

      clippedMesh.numberOfPoints should be(facemesh.numberOfPoints - 100)
    }

    it("computes the right binary image for the unit sphere") {
      val path = getClass().getResource("/unit-sphere.vtk").getPath
      val spheremesh = MeshIO.readMesh(new File(path)).get
      val binaryImg = Mesh.meshToBinaryImage(spheremesh)
      binaryImg(Point3D(0,0,0)) should be(1)
      binaryImg(Point3D(2,0,0)) should be(0)
               
    }

  }

}