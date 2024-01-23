package scalismo.io.stl

import scalismo.common.PointId
import scalismo.geometry.{EuclideanVector3D, Point3D}
import scalismo.mesh.{TriangleCell, TriangleList, TriangleMesh3D}

import scala.collection.mutable

private[stl] case class STLTriangle(n: EuclideanVector3D, p1: Point3D, p2: Point3D, p3: Point3D)

object STLTriangle {
  def STLTrianglesToTriangleMesh(stlTriangles: Seq[STLTriangle]): TriangleMesh3D = {
    val pointsMap = mutable.HashMap.empty[Point3D, Int]
    val pointsArray = new Array[Point3D](stlTriangles.length * 3)
    var pointCounter = 0

    def getOrCreatePointId(point: Point3D): Int =
      pointsMap.getOrElseUpdate(point, {
                                  val id = pointCounter
                                  pointsArray(id) = point
                                  pointCounter += 1
                                  id
                                }
      )

    val triangles = stlTriangles.map { triangle =>
      val ptId1 = getOrCreatePointId(triangle.p1)
      val ptId2 = getOrCreatePointId(triangle.p2)
      val ptId3 = getOrCreatePointId(triangle.p3)
      TriangleCell(PointId(ptId1), PointId(ptId2), PointId(ptId3))
    }.toIndexedSeq
    TriangleMesh3D(pointsArray.take(pointCounter).toIndexedSeq, TriangleList(triangles))
  }
}
