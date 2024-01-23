package scalismo.mesh.decimate

import scalismo.common.PointId
import scalismo.geometry.{_3D, EuclideanVector3D, Point, SquareMatrix}
import scalismo.mesh.{TriangleCell, TriangleList, TriangleMesh, TriangleMesh3D}

import scala.collection.mutable.ArrayBuffer
// Based on https://github.com/sp4cerat/Fast-Quadric-Mesh-Simplification
// and its java implementation: https://gist.github.com/jayfella/00a328b2dbdf6304078a821143b7aef7

case class ErrorEntry(err: Array[Double] = Array.fill(4)(0.0))

case class Triangle(
  v: TriangleCell,
  n: EuclideanVector3D = EuclideanVector3D(0.0, 0.0, 0.0),
  err: ErrorEntry = ErrorEntry(),
  deleted: Boolean = false,
  dirty: Boolean = false
)

case class Vertex(
  p: Point[_3D],
  border: Boolean,
  tstart: Int = 0,
  tcount: Int = 0,
  q: SymmetricMatrix = new SymmetricMatrix(0.0)
)

case class Ref(
  tid: Int,
  tvertex: Int
)

class MeshDecimation(mesh: TriangleMesh[_3D]) {
  val numberOfPoints: Int = mesh.pointSet.numberOfPoints
  val numberOfTriangles: Int = mesh.triangulation.triangles.length
  var triangles: ArrayBuffer[Triangle] = ArrayBuffer.empty
  mesh.triangulation.triangleIds.foreach(tid =>
    triangles += Triangle(
      v = mesh.triangulation.triangle(tid)
    )
  )
  var vertices: ArrayBuffer[Vertex] = ArrayBuffer.empty
  mesh.pointSet.pointIds.toIndexedSeq.foreach(pid =>
    vertices += Vertex(
      p = mesh.pointSet.point(pid),
      border = mesh.operations.pointIsOnBoundary(pid)
    )
  )
  var refs: IndexedSeq[Ref] = mesh.triangulation.triangleIds.flatMap { tid =>
    Seq(
      mesh.triangulation.triangle(tid).ptId1,
      mesh.triangulation.triangle(tid).ptId2,
      mesh.triangulation.triangle(tid).ptId3
    ).map(ptId => Ref(tid = tid.id, tvertex = ptId.id))
  }

  def simplify(targetCount: Int, aggressiveness: Double): TriangleMesh[_3D] = {
    var deletedTriangles = 0

    val deleted0: Vector[Boolean] = Vector.empty[Boolean]
    val deleted1: Vector[Boolean] = Vector.empty[Boolean]

    val maxIterations = 100;

    val triangleCount = numberOfTriangles

    println(s"Target triangle count: ${targetCount}, from ${triangleCount}.")
    var iteration = 0
    while (iteration < maxIterations) {
      println(
        f"Iteration ${iteration} -> triangles [ deleted: ${deletedTriangles} : count: ${triangleCount - deletedTriangles} | removed: ${(deletedTriangles * 100 / triangleCount)}]"
      )

      if (triangleCount - deletedTriangles <= targetCount) {
        iteration = maxIterations
      }

      if (iteration % 5 == 0) {
        updateMesh(iteration)
      }

      val threshold: Double = 0.000000001 * Math.pow((iteration + 3).toDouble, aggressiveness)

      triangles.mapInPlace(_.copy(dirty = false))

      triangles.indices.foreach {
        i => // DO NOT USE FOREACH as the triangles are updated within the loop - yes, I know, very ugly.
          val t = triangles(i)

          if (t.err.err(3) <= threshold && !t.deleted && !t.dirty) {
            var j = 0;
            while (j < 3) {
              if (t.err.err(j) < threshold) {
                val i0 = t.v.pointIds(j).id
                val i1 = t.v.pointIds((j + 1) % 3).id

                val v0 = vertices(i0)
                val v1 = vertices(i1)

                if (v0.border == v1.border) {
                  val (_, p) = calculateError(v0, v1)

                  val (flipped0, deleted0) = flipped(p, i1, v0)
                  val (flipped1, deleted1) = flipped(p, i0, v1)

                  if (!flipped0 && !flipped1) {
                    vertices.update(i0, vertices(i0).copy(p = p.toPoint, q = v0.q.add(v1.q)))
                    val tstart = refs.length

                    deletedTriangles += updateTriangles(i0, v0, deleted0)
                    deletedTriangles += updateTriangles(i0, v1, deleted1)

                    val tcount = refs.length - tstart
                    vertices.update(i0, vertices(i0).copy(tstart = tstart, tcount = tcount))
                    j = 3
                  }
                }
              }
              j += 1
            }
            if (triangleCount - deletedTriangles <= targetCount) {
              iteration = maxIterations
            }
          }
      }
      iteration += 1
    }

    compactMesh()
    println(s"Final triangles count: ${triangles.length}.")
    createSimplifiedMesh()
  }

  private def flipped(p: EuclideanVector3D, i1: Int, v0: Vertex): (Boolean, Array[Boolean]) = {
    val deleted = Array.fill(v0.tcount)(false)
    var returnValue = false
    var k = 0
    val counterMaxValue = v0.tcount
    while (k < counterMaxValue) {
//    (0 until v0.tcount).foreach { k =>
      val ref = refs(v0.tstart + k)
      val t = triangles(ref.tid)

      if (!t.deleted) {
        val s = ref.tvertex
        val id1 = t.v.pointIds((s + 1) % 3).id
        val id2 = t.v.pointIds((s + 2) % 3).id
        if (id1 == i1 || id2 == i1) {
          deleted(k) = true
        } else {
          val d1 = (vertices(id1).p.-(p)).toVector.normalize
          val d2 = (vertices(id2).p.-(p)).toVector.normalize

          if (Math.abs(d1.dot(d2)) > 0.999) {
            returnValue = true
            k = counterMaxValue
          } else {
            val n = d1.crossproduct(d2).normalize
//            deleted(k) = false
            if (n.dot(t.n) < 0.2) {
              returnValue = true
              k = counterMaxValue
            }
          }
        }
      }
      k += 1
    }
    (returnValue, deleted)
  }

  private def updateTriangles(i0: Int, v: Vertex, deleted: Array[Boolean]): Int = {
    var triangleDeleted = 0

    (0 until v.tcount).foreach { k =>
      val r = refs(v.tstart + k)
      val t = triangles(r.tid)

      if (!t.deleted) {
        if (deleted(k)) {
          triangles(r.tid) = t.copy(deleted = true)
          triangleDeleted += 1
        } else {
          val err0 = calculateError(vertices(t.v.ptId1.id), vertices(t.v.ptId2.id))._1
          val err1 = calculateError(vertices(t.v.ptId2.id), vertices(t.v.ptId3.id))._1
          val err2 = calculateError(vertices(t.v.ptId3.id), vertices(t.v.ptId1.id))._1
          val err3 = Math.min(err0, Math.min(err1, err2))
          val err = ErrorEntry(
            Array[Double](
              err0,
              err1,
              err2,
              err3
            )
          )
          val vNew =
            if (r.tvertex == 0) t.v.copy(ptId1 = PointId(i0))
            else if (r.tvertex == 1) t.v.copy(ptId2 = PointId(i0))
            else if (r.tvertex == 2) t.v.copy(ptId3 = PointId(i0))
            else t.v // this state should not be reachable
          triangles(r.tid) = t.copy(
            v = vNew,
            dirty = true,
            err = err
          )
          refs = refs :+ r
        }
      }
    }
    triangleDeleted
  }

  private def calculateNormal(p: Seq[Point[_3D]]): EuclideanVector3D = {
    (p(1) - p(0)).crossproduct(p(2) - p(0)).normalize
  }

  private def cellToPoints(cell: TriangleCell): Seq[Point[_3D]] = {
    (0 until 3).map { j =>
      val id = cell.pointIds(j).id
      val v = vertices(id)
      v.p
    }
  }

  private def updateMesh(iteration: Int): Unit = {
    if (iteration > 0) {
      triangles.filterInPlace(_.deleted == false)
    }

    vertices.mapInPlace(_.copy(tstart = 0, tcount = 0))

    triangles.indices.foreach { i =>
      val t = triangles(i)
      (0 until 3).foreach { j =>
        val id0 = t.v.pointIds(j).id
        val v0 = vertices(id0)
        vertices.update(id0, v0.copy(tcount = v0.tcount + 1))
      }
    }
    var tstart = 0;

    vertices.indices.foreach { i =>
      val v = vertices(i)
      val locTstart = tstart
      tstart += v.tcount
      vertices(i) = v.copy(tstart = locTstart, tcount = 0)
    }

    refs = refs.take(triangles.length * 3)

    triangles.zipWithIndex.foreach { case (t, i) =>
      (0 until 3).foreach { j =>
        val vId = t.v.pointIds(j).id
        val v = vertices(vId)
        val rId = v.tstart + v.tcount
        val ref = refs(rId)
        refs = refs.updated(rId, ref.copy(tid = i, tvertex = j))
        vertices.update(vId, v.copy(tcount = v.tcount + 1))
      }
    }

    if (iteration == 0) {
      triangles.zipWithIndex.foreach { case (t, i) =>
        val points = cellToPoints(t.v)
        val n = calculateNormal(points)
        triangles(i) = t.copy(n = n)
        (0 until 3).foreach { j =>
          val id = t.v.pointIds(j).id
          val v = vertices(id)
          val newQ =
            v.q.add(new SymmetricMatrix(n.x, n.y, n.z, -n.dot(points(0).toVector)))
          vertices.update(id, v.copy(q = newQ))
        }
      }
      triangles.zipWithIndex.foreach { case (t, i) =>
        val allErrors = (0 until 3).map { j =>
          val (err, _) = calculateError(vertices(t.v.pointIds(j).id), vertices(t.v.pointIds((j + 1) % 3).id))
          err
        }
        val err = ErrorEntry(
          Array[Double](
            allErrors(0),
            allErrors(1),
            allErrors(2),
            Math.min(allErrors(0), Math.min(allErrors(1), allErrors(2)))
          )
        )
        triangles(i) = t.copy(err = err)
      }
    }

  }

  private def compactMesh(): Unit = {
    var distance = 0

    vertices.mapInPlace(_.copy(tcount = 0))

    triangles.indices.foreach { i =>
      val t = triangles(i)
      if (!t.deleted) {
        triangles(distance) = t
        distance += 1
        (0 until 3).foreach { j =>
          val id = t.v.pointIds(j).id;
          vertices.update(id, vertices(id).copy(tcount = 1))
        }
      }
    }
    triangles.dropRightInPlace(triangles.length - distance)

    distance = 0
    vertices.zipWithIndex.foreach { case (v, i) =>
      if (v.tcount != 0) {
        vertices.update(i, vertices(i).copy(tstart = distance))
        vertices.update(distance, vertices(distance).copy(p = v.p))
        distance += 1
      }
    }
    triangles.zipWithIndex.foreach { case (t, i) =>
      val t = triangles(i)
      val newIds = (0 until 3).map { j =>
        PointId(vertices(t.v.pointIds(j).id).tstart)
      }
      val newCell = TriangleCell(newIds(0), newIds(1), newIds(2))
      triangles(i) = t.copy(v = newCell)
    }
    vertices.dropRightInPlace(vertices.length - distance)
  }

  def vertexError(q: SymmetricMatrix, x: Double, y: Double, z: Double): Double = {
    q.getValue(0) * x * x
      + 2 * q.getValue(1) * x * y
      + 2 * q.getValue(2) * x * z
      + 2 * q.getValue(3) * x
      + q.getValue(4) * y * y
      + 2 * q.getValue(5) * y * z
      + 2 * q.getValue(6) * y
      + q.getValue(7) * z * z
      + 2 * q.getValue(8) * z
      + q.getValue(9)
  }

  private def calculateDet(m: Array[Double],
                           a11: Int,
                           a12: Int,
                           a13: Int,
                           a21: Int,
                           a22: Int,
                           a23: Int,
                           a31: Int,
                           a32: Int,
                           a33: Int
  ): Double = {
    m(a11) * m(a22) * m(a33) + m(a13) * m(a21) * m(a32) + m(a12) * m(a23) * m(a31)
      - m(a13) * m(a22) * m(a31) - m(a11) * m(a23) * m(a32) - m(a12) * m(a21) * m(a33);
  }

  def calculateError(v1: Vertex, v2: Vertex): (Double, EuclideanVector3D) = {
    // compute interpolated vertex
    val q: SymmetricMatrix = v1.q.add(v2.q)
    val border: Boolean = v1.border & v2.border
    val det: Double = q.det(0, 1, 2, 1, 4, 5, 2, 5, 7)
    val (error, p) = if (det != 0 && !border) {
      val vect = EuclideanVector3D(
        x = (-1 / det * (q.det(1, 2, 3, 4, 5, 6, 5, 7, 8))),
        y = (1 / det * (q.det(0, 2, 3, 1, 5, 6, 2, 7, 8))),
        z = (-1 / det * (q.det(0, 1, 3, 1, 4, 6, 2, 5, 8)))
      )
      val error: Double = vertexError(q, vect.x, vect.y, vect.z)
      (error, vect)
    } else {
      val p1 = v1.p.toVector
      val p2 = v2.p.toVector
      val p3 = (p1.+(p2))./(2.0)
      val error1 = vertexError(q, p1.x, p1.y, p1.z)
      val error2 = vertexError(q, p2.x, p2.y, p2.z)
      val error3 = vertexError(q, p3.x, p3.y, p3.z)
      val error: Double = Math.min(error1, Math.min(error2, error3))
      val vect = if (error3 == error) p3 else if (error2 == error) p2 else p1
      (error, vect)
    }
    (error, p)
  }

  private def createSimplifiedMesh(): TriangleMesh[_3D] = {
    val newPoints: IndexedSeq[Point[_3D]] = vertices.map(v => v.p).toIndexedSeq
    val newTriangles: IndexedSeq[TriangleCell] = triangles.map(t => t.v).toIndexedSeq
    val newTopology: TriangleList = TriangleList(newTriangles)
    TriangleMesh3D(points = newPoints, topology = newTopology)
  }
}
