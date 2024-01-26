package scalismo.mesh.decimate

import scalismo.common.PointId
import scalismo.geometry.{EuclideanVector, EuclideanVector3D, Point, SquareMatrix, _3D}
import scalismo.mesh.{TriangleCell, TriangleList, TriangleMesh, TriangleMesh3D}

import scala.collection.mutable.ArrayBuffer
// Based on https://github.com/sp4cerat/Fast-Quadric-Mesh-Simplification
// and its java implementation: https://gist.github.com/jayfella/00a328b2dbdf6304078a821143b7aef7


object MeshDecimation {

  val MAX_ITERATIONS = 100;

  private class Buffers(mesh: TriangleMesh[_3D]) {
    val triangles = ArrayBuffer.from(
      mesh.triangulation.triangles.map { t => Triangle(cell = t) }
    )

    val vertices = ArrayBuffer.from(
      mesh.pointSet.pointIds.map(pid =>
        Vertex(
          point = mesh.pointSet.point(pid),
          border = mesh.operations.pointIsOnBoundary(pid)
        )
      )
    )

    val refs = ArrayBuffer.from(
      mesh.triangulation.triangleIds.flatMap { tid =>
        mesh.triangulation.triangle(tid).pointIds.map { ptId =>
          Ref(tid = tid.id, tvertex = ptId.id)
        }
      }
    )
  }

  def simplify(mesh: TriangleMesh[_3D], targetCount: Int, aggressiveness: Double): TriangleMesh[_3D] = {
    val numberOfTriangles: Int = mesh.triangulation.triangles.length
    val data = Buffers(mesh)

    var deletedTriangles = 0

    val deleted0: Vector[Boolean] = Vector.empty[Boolean]
    val deleted1: Vector[Boolean] = Vector.empty[Boolean]

    println(s"Target triangle count: ${targetCount}, from ${numberOfTriangles}.")
    var iteration = 0
    while (iteration < MAX_ITERATIONS) {
      println(
        f"Iteration ${iteration} -> triangles [ deleted: ${deletedTriangles} : count: ${numberOfTriangles - deletedTriangles} | removed: ${(deletedTriangles * 100 / numberOfTriangles)}]"
      )

      if (numberOfTriangles - deletedTriangles <= targetCount) {
        iteration = MAX_ITERATIONS // REMOVE? why not return or in while condition? same condition towards the end of the loop body
      }

      if (iteration % 5 == 0) {
        updateMesh(data,iteration)
      }

      val threshold: Double = 1.0E-9 * Math.pow((iteration + 3).toDouble, aggressiveness)

      data.triangles.foreach(_.dirty = false)

      data.triangles.indices.foreach {
        i => // DO NOT USE FOREACH as the triangles are updated within the loop - yes, I know, very ugly.
          val t = data.triangles(i)

          if (isTriangleRemovable(threshold, t)) {
            var j = 0
            while(j<3) {
              if (t.err.vertexError(j) < threshold) {
                val pid1 = t.cell.pointIds(j).id
                val pid2 = t.cell.pointIds((j + 1) % 3).id

                val pt1 = data.vertices(pid1)
                val pt2 = data.vertices(pid2)

                if (pt1.border == pt2.border) {
                  val (_, p) = calculateError(pt1, pt2)

                  val (flipped0, deleted0) = flipped(data,p, pid2, pt1)
                  val (flipped1, deleted1) = flipped(data,p, pid1, pt2)

                  if (!flipped0 && !flipped1) {
                    data.vertices(pid1).point = p.toPoint
                    data.vertices(pid1).q = pt1.q.add(pt2.q)
                    val tstart = data.refs.length

                    deletedTriangles += updateTriangles(data,pid1, pt1, deleted0)
                    deletedTriangles += updateTriangles(data,pid1, pt2, deleted1)

                    val tcount = data.refs.length - tstart
                    data.vertices(pid1).tstart = tstart
                    data.vertices(pid1).tcount = tcount
                    j = 3
                  }
                }
              }
              j += 1
            }
            if (numberOfTriangles - deletedTriangles <= targetCount) {
              iteration = MAX_ITERATIONS
            }
          }
      }
      iteration += 1
    }

    compactMesh(data.vertices, data.triangles)
  }

  private def isTriangleRemovable(threshold: Double, t: Triangle) = {
    t.err.minVertexError <= threshold && !t.deleted && !t.dirty
  }

  private def flipped(data: Buffers, p: EuclideanVector3D, i1: Int, v0: Vertex): (Boolean, Array[Boolean]) = {
    val deleted = Array.fill(v0.tcount)(false)
    var returnValue = false
    var k = 0
    val counterMaxValue = v0.tcount
    while (k < counterMaxValue) {
      val ref = data.refs(v0.tstart + k)
      val t = data.triangles(ref.tid)

      if (!t.deleted) {
        val s = ref.tvertex
        val id1 = t.cell.pointIds((s + 1) % 3).id
        val id2 = t.cell.pointIds((s + 2) % 3).id
        if (id1 == i1 || id2 == i1) {
          deleted(k) = true
        } else {
          val d1 = (data.vertices(id1).point.-(p)).toVector.normalize
          val d2 = (data.vertices(id2).point.-(p)).toVector.normalize

          if (Math.abs(d1.dot(d2)) > 0.999) {
            returnValue = true
            k = counterMaxValue
          } else {
            val n = d1.crossproduct(d2).normalize
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

  private def updateTriangles(data: Buffers, i0: Int, v: Vertex, deleted: Array[Boolean]): Int = {
    var triangleDeleted = 0

    (0 until v.tcount).foreach { k =>
      val r = data.refs(v.tstart + k)
      val t = data.triangles(r.tid)

      if (!t.deleted) {
        if (deleted(k)) {
          data.triangles(r.tid) = t.copy(deleted = true)
          triangleDeleted += 1
        } else {
          val err0 = calculateError(data.vertices(t.cell.ptId1.id), data.vertices(t.cell.ptId2.id))._1
          val err1 = calculateError(data.vertices(t.cell.ptId2.id), data.vertices(t.cell.ptId3.id))._1
          val err2 = calculateError(data.vertices(t.cell.ptId3.id), data.vertices(t.cell.ptId1.id))._1
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
            if (r.tvertex == 0) t.cell.copy(ptId1 = PointId(i0))
            else if (r.tvertex == 1) t.cell.copy(ptId2 = PointId(i0))
            else t.cell.copy(ptId3 = PointId(i0))
          data.triangles(r.tid) = t.copy(
            cell = vNew,
            dirty = true,
            err = err
          )
          data.refs += r
        }
      }
    }
    triangleDeleted
  }

  private def calculateNormal(p: Seq[Point[_3D]]): EuclideanVector3D = {
    (p(1) - p(0)).crossproduct(p(2) - p(0)).normalize
  }

  private def cellToPoints(data: Buffers, cell: TriangleCell): Seq[Point[_3D]] = {
    cell.pointIds.map{ id => data.vertices(id.id).point }
  }

  private def updateMesh(data: Buffers, iteration: Int): Unit = {
    if (iteration > 0) {
      data.triangles.filterInPlace(!_.deleted)
    }

    data.vertices.map{ v =>
      v.tcount = 0
    }

    data.triangles.foreach { t =>
      t.cell.pointIds.foreach { pid =>
        data.vertices(pid.id).tcount += 1
      }
    }

    var tstart = 0;
    data.vertices.foreach { v =>
      v.tstart = tstart
      tstart += v.tcount
      v.tcount = 0
    }

    data.refs.dropRightInPlace(data.refs.length - data.triangles.length * 3)

    data.triangles.zipWithIndex.foreach { case (t, i) =>
      t.cell.pointIds.zipWithIndex.foreach { case (pid, j) =>
        val v = data.vertices(pid.id)
        val rId = v.tstart + v.tcount
        val r = data.refs(rId)
        r.tid = i
        r.tvertex = j
        v.tcount = v.tcount + 1
      }
    }

    if (iteration == 0) {
      onFirstIteration(data)
    }
  }

  private def onFirstIteration(data: Buffers): Unit = {
    // calculate all face normals and the q value for each vertex.
    data.triangles.foreach { t =>
      val points = cellToPoints(data,t.cell)
      val n = calculateNormal(points)
      t.n = n
      t.cell.pointIds.foreach { id =>
        val v = data.vertices(id.id)
        v.q = v.q.add(
          SymmetricMatrix(n.x, n.y, n.z, -n.dot(points(0).toVector))
        )
      }
    }

    // calculate the error for each vertex, which uses the former q values
    data.triangles.foreach { t =>
      (0 until 3).foreach { j =>
        val (e, _) = calculateError(data.vertices(t.cell.pointIds(j).id), data.vertices(t.cell.pointIds((j + 1) % 3).id))
        t.err.vertexError(j) = e
      }
      t.err.minVertexError = t.err.vertexError.min
    }
  }

  private def compactMesh(vertices: ArrayBuffer[Vertex], triangles: ArrayBuffer[Triangle]): TriangleMesh[_3D] = {

    // Initialize use counter to zero
    vertices.foreach(_.tcount = 0)

    // Remove deleted triangles and mark vertices as used
    triangles.filterInPlace { t =>
      val keep = !t.deleted
      if (keep) {
        t.cell.pointIds.foreach { id =>
          vertices(id.id).tcount += 1
        }
      }
      keep
    }

    // Copy used points to the beginning of the list.
    // Keep track where they moved, to update triangle cells later on.
    var nextFreePointLocation = 0
    vertices.foreach { v =>
      if (v.tcount != 0) {
        vertices(nextFreePointLocation).point = v.point
        v.tstart = nextFreePointLocation
        nextFreePointLocation += 1
      }
    }

    // Update triangle cells with new vertex indices
    triangles.foreach { t =>
      val newIds = t.cell.pointIds.map { pid =>
        PointId(vertices(pid.id).tstart)
      }
      t.cell = TriangleCell(newIds(0), newIds(1), newIds(2))
    }

    // Remove no longer needed points.
    vertices.dropRightInPlace(vertices.length - nextFreePointLocation)

    // Create scalismo mesh form the data.
    val newPoints: IndexedSeq[Point[_3D]] = vertices.map(_.point).toIndexedSeq
    val newTriangles: IndexedSeq[TriangleCell] = triangles.map(_.cell).toIndexedSeq
    val newTopology: TriangleList = TriangleList(newTriangles)
    TriangleMesh3D(points = newPoints, topology = newTopology)
  }

  def vertexError(q: SymmetricMatrix, x: Double, y: Double, z: Double): Double = {
    q.m(0) * x * x
      + 2 * q.m(1) * x * y
      + 2 * q.m(2) * x * z
      + 2 * q.m(3) * x
      + q.m(4) * y * y
      + 2 * q.m(5) * y * z
      + 2 * q.m(6) * y
      + q.m(7) * z * z
      + 2 * q.m(8) * z
      + q.m(9)
  }

  def calculateError(v1: Vertex, v2: Vertex): (Double, EuclideanVector[_3D]) = {
    // compute interpolated vertex
    val q: SymmetricMatrix = v1.q.add(v2.q)
    val border: Boolean = v1.border && v2.border

    if (!border) {
      val det: Double = SymmetricMatrix.det(q.m(0), q.m(1), q.m(2), q.m(1), q.m(4), q.m(5), q.m(2), q.m(5), q.m(7))
      if (det != 0) {
        optimizePosition(q, det)
      } else {
        selectVertexOrMidpoint(v1, v2, q)
      }
    } else {
      selectVertexOrMidpoint(v1, v2, q)
    }
  }
  private def selectVertexOrMidpoint(v1: Vertex, v2: Vertex, q: SymmetricMatrix) = {
    val p1 = v1.point.toVector
    val p2 = v2.point.toVector
    val p3 = (p1 + p2) / 2.0
    Seq(p1, p2, p3).map {
      p => (vertexError(q, p.x, p.y, p.z), p)
    }.minBy(_._1)
  }

  private def optimizePosition(q: SymmetricMatrix, det: Double) = {
    val dInv = 1 / det
    val vec = EuclideanVector3D(
      x = (-dInv * (SymmetricMatrix.det(q.m(1), q.m(2), q.m(3), q.m(4), q.m(5), q.m(6), q.m(5), q.m(7), q.m(8)))),
      y = (dInv * (SymmetricMatrix.det(q.m(0), q.m(2), q.m(3), q.m(1), q.m(5), q.m(6), q.m(2), q.m(7), q.m(8)))),
      z = (-dInv * (SymmetricMatrix.det(q.m(0), q.m(1), q.m(3), q.m(1), q.m(4), q.m(6), q.m(2), q.m(5), q.m(8))))
    )
    val error: Double = vertexError(q, vec.x, vec.y, vec.z)
    (error, vec)
  }
}
