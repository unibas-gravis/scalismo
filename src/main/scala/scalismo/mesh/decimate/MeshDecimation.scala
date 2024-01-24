package scalismo.mesh.decimate

import scalismo.common.PointId
import scalismo.geometry.{_3D, EuclideanVector3D, Point, SquareMatrix}
import scalismo.mesh.{TriangleCell, TriangleList, TriangleMesh, TriangleMesh3D}

import scala.collection.mutable.ArrayBuffer
// Based on https://github.com/sp4cerat/Fast-Quadric-Mesh-Simplification
// and its java implementation: https://gist.github.com/jayfella/00a328b2dbdf6304078a821143b7aef7


class MeshDecimation(mesh: TriangleMesh[_3D]) {
  val numberOfPoints: Int = mesh.pointSet.numberOfPoints
  val numberOfTriangles: Int = mesh.triangulation.triangles.length

  var triangles: ArrayBuffer[Triangle] = ArrayBuffer.empty
  var vertices: ArrayBuffer[Vertex] = ArrayBuffer.empty
  var refs: ArrayBuffer[Ref] = ArrayBuffer.empty

  private def initializeArrayBuffers(): Unit = {
    mesh.triangulation.triangleIds.foreach(tid =>
      triangles += Triangle(
        cell = mesh.triangulation.triangle(tid)
      )
    )

    mesh.pointSet.pointIds.toIndexedSeq.foreach(pid =>
      vertices += Vertex(
        point = mesh.pointSet.point(pid),
        border = mesh.operations.pointIsOnBoundary(pid)
      )
    )

    mesh.triangulation.triangleIds.foreach { tid =>
      mesh.triangulation.triangle(tid).pointIds.foreach { ptId =>
        refs += Ref(tid = tid.id, tvertex = ptId.id)
      }
    }
  }

  def simplify(targetCount: Int, aggressiveness: Double): TriangleMesh[_3D] = {
    var deletedTriangles = 0

    val deleted0: Vector[Boolean] = Vector.empty[Boolean]
    val deleted1: Vector[Boolean] = Vector.empty[Boolean]

    val maxIterations = 100;

    val triangleCount = numberOfTriangles

    initializeArrayBuffers()

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
                val i0 = t.cell.pointIds(j).id
                val i1 = t.cell.pointIds((j + 1) % 3).id

                val v0 = vertices(i0)
                val v1 = vertices(i1)

                if (v0.border == v1.border) {
                  val (_, p) = calculateError(v0, v1)

                  val (flipped0, deleted0) = flipped(p, i1, v0)
                  val (flipped1, deleted1) = flipped(p, i0, v1)

                  if (!flipped0 && !flipped1) {
                    vertices(i0) = vertices(i0).copy(point = p.toPoint, q = v0.q.add(v1.q))
                    val tstart = refs.length

                    deletedTriangles += updateTriangles(i0, v0, deleted0)
                    deletedTriangles += updateTriangles(i0, v1, deleted1)

                    val tcount = refs.length - tstart
                    vertices(i0) = vertices(i0).copy(tstart = tstart, tcount = tcount)
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

    compactMesh(vertices, triangles)
  }

  private def flipped(p: EuclideanVector3D, i1: Int, v0: Vertex): (Boolean, Array[Boolean]) = {
    val deleted = Array.fill(v0.tcount)(false)
    var returnValue = false
    var k = 0
    val counterMaxValue = v0.tcount
    while (k < counterMaxValue) {
      val ref = refs(v0.tstart + k)
      val t = triangles(ref.tid)

      if (!t.deleted) {
        val s = ref.tvertex
        val id1 = t.cell.pointIds((s + 1) % 3).id
        val id2 = t.cell.pointIds((s + 2) % 3).id
        if (id1 == i1 || id2 == i1) {
          deleted(k) = true
        } else {
          val d1 = (vertices(id1).point.-(p)).toVector.normalize
          val d2 = (vertices(id2).point.-(p)).toVector.normalize

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
          val err0 = calculateError(vertices(t.cell.ptId1.id), vertices(t.cell.ptId2.id))._1
          val err1 = calculateError(vertices(t.cell.ptId2.id), vertices(t.cell.ptId3.id))._1
          val err2 = calculateError(vertices(t.cell.ptId3.id), vertices(t.cell.ptId1.id))._1
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
          triangles(r.tid) = t.copy(
            cell = vNew,
            dirty = true,
            err = err
          )
          refs += r
        }
      }
    }
    triangleDeleted
  }

  private def calculateNormal(p: Seq[Point[_3D]]): EuclideanVector3D = {
    (p(1) - p(0)).crossproduct(p(2) - p(0)).normalize
  }

  private def cellToPoints(cell: TriangleCell): Seq[Point[_3D]] = {
    cell.pointIds.map{ id => vertices(id.id).point }
  }

  private def updateMesh(iteration: Int): Unit = {
    if (iteration > 0) {
      triangles.filterInPlace(!_.deleted)
    }

    vertices.map{ v =>
      v.tcount = 0
    }

    triangles.foreach { t =>
      t.cell.pointIds.foreach { pid =>
        vertices(pid.id).tcount += 1
      }
    }

    var tstart = 0;
    vertices.foreach { v =>
      v.tstart = tstart
      tstart += v.tcount
      v.tcount = 0
    }

    refs.dropRightInPlace(refs.length - triangles.length * 3)

    triangles.zipWithIndex.foreach { case (t, i) =>
      t.cell.pointIds.zipWithIndex.foreach { case (pid, j) =>
        val v = vertices(pid.id)
        val rId = v.tstart + v.tcount
        val r = refs(rId)
        r.tid = i
        r.tvertex = j
        v.tcount = v.tcount + 1
      }
    }

    if (iteration == 0) {
      onFirstIteration()
    }
  }

  private def onFirstIteration(): Unit = {
    // calculate all face normals and the q value for each vertex.
    triangles.foreach { t =>
      val points = cellToPoints(t.cell)
      val n = calculateNormal(points)
      t.n = n
      t.cell.pointIds.foreach { id =>
        val v = vertices(id.id)
        v.q = v.q.add(
          SymmetricMatrix(n.x, n.y, n.z, -n.dot(points(0).toVector))
        )
      }
    }

    // calculate the error for each vertex, which uses the former q values
    triangles.foreach { t =>
      (0 until 3).foreach { j =>
        val (e, _) = calculateError(vertices(t.cell.pointIds(j).id), vertices(t.cell.pointIds((j + 1) % 3).id))
        t.err.err(j) = e
      }
      t.err.err(3) = Math.min(t.err.err(0), Math.min(t.err.err(1),t.err.err(2)))
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

  def calculateError(v1: Vertex, v2: Vertex): (Double, EuclideanVector3D) = {
    // compute interpolated vertex
    val q: SymmetricMatrix = v1.q.add(v2.q)
    val border: Boolean = v1.border & v2.border
    val det: Double = SymmetricMatrix.det(q.m(0), q.m(1), q.m(2), q.m(1), q.m(4), q.m(5), q.m(2), q.m(5), q.m(7))
    val (error, p) = if (det != 0 && !border) {
      val dInv = 1 / det
      val vec = EuclideanVector3D(
        x = (-dInv * (SymmetricMatrix.det(q.m(1), q.m(2), q.m(3), q.m(4), q.m(5), q.m(6), q.m(5), q.m(7), q.m(8)))),
        y = (dInv * (SymmetricMatrix.det(q.m(0), q.m(2), q.m(3), q.m(1), q.m(5), q.m(6), q.m(2), q.m(7), q.m(8)))),
        z = (-dInv * (SymmetricMatrix.det(q.m(0), q.m(1), q.m(3), q.m(1), q.m(4), q.m(6), q.m(2), q.m(5), q.m(8))))
      )
      val error: Double = vertexError(q, vec.x, vec.y, vec.z)
      (error, vec)
    } else {
      val p1 = v1.point.toVector
      val p2 = v2.point.toVector
      val p3 = (p1+p2)/2.0
      Seq(p1,p2,p3).map{
        p => (vertexError(q,p.x,p.y,p.z),p)
      }.minBy(_._1)
    }
    (error, p)
  }
}
