
package scalismo.mesh

import scalismo.common._
import scalismo.geometry._


import scala.language.implicitConversions

/** Triangle cell in a triangle mesh. The cell relates 3 points with the given identifiers */
case class TriangleCell(ptId1: PointId, ptId2: PointId, ptId3: PointId) extends Cell {
  /** Identifiers of the points belonging to the cell*/
  val pointIds = IndexedSeq(ptId1, ptId2, ptId3)

  /** Returns true if the given point identifier is part of the triangle cell*/
  def containsPoint(ptId: PointId) = ptId1 == ptId || ptId2 == ptId || ptId3 == ptId

  def toIntVector3D = IntVector(ptId1.id, ptId2.id, ptId3.id)
}

trait TriangleMesh[D <: Dim] {

  def triangulation: TriangleList
  def domain: UnstructuredPointsDomain[D]
}

object TriangleMesh {

  def apply[D <: Dim: NDSpace](domain: UnstructuredPointsDomain[D], topology: TriangleList)(implicit creator: Create[D]) = {
    creator.createTriangleMesh(domain, topology)
  }

  def apply[D <: Dim: NDSpace](points: IndexedSeq[Point[D]], topology: TriangleList)(implicit creator: Create[D], domainCreator: CreateUnstructuredPointsDomain[D]) = {
    creator.createTriangleMesh(UnstructuredPointsDomain(points), topology)
  }

  /** Typeclass for creating domains of arbitrary dimensionality */
  trait Create[D <: Dim] extends CreateUnstructuredPointsDomain[D] {
    def createTriangleMesh(domain: UnstructuredPointsDomain[D], topology: TriangleList): TriangleMesh[D]
  }

  trait Create1D extends Create[_1D] {
    override def createTriangleMesh(domain: UnstructuredPointsDomain[_1D], topology: TriangleList) = {
      TriangleMesh1D(domain)
    }
  }

  trait Create2D extends Create[_2D] {
    override def createTriangleMesh(domain: UnstructuredPointsDomain[_2D], topology: TriangleList) = {
      TriangleMesh2D(domain, topology)
    }
  }

  trait Create3D extends Create[_3D] {
    override def createTriangleMesh(domain: UnstructuredPointsDomain[_3D], topology: TriangleList) = {
      TriangleMesh3D(domain, topology)
    }
  }

  implicit def parametricToConcreteType1D(triangleMesh: TriangleMesh[_1D]): TriangleMesh1D = {
    triangleMesh.asInstanceOf[TriangleMesh1D]
  }

  implicit def parametricToConcreteType2D(triangleMesh: TriangleMesh[_2D]): TriangleMesh2D = {
    triangleMesh.asInstanceOf[TriangleMesh2D]
  }

  implicit def parametricToConcreteType3D(triangleMesh: TriangleMesh[_3D]): TriangleMesh3D = {
    triangleMesh.asInstanceOf[TriangleMesh3D]
  }

}

/** Standard 3D Gravis mesh, geometry only */

case class TriangleMesh3D(domain: UnstructuredPointsDomain[_3D], triangulation: TriangleList) extends TriangleMesh[_3D] {

  val position = SurfacePointProperty(triangulation, domain.point _)
  val triangles = triangulation.triangles
  val cells = triangles

  lazy val boundingBox = domain.boundingBox

  /** Get all cell normals as a surface property */
  lazy val cellNormals: TriangleProperty[Vector[_3D]] = {
    val triangleNormals: Array[Vector[_3D]] = new Array[Vector[_3D]](triangles.size)
    triangulation.triangleIds.foreach { tId =>
      val cell = triangulation.triangle(tId)
      triangleNormals(tId.id) = computeCellNormal(cell)
    }
    TriangleProperty(triangulation, triangleNormals)
  }

  /** Get all vertex normals as a surface property, averages over cell normals */
  lazy val vertexNormals: SurfacePointProperty[Vector[_3D]] = {
    // create data array: average over all adjacent triangles
    val pointNormals = new Array[Vector[_3D]](domain.numberOfPoints)
    domain.pointIds.foreach { ptId =>
      val tr = triangulation.adjacentTrianglesForPoint(ptId)
      var x = 0f
      var y = 0f
      var z = 0f
      tr.foreach { tId =>
        val n = cellNormals(tId)
        x += n.x
        y += n.y
        z += n.z
      }
      val n = tr.size
      pointNormals(ptId.id) = Vector3D(x / n, y / n, z / n).normalize
    }
    SurfacePointProperty(triangulation, pointNormals)
  }

  /**
   * Area of the mesh surface.
   *
   *  The computed area is the sum of all the triangle cell areas.
   */
  lazy val area: Double = {
    var sum = 0.0
    triangles.foreach(t => sum += computeTriangleArea(t))
    sum
  }

  /**
   *  Returns a triangle mesh that is the image of this mesh by the given transform.
   *
   *  This method maps all mesh points to their images by the given transform while maintaining the same triangle cell relations.
   *
   *  @param transform A function that maps a given point to a new position. All instances of [[scalismo.registration.Transformation]] being descendants of <code>Function1[Point[_3D], Point[_3D] ]</code> are valid arguments.
   */
  def transform(transform: Point[_3D] => Point[_3D]): TriangleMesh3D = {
    TriangleMesh3D(domain.points.map(transform).toIndexedSeq, triangulation)
  }

  /** Returns a 3D vector that is orthogonal to the triangle defined by the cell points*/
  def computeCellNormal(cell: TriangleCell): Vector[_3D] = {
    val pt1 = domain.point(cell.ptId1)
    val pt2 = domain.point(cell.ptId2)
    val pt3 = domain.point(cell.ptId3)

    val u = pt2 - pt1
    val v = pt3 - pt1
    u.crossproduct(v).normalize
  }

  /**
   *  Returns the area of the indicated triangle cell.
   */
  def computeTriangleArea(t: TriangleCell): Double = {
    // compute are of the triangle using heron's formula
    val A = domain.point(t.ptId1)
    val B = domain.point(t.ptId2)
    val C = domain.point(t.ptId3)
    val a = (B - A).norm
    val b = (C - B).norm
    val c = (C - A).norm
    val s = (a + b + c) / 2
    val areaSquared = s * (s - a) * (s - b) * (s - c)
    // it can happen that the area is negative, due to a degenerate triangle.
    if (areaSquared <= 0.0) 0.0 else math.sqrt(areaSquared)
  }
}

object TriangleMesh3D {
  def apply(points: IndexedSeq[Point[_3D]], topology: TriangleList): TriangleMesh3D = {
    TriangleMesh3D(UnstructuredPointsDomain(points), topology)
  }
}

case class TriangleMesh2D(domain: UnstructuredPointsDomain[_2D], triangulation: TriangleList) extends TriangleMesh[_2D] {
  val position = SurfacePointProperty(triangulation, domain.point _)

}

case class TriangleMesh1D(domain: UnstructuredPointsDomain[_1D]) extends TriangleMesh[_1D] {
  override val triangulation = TriangleList(IndexedSeq[TriangleCell]())
}
