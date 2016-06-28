/*
 * Copyright 2015 University of Basel, Graphics and Vision Research Group
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package scalismo.mesh

import scalismo.common.PointId

import scala.reflect.ClassTag

/** general property defined on the surface of a mesh */
trait MeshSurfaceProperty[@specialized(Double, Float, Int, Boolean) A] {
  /** access via triangle coordinates */
  def onSurface(triangleId: TriangleId, bcc: BarycentricCoordinates): A

  /** access via triangle coordinates */
  def apply(triangleId: TriangleId, bcc: BarycentricCoordinates): A = onSurface(triangleId, bcc)

  /** map a surface property through a function */
  def map[@specialized(Double, Float, Int, Boolean) B](f: A => B): MeshSurfaceProperty[B] = MappedSurfaceProperty(this, f)

  /** triangulation: domain of surface property */
  def triangulation: TriangleList
}

/** constant property value on complete surface */
case class ConstantProperty[@specialized(Double, Float, Int, Boolean) A](override val triangulation: TriangleList, value: A)
    extends MeshSurfaceProperty[A] {

  def atPoint(pointId: PointId): A = value

  override def onSurface(triangleId: TriangleId, bcc: BarycentricCoordinates): A = value

  override def map[@specialized(Double, Float, Int, Boolean) B](f: A => B): ConstantProperty[B] = ConstantProperty(triangulation, f(value))
}

/** property per vertex, with interpolation */
case class SurfacePointProperty[@specialized(Double, Float, Int, Boolean) A](override val triangulation: TriangleList, pointData: PointId => A)(implicit val interpolator: Interpolator[A])
    extends MeshSurfaceProperty[A] {

  def atPoint(pointId: PointId): A = pointData(pointId)

  def apply(pointId: PointId): A = pointData(pointId)

  override def onSurface(triangleId: TriangleId, bcc: BarycentricCoordinates): A = {
    val t = triangulation.triangles(triangleId.id)
    val v1 = pointData(t.ptId1)
    val v2 = pointData(t.ptId2)
    val v3 = pointData(t.ptId3)
    bcc.interpolateProperty(v1, v2, v3)
  }

  def toIndexedSeq: IndexedSeq[A] = triangulation.pointIds.map(pointData)

  def toArray(implicit tag: ClassTag[A]): Array[A] = {
    val data = new Array[A](triangulation.pointIds.size)
    triangulation.pointIds.foreach { ptId =>
      data(ptId.id) = pointData(ptId)
    }
    data
  }

  def buffer(implicit tag: ClassTag[A]): SurfacePointProperty[A] = {
    val data = toArray
    val t = triangulation
    SurfacePointProperty(t, data)
  }

  /** equality: same value for each triangle (explicit, not handled by case class) */
  override def equals(other: Any) = other match {
    case spp: SurfacePointProperty[A] => triangulation == spp.triangulation && triangulation.pointIds.forall(id => atPoint(id) == spp.atPoint(id))
    case _ => false
  }

  /** hashCode for each triangle value (explicit, not handled by case class) */
  override def hashCode(): Int = triangulation.hashCode() + triangulation.pointIds.foldLeft(0)((hc, id) => 41 * hc + atPoint(id).hashCode())
}

object SurfacePointProperty {
  def apply[A](triangulation: TriangleList, pointData: IndexedSeq[A])(implicit interpolator: Interpolator[A]) = new SurfacePointProperty[A](triangulation, id => pointData(id.id))

  def apply[@specialized(Double, Float, Int, Boolean) A](triangulation: TriangleList, pointData: Array[A])(implicit interpolator: Interpolator[A]) = new SurfacePointProperty[A](triangulation, id => pointData(id.id))

  def averagedPointProperty[A](triangulation: TriangleList, property: MeshSurfaceProperty[A])(implicit ops: Interpolator[A]): SurfacePointProperty[A] = {
    def averager(data: IndexedSeq[A]): A = {
      data.size match {
        case 0 => throw new Exception("averaging over empty set")
        case _ =>
          ops.average(data.head, data.tail: _*)
      }
    }
    sampleSurfaceProperty(triangulation, property, averager)
  }

  def sampleSurfaceProperty[A](triangulation: TriangleList, property: MeshSurfaceProperty[A], reducer: IndexedSeq[A] => A)(implicit blender: Interpolator[A]): SurfacePointProperty[A] = {
    // get all data for a single vertex:
    def getVertex(pointId: PointId): A = {
      val triangles = triangulation.adjacentTrianglesForPoint(pointId)
      val vdata =
        for (t <- triangles) yield {
          val localTriangleIndex = triangulation.triangles(t.id).pointIds.indexOf(pointId)
          property.onSurface(t, BarycentricCoordinates.canonical(localTriangleIndex))
        }
      reducer(vdata) // reduce to a single value
    }
    // do for each vertex
    val data = for (v <- triangulation.pointIds) yield getVertex(v)
    SurfacePointProperty(triangulation, id => data(id.id))
  }
}

/** property constant per triangle */
case class TriangleProperty[@specialized(Double, Float, Int, Boolean) A](override val triangulation: TriangleList, triangleData: TriangleId => A)
    extends MeshSurfaceProperty[A] {

  def apply(triangleId: TriangleId): A = triangleData(triangleId)

  override def onSurface(triangleId: TriangleId, bcc: BarycentricCoordinates): A = triangleData(triangleId)

  def toIndexedSeq: IndexedSeq[A] = triangulation.triangleIds.map(id => triangleData(id))

  def toArray(implicit tag: ClassTag[A]): Array[A] = triangulation.triangleIds.toArray.map(triangleData)

  /** equality: same value for each triangle (explicit, not handled by case class) */
  override def equals(other: Any) = other match {
    case tp: TriangleProperty[A] => triangulation == tp.triangulation && triangulation.triangleIds.forall(id => this(id) == tp(id))
    case _ => false
  }

  /** hashCode for each triangle value (explicit, not handled by case class) */
  override def hashCode(): Int = triangulation.hashCode() + triangulation.triangleIds.foldLeft(0)((hc, id) => 41 * hc + this(id).hashCode())
}

object TriangleProperty {
  def apply[A](triangulation: TriangleList, triangleData: IndexedSeq[A]) = new TriangleProperty[A](triangulation, id => triangleData(id.id))

  def apply[@specialized(Double, Float, Int, Boolean) A](triangulation: TriangleList, triangleData: Array[A]) = new TriangleProperty[A](triangulation, id => triangleData(id.id))

  def fromAveragedPoints[A](triangulation: TriangleList, property: MeshSurfaceProperty[A])(implicit blender: Interpolator[A]): TriangleProperty[A] = {
    val triangleData = for (t <- triangulation.triangleIds) yield {
      val v1 = property(t, BarycentricCoordinates.v0)
      val v2 = property(t, BarycentricCoordinates.v1)
      val v3 = property(t, BarycentricCoordinates.v2)
      blender.convexCombination((v1, 1f), (v2, 1f), (v3, 1f))
    }
    TriangleProperty(triangulation, triangleData)
  }

  def fromTriangleCenters[A](triangulation: TriangleList, property: MeshSurfaceProperty[A]) = {
    val triangleData = for (t <- triangulation.triangleIds) yield property(t, BarycentricCoordinates.center)
    TriangleProperty(triangulation, triangleData)
  }
}

/** function indirection for surface access */
case class MappedSurfaceProperty[@specialized(Double, Float, Int, Boolean) A, @specialized(Double, Float, Int, Boolean) B](values: MeshSurfaceProperty[A], f: A => B)
    extends MeshSurfaceProperty[B] {
  /// access via triangle coordinates
  override def onSurface(triangleId: TriangleId, bcc: BarycentricCoordinates): B = f(values.onSurface(triangleId, bcc))

  override val triangulation: TriangleList = values.triangulation
}

