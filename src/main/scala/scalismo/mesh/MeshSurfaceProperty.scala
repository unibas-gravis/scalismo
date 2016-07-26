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
trait MeshSurfaceProperty[A] {
  /** access via triangle coordinates */
  def onSurface(triangleId: TriangleId, bcc: BarycentricCoordinates): A

  /** access via triangle coordinates */
  def apply(triangleId: TriangleId, bcc: BarycentricCoordinates): A = onSurface(triangleId, bcc)

  /** map a surface property through a function */
  def map[B](f: A => B): MeshSurfaceProperty[B] = MappedSurfaceProperty(this, f)

  /** triangulation: domain of surface property */
  def triangulation: TriangleList
}

/** constant property value on complete surface */
case class ConstantProperty[A](triangulation: TriangleList, value: A)
    extends MeshSurfaceProperty[A] {

  def atPoint(pointId: PointId): A = value

  override def onSurface(triangleId: TriangleId, bcc: BarycentricCoordinates): A = value

  override def map[B](f: A => B): ConstantProperty[B] = ConstantProperty(triangulation, f(value))
}

/** property per vertex, with interpolation */
case class SurfacePointProperty[A](triangulation: TriangleList, pointData: IndexedSeq[A])(implicit val interpolator: Interpolator[A])
    extends MeshSurfaceProperty[A] {

  def atPoint(pointId: PointId): A = pointData(pointId.id)

  def apply(pointId: PointId): A = pointData(pointId.id)

  override def onSurface(triangleId: TriangleId, bcc: BarycentricCoordinates): A = {
    val t = triangulation.triangles(triangleId.id)
    val v1 = pointData(t.ptId1.id)
    val v2 = pointData(t.ptId2.id)
    val v3 = pointData(t.ptId3.id)
    bcc.interpolateProperty(v1, v2, v3)
  }

  def mapPoints[B](f: A => B)(implicit interpolator: Interpolator[B]) = SurfacePointProperty(triangulation, pointData.map(f))
}

object SurfacePointProperty {
  def averagedPointProperty[A](property: MeshSurfaceProperty[A])(implicit ops: Interpolator[A]): SurfacePointProperty[A] = {
    def averager(data: IndexedSeq[A]): A = {
      data.size match {
        case 0 => throw new Exception("averaging over empty set")
        case _ =>
          ops.average(data.head, data.tail: _*)
      }
    }
    sampleSurfaceProperty(property, averager)
  }

  def sampleSurfaceProperty[A](property: MeshSurfaceProperty[A], reducer: IndexedSeq[A] => A)(implicit blender: Interpolator[A]): SurfacePointProperty[A] = {
    val triangulation = property.triangulation
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
    SurfacePointProperty(triangulation, data)
  }
}

/** property constant per triangle */
case class TriangleProperty[A](triangulation: TriangleList, triangleData: IndexedSeq[A])
    extends MeshSurfaceProperty[A] {

  def apply(triangleId: TriangleId): A = onTriangle(triangleId)

  override def onSurface(triangleId: TriangleId, bcc: BarycentricCoordinates): A = triangleData(triangleId.id)

  def onTriangle(triangleId: TriangleId): A = triangleData(triangleId.id)

  override def map[B](f: A => B): TriangleProperty[B] = TriangleProperty(triangulation, triangleData.map(f))
}

object TriangleProperty {
  def fromAveragedPoints[A](property: MeshSurfaceProperty[A])(implicit blender: Interpolator[A]): TriangleProperty[A] = {
    val triangleData = for (t <- property.triangulation.triangleIds) yield {
      val v1 = property(t, BarycentricCoordinates.v0)
      val v2 = property(t, BarycentricCoordinates.v1)
      val v3 = property(t, BarycentricCoordinates.v2)
      blender.convexCombination((v1, 1f), (v2, 1f), (v3, 1f))
    }
    TriangleProperty(property.triangulation, triangleData)
  }

  def fromTriangleCenters[A](triangulation: TriangleList, property: MeshSurfaceProperty[A]) = {
    val triangleData = for (t <- triangulation.triangleIds) yield property(t, BarycentricCoordinates.center)
    TriangleProperty(triangulation, triangleData)
  }
}

/** function indirection for surface access */
case class MappedSurfaceProperty[A, B](values: MeshSurfaceProperty[A], f: A => B)
    extends MeshSurfaceProperty[B] {
  /// access via triangle coordinates
  override def onSurface(triangleId: TriangleId, bcc: BarycentricCoordinates): B = f(values.onSurface(triangleId, bcc))

  override def triangulation: TriangleList = values.triangulation

  override def map[C](g: B => C): MappedSurfaceProperty[A, C] = MappedSurfaceProperty(values, g compose f)
}

