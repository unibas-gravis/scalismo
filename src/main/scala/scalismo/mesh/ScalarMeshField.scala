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

import scalismo.common._
import scalismo.geometry._3D

import scala.reflect.ClassTag

/**
 * 3-dimensional triangle mesh with scalar values associated to mesh points.
 *
 * @tparam S type of the scalar values defined over the mesh (Short, Int, Float, Double)
 * @constructor Returns a scalar mesh data given a triangle mesh and an array of values.
 * The number of values and mesh points must be equal.
 */
case class ScalarMeshField[S: Scalar: ClassTag](mesh: TriangleMesh[_3D], override val data: ScalarArray[S])
    extends DiscreteScalarField[_3D, UnstructuredPointsDomain[_3D], S](mesh.pointSet, data) {
  require(mesh.pointSet.numberOfPoints == data.size)

  override def values = data.iterator
  override val domain = mesh.pointSet

  override def apply(ptId: PointId) = data(ptId.id)
  override def isDefinedAt(ptId: PointId) = data.isDefinedAt(ptId.id)

  override def map[S2: Scalar: ClassTag](f: S => S2): ScalarMeshField[S2] = {
    ScalarMeshField(mesh, data.map(f))
  }
}

object ScalarMeshField {
  def apply[S: Scalar: ClassTag](mesh: TriangleMesh[_3D], data: Traversable[S]): ScalarMeshField[S] = {
    ScalarMeshField(mesh, ScalarArray(data.toArray))
  }
}