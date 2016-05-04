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
package scalismo.common

import scalismo.geometry._
import scalismo.registration.{ CanInvert, Transformation }

trait Domain[D <: Dim] {
  self =>
  def isDefinedAt(pt: Point[D]): Boolean

  def warp(t: Transformation[D] with CanInvert[D]): Domain[D] = new Domain[D] {
    val tinv = t.inverse

    override def isDefinedAt(pt: Point[D]): Boolean = {
      self.isDefinedAt(tinv(pt))
    }
  }
}

object Domain {
  def fromPredicate[D <: Dim](chi: Point[D] => Boolean) = new Domain[D] {
    override def isDefinedAt(pt: Point[D]) = chi(pt)
  }

  def intersection[D <: Dim](thisDomain: Domain[D], thatDomain: Domain[D]) = new Domain[D] {
    override def isDefinedAt(pt: Point[D]) = thisDomain.isDefinedAt(pt) && thatDomain.isDefinedAt(pt)
  }

  def union[D <: Dim](thisDomain: Domain[D], thatDomain: Domain[D]) = new Domain[D] {
    override def isDefinedAt(pt: Point[D]) = thisDomain.isDefinedAt(pt) || thatDomain.isDefinedAt(pt)
  }
}

class RealSpace[D <: Dim] extends Domain[D] {
  override def isDefinedAt(pt: Point[D]) = true
}

object RealSpace {
  def apply[D <: Dim] = new RealSpace[D]
}

trait BoxDomain[D <: Dim] extends Domain[D] {

  val origin: Point[D]
  val oppositeCorner: Point[D]

  def isDefinedAt(pt: Point[D]): Boolean = {
    def isInsideAxis(i: Int) = pt(i) >= origin(i) && pt(i) <= oppositeCorner(i)
    (0 until pt.dimensionality).forall(i => isInsideAxis(i))
  }

  val extent: Vector[D] = oppositeCorner - origin
  val volume: Double = (0 until origin.dimensionality).foldLeft(1.0)((prod, i) => prod * (oppositeCorner(i) - origin(i)))

}

object BoxDomain {
  def apply(origin: Point1D, oppositeCorner: Point1D) = BoxDomain1D(origin, oppositeCorner)
  def apply(origin: Point2D, oppositeCorner: Point2D) = BoxDomain2D(origin, oppositeCorner)
  def apply(origin: Point3D, oppositeCorner: Point3D) = BoxDomain3D(origin, oppositeCorner)

  /**
   * Creates a BoxDomain of dimensionality D. Attention, due to the fact that it is a generic
   * constructor, the isDefinedAt method of the resulting object will not be as optimized as when created for a
   * specific dimensionality
   */
  def apply[D <: Dim: NDSpace](orig: Point[D], oppCorner: Point[D]) = new BoxDomain[D] {
    override lazy val oppositeCorner = oppCorner
    override lazy val origin = orig
  }
}

case class BoxDomain1D(origin: Point1D, oppositeCorner: Point1D) extends BoxDomain[_1D] {
  override def isDefinedAt(p: Point[_1D]): Boolean = {
    val pt: Point1D = p
    pt.x >= origin.x && pt.x <= oppositeCorner.x
  }
}

case class BoxDomain2D(origin: Point2D, oppositeCorner: Point2D) extends BoxDomain[_2D] {
  override def isDefinedAt(p: Point[_2D]): Boolean = {
    val pt: Point2D = p
    pt.x >= origin.x && pt.x <= oppositeCorner.x &&
      pt.y >= origin.y && pt.y <= oppositeCorner.y
  }
}

case class BoxDomain3D(origin: Point3D, oppositeCorner: Point3D) extends BoxDomain[_3D] {
  override def isDefinedAt(p: Point[_3D]): Boolean = {
    val pt: Point3D = p
    pt.x >= origin.x && pt.x <= oppositeCorner.x &&
      pt.y >= origin.y && pt.y <= oppositeCorner.y &&
      pt.z >= origin.z && pt.z <= oppositeCorner.z
  }
}
