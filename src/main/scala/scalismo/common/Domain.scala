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

import scalismo.geometry.{ Point, Vector, Dim, _1D, _2D, _3D }
import breeze.linalg.DenseVector
import scalismo.registration.{CanInvert, Transformation}

trait Domain[D <: Dim] { self =>
  def isDefinedAt(pt: Point[D]): Boolean

  def warp(t : Transformation[D] with CanInvert[D]) : Domain[D] = new Domain[D] {
    val tinv = t.inverse
    override def isDefinedAt(pt : Point[D]): Boolean = {
      self.isDefinedAt(tinv(pt))
    }
  }
}

object Domain {
  def fromPredicate[D <: Dim](chi : Point[D] => Boolean) = new Domain[D] {
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
  override def isDefinedAt(pt: Point[D])= true
}
object RealSpace {
  def apply[D <: Dim] = new RealSpace[D]
}


case class BoxDomain[D <: Dim](origin : Point[D], oppositeCorner : Point[D]) extends Domain[D]  {
  def isDefinedAt(pt: Point[D]): Boolean = {
    isInside(pt)
  }

  def extent : Vector[D] = oppositeCorner - origin
  def volume: Double = (0 until origin.dimensionality).foldLeft(1.0)((prod, i) => prod * (oppositeCorner(i) - origin(i)))
  def isInside(pt : Point[D]) : Boolean = {
    def isInsideAxis(i: Int) = pt(i) >= origin(i) && pt(i) <= oppositeCorner(i)
    (0 until pt.dimensionality).foldLeft(true)((defined, i) => defined && isInsideAxis(i))
  }
}

