/*
 * Copyright 2012 Nadav Samet  https://github.com/thesamet/kdtree-scala
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

package scalismo.mesh.kdtree

import scala.language.implicitConversions

import scala.math.Ordering.Implicits._

private[scalismo] sealed trait Region[A] {
  def overlapsWith(other: Region[A])(implicit ord: DimensionalOrdering[A]): Boolean
  def contains(p: A)(implicit ord: DimensionalOrdering[A]): Boolean
}

private[scalismo] case class EntireSpace[A]() extends Region[A] {
  def overlapsWith(other: Region[A])(implicit ord: DimensionalOrdering[A]): Boolean = true
  def contains(p: A)(implicit ord: DimensionalOrdering[A]): Boolean = true
}

private[scalismo] case class AboveHyperplane[A](a: A, dim: Int) extends Region[A] {
  def overlapsWith(other: Region[A])(implicit ord: DimensionalOrdering[A]): Boolean = other match {
    case EntireSpace() => true
    case AboveHyperplane(b, bdim) => true
    case BelowHyperplane(b, bdim) => (dim != bdim) || (ord.compareProjection(dim)(b, a) >= 0)
    case RegionIntersection(regions) => regions.forall(overlapsWith _)
  }
  def contains(p: A)(implicit ord: DimensionalOrdering[A]): Boolean =
    ord.compareProjection(dim)(p, a) >= 0
}

private[scalismo] case class BelowHyperplane[A](a: A, dim: Int) extends Region[A] {
  def overlapsWith(other: Region[A])(implicit ord: DimensionalOrdering[A]): Boolean = other match {
    case EntireSpace() => true
    case AboveHyperplane(b, bdim) => (dim != bdim) || (ord.compareProjection(dim)(b, a) <= 0)
    case BelowHyperplane(b, bdim) => true
    case RegionIntersection(regions) => regions.forall(overlapsWith)
  }
  def contains(p: A)(implicit ord: DimensionalOrdering[A]): Boolean =
    ord.compareProjection(dim)(p, a) <= 0
}

private[scalismo] case class RegionIntersection[A](regions: Seq[Region[A]]) extends Region[A] {
  def overlapsWith(other: Region[A])(implicit ord: DimensionalOrdering[A]): Boolean = {
    regions.forall(_.overlapsWith(other))
  }
  def contains(p: A)(implicit ord: DimensionalOrdering[A]): Boolean =
    regions.forall(_.contains(p))
}

private[scalismo] class RegionBuilder[A] {
  var regions = new scala.collection.mutable.ArrayBuffer[Region[A]]

  def addRegion(region: Region[A]) = {
    regions += region
    this
  }

  def from(a: A, dim: Int): RegionBuilder[A] = addRegion(AboveHyperplane(a, dim))

  def to(a: A, dim: Int): RegionBuilder[A] = addRegion(BelowHyperplane(a, dim))

  def and(other: RegionBuilder[A]): RegionBuilder[A] = {
    regions ++= other.regions
    this
  }

  def build: Region[A] = regions.length match {
    case 0 => EntireSpace()
    case 1 => regions.head
    case _ => RegionIntersection(regions)
  }
}

private[scalismo] object Region {
  implicit def fromBuilder[A](builder: RegionBuilder[A]): Region[A] = builder.build
  def from[A](a: A, dim: Int): RegionBuilder[A] = (new RegionBuilder).from(a, dim)
  def to[A](a: A, dim: Int): RegionBuilder[A] = (new RegionBuilder).to(a, dim)
  def and[A](other: RegionBuilder[A]): RegionBuilder[A] = (new RegionBuilder).and(other)
}
