/*
*
* This file is shamelessly copied from 
* https://github.com/thesamet/kdtree-scala
*/

package org.statismo.stk.core.mesh.kdtree
import scala.language.implicitConversions

import scala.math.Ordering.Implicits._

sealed trait Region[A] {
  def overlapsWith(other: Region[A])(implicit ord: DimensionalOrdering[A]): Boolean
  def contains(p: A)(implicit ord: DimensionalOrdering[A]): Boolean
}

case class EntireSpace[A]() extends Region[A] {
  def overlapsWith(other: Region[A])(implicit ord: DimensionalOrdering[A]): Boolean = true
  def contains(p: A)(implicit ord: DimensionalOrdering[A]): Boolean = true
}

case class AboveHyperplane[A](a: A, dim: Int) extends Region[A] {
  def overlapsWith(other: Region[A])(implicit ord: DimensionalOrdering[A]): Boolean = other match {
    case EntireSpace() => true
    case AboveHyperplane(b, bdim) => true
    case BelowHyperplane(b, bdim) => (dim != bdim) || (ord.compareProjection(dim)(b, a) >= 0)
    case RegionIntersection(regions) => regions.forall(overlapsWith _)
  }
  def contains(p: A)(implicit ord: DimensionalOrdering[A]): Boolean =
      ord.compareProjection(dim)(p, a) >= 0
}

case class BelowHyperplane[A](a: A, dim: Int) extends Region[A] {
  def overlapsWith(other: Region[A])(implicit ord: DimensionalOrdering[A]): Boolean = other match {
    case EntireSpace() => true
    case AboveHyperplane(b, bdim) => (dim != bdim) || (ord.compareProjection(dim)(b, a) <= 0)
    case BelowHyperplane(b, bdim) => true
    case RegionIntersection(regions) => regions.forall(overlapsWith)
  }
  def contains(p: A)(implicit ord: DimensionalOrdering[A]): Boolean =
      ord.compareProjection(dim)(p, a) <= 0
}

case class RegionIntersection[A](regions: Seq[Region[A]]) extends Region[A] {
  def overlapsWith(other: Region[A])(implicit ord: DimensionalOrdering[A]): Boolean = {
    regions.forall(_.overlapsWith(other))
  }
  def contains(p: A)(implicit ord: DimensionalOrdering[A]): Boolean =
    regions.forall(_.contains(p))
}

class RegionBuilder[A] {
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

object Region {
  implicit def fromBuilder[A](builder: RegionBuilder[A]): Region[A] = builder.build
  def from[A](a: A, dim: Int): RegionBuilder[A] = (new RegionBuilder).from(a, dim)
  def to[A](a: A, dim: Int): RegionBuilder[A] = (new RegionBuilder).to(a, dim)
  def and[A](other: RegionBuilder[A]): RegionBuilder[A] = (new RegionBuilder).and(other)
}
