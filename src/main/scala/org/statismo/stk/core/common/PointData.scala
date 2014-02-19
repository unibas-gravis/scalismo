package org.statismo.stk.core.common

import org.statismo.stk.core.geometry.Dim
import scala.reflect.ClassTag


trait PointData[D <: Dim, @specialized(Float, Short) Pixel] extends PartialFunction[Int, Pixel] {
  def domain: DiscreteDomain[D]
  def values: Array[Pixel]
  def valueDimensionality: Int
  def apply(idx: Int): Pixel = values(idx)
  def isDefinedAt(idx: Int) = idx >= 0 && idx <= values.size

  def foreach[A](f: Pixel => A): Unit = values.foreach(f)
  def map[Pixel2: ScalarValue: ClassTag](f: Pixel => Pixel2): PointData[D, Pixel2]

  override def hashCode = values.deep.hashCode
  override def equals(other: Any): Boolean = other match {
    case that: PointData[D, Pixel] => {
      that.canEqual(this) && this.values.deep == that.values.deep && this.domain == that.domain
    }
    case _ => false
  }

  def canEqual(other: Any): Boolean = other.isInstanceOf[PointData[D, Pixel]]
}

