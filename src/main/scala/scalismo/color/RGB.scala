/*
 * Copyright 2016 University of Basel, Graphics and Vision Research Group
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

package scalismo.color

import java.awt.Color

import breeze.linalg.DenseVector
import scalismo.common.{ComponentRepresentation, Vectorizer}
import scalismo.geometry.EuclideanVector._
import scalismo.geometry.{_3D, EuclideanVector}
import scalismo.numerics.ValueInterpolator

import scala.annotation.switch

case class RGB(r: Double, g: Double, b: Double) {
  def isInBounds: Boolean = r >= 0.0 && r <= 1.0 && g >= 0.0 && g <= 1.0 && b >= 0.0 && b <= 1.0

  def sum: Double = r + g + b

  /** clamp all values to valid range [0, 1] */
  def clamped: RGB = {
    def clamp(f: Double): Double = math.min(1.0, math.max(0.0, f))
    if (!isInBounds)
      RGB(clamp(r), clamp(g), clamp(b))
    else
      this
  }

  /** average intensity value */
  def gray: Double = sum / 3.0

  /** l2 norm of color rgb values */
  def norm: Double = math.sqrt(math.pow(r, 2) + math.pow(g, 2) + math.pow(b, 2))

  def luminance: Double = 0.3 * r + 0.59 * g + 0.11 * b

  /** addition of another RGB */
  def +(other: RGB): RGB = new RGB(r + other.r, g + other.g, b + other.b)

  /** subtraction of another RGB */
  def -(other: RGB): RGB = new RGB(r - other.r, g - other.g, b - other.b)

  /** scaling with scalar number */
  def *(f: Double): RGB = new RGB(r * f, g * f, b * f)

  /** scaling with scalar number */
  def *:(f: Double): RGB = new RGB(r * f, g * f, b * f)

  /** scaling with scalar number */
  def /(f: Double): RGB = this * (1.0f / f)

  /** dot product */
  def dot(other: RGB): Double = r * other.r + g * other.g + b * other.b

  /** component-wise multiplication */
  def x(other: RGB): RGB = RGB(r * other.r, g * other.g, b * other.b)

  /** component-wise division */
  def /(other: RGB): RGB = RGB(r / other.r, g / other.g, b / other.b)

  /** applies f to all channels */
  def map(f: Double => Double): RGB = new RGB(f(r), f(g), f(b))

  /** blend with RGBA: RGBA pixel overlay, this color as basis */
  def blend(color: RGBA): RGB = {
    val a = color.a
    val na = 1.0f - color.a
    new RGB(na * r + a * color.r, na * g + a * color.g, na * b + a * color.b)
  }

  /** convert to RGBA with full opacity */
  def toRGBA: RGBA = RGBA(r, g, b, 1.0)

  /** convert to Tuple */
  def toTuple: (Double, Double, Double) = (r, g, b)

  /** convert to standard Vector[_3D] */
  def toVector: EuclideanVector[_3D] = EuclideanVector(r, g, b)

  /**
   * convert to AWT default color
   * expects a clamped color value
   */
  def toAWTColor: Color = new java.awt.Color(r.toFloat, g.toFloat, b.toFloat)
}

object RGB {

  val White: RGB = RGB(1.0, 1.0, 1.0)
  val Black: RGB = RGB(0.0, 0.0, 0.0)

  def apply(color: RGBA): RGB = new RGB(color.r, color.g, color.b)
  def apply(gray: Double): RGB = new RGB(gray, gray, gray)
  def apply(tuple: (Double, Double, Double)) = new RGB(tuple._1, tuple._2, tuple._3)
  def apply(vector3D: EuclideanVector[_3D]) = new RGB(vector3D.x, vector3D.y, vector3D.z)
  def apply(awtColor: Color) =
    new RGB(fromInt8(awtColor.getRed), fromInt8(awtColor.getGreen), fromInt8(awtColor.getBlue))

  implicit object RGBComponents extends ComponentRepresentation[RGB] with Vectorizer[RGB] {

    override def fromArray(arr: Array[Double]): RGB = {
      require(arr.length == size)
      RGB(arr(0), arr(1), arr(2))
    }

    override def toArray(color: RGB): Array[Double] = Array(color.r, color.g, color.b)

    override def intoArray(color: RGB, array: Array[Double]): Array[Double] = {
      require(array.length == 3)
      array(0) = color.r
      array(1) = color.g
      array(2) = color.b
      array
    }

    override val size: Int = 3

    override def component(color: RGB, index: Int): Double = (index: @switch) match {
      case 0 => color.r
      case 1 => color.g
      case 2 => color.b
      case _ => throw new Exception(s"invalid index ($index) in RGB vectorizer")
    }

    override def fromComponents(comp: (Int) => Double): RGB = RGB(comp(0), comp(1), comp(2))

    override def dim: Int = 3

    override def vectorize(v: RGB): DenseVector[Double] = DenseVector(v.r, v.g, v.b)

    override def unvectorize(d: DenseVector[Double]): RGB = RGB(d(0), d(1), d(2))
  }

  implicit object RGBOperations extends ColorSpaceOperations[RGB] {

    /** add two pixels */
    override def add(pix1: RGB, pix2: RGB): RGB = pix1 + pix2

    /** scalar multiplication */
    override def scale(pix: RGB, l: Double): RGB = pix * l

    /** dot product */
    override def dot(pix1: RGB, pix2: RGB): Double = pix1.dot(pix2)

    /** channel-wise multiplication */
    override def multiply(pix1: RGB, pix2: RGB): RGB = pix1 x pix2

    /** zero element */
    override def zero: RGB = Black

    override val dimensionality = 3
  }

  implicit object RGBInterpolator extends ValueInterpolator[RGB] {
    override def blend(obj1: RGB, obj2: RGB, l: Double): RGB =
      RGB(obj1.r * l + (1f - l) * obj2.r, obj1.g * l + (1f - l) * obj2.g, obj1.b * l + (1f - l) * obj2.b)

    override def average(first: RGB, rest: RGB*): RGB = {
      var r = first.r
      var g = first.g
      var b = first.b
      rest.foreach { c =>
        r += c.r
        g += c.g
        b += c.b
      }
      val n = rest.size + 1
      RGB(r / n, g / n, b / n)
    }

    override def barycentricInterpolation(v1: RGB, f1: Double, v2: RGB, f2: Double, v3: RGB, f3: Double): RGB = {
      RGB(v1.r * f1 + v2.r * f2 + v3.r * f3, v1.g * f1 + v2.g * f2 + v3.g * f3, v1.b * f1 + v2.b * f2 + v3.b * f3)
    }
  }

  //private def toInt8(value: Double): Int = (value * 255.0).toInt

  private def fromInt8(intValue: Int): Double = intValue / 255.0
}
