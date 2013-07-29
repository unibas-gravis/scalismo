package smptk.geometry

import scala.language.implicitConversions

import breeze.linalg.DenseVector
import scala.reflect.ClassTag

/** a marker trait to distinguish the dimension */
sealed trait Dim
trait OneD extends Dim
trait TwoD extends Dim
trait ThreeD extends Dim

/**
 * The basic N-tuple in R^N with scalar type S
 */
abstract class Coordinate[D <: Dim: DimTraits, @specialized(Int, Float, Double) S] {
  val data: Array[S]
  def dimensionality: Int = implicitly[DimTraits[D]].dimensionality
  def apply(i: Int): S = data(i)

  override def toString: String = {
    "(" + data.mkString(", ") + ")"
  }

}

/**
 * An ND Point
 */
abstract class Point[D <: Dim: DimTraits] extends Coordinate[D, Double] { self: Coordinate[D, Double] =>

  def +(that: Vector[D]): Point[D] = new Point[D] {
    val data = (self.data zip that.data).map { case (s, t) => s + t }
  }

  def -(that: Vector[D]): Point[D] = new Point[D] {
    val data = (self.data zip that.data).map { case (s, t) => s - t }
  }

  def -(that: Point[D]): Vector[D] = new Vector[D] {
    val data = (self.data zip that.data).map { case (s, t) => s - t }
  }

  override def hashCode = {
    data.hashCode()
  }

  override def equals(other: Any): Boolean = other match {

    case that: Point[D] => (
      that.canEqual(this)
      && this.data.deep == (that.data.deep))

    case _ => false
  }
  def canEqual(other: Any): Boolean = other.isInstanceOf[Point[D]]

}

abstract class Vector[D <: Dim: DimTraits] extends Coordinate[D, Double] { self: Vector[D] =>
  def +(that: Vector[D]): Vector[D] = new Vector[D] {
    val data = (self.data zip that.data).map { case (s, t) => s + t }
  }

  def -(that: Vector[D]): Vector[D] = new Vector[D] {
    val data = (self.data zip that.data).map { case (s, t) => s - t }
  }

  def *(s: Double): Vector[D] = new Vector[D] {
    val data = self.data.map(v => s * v)
  }

  def norm2: Double = data.map(v => v * v).sum
  def norm = math.sqrt(norm2)

  def toPoint: Point[D] = new Point[D] {
    val data = self.data
  }

  override def hashCode = data.hashCode()

  override def equals(other: Any): Boolean = other match {

    case that: Vector[D] => (
      that.canEqual(this)
      && this.data.deep == (that.data.deep))

    case _ => false
  }
  def canEqual(other: Any): Boolean = other.isInstanceOf[Vector[D]]

}

abstract class Index[D <: Dim: DimTraits] extends Coordinate[D, Int] {

  override def hashCode = data.hashCode()

  override def equals(other: Any): Boolean = other match {

    case that: Index[D] => (
      that.canEqual(this)
      && this.data.deep == (that.data.deep))

    case _ => false
  }
  def canEqual(other: Any): Boolean = other.isInstanceOf[Index[D]]

}

// Concrete instances for 1D, 2D and 3D
case class Point1D(v: Double) extends Point[OneD] {
  override val data = Array(v)
}

case class Point2D(x: Double, y: Double) extends Point[TwoD] {
  override val data = Array(x, y)
}

case class Point3D(x: Double, y: Double, z: Double) extends Point[ThreeD] {
  override val data = Array(x, y, z)
}

case class Vector1D(x: Double) extends Vector[OneD] {
  val data = Array(x)
}

case class Vector2D(x: Double, y: Double) extends Vector[TwoD] {
  val data = Array(x, y)
}

case class Vector3D(x: Double, y: Double, z: Double) extends Vector[ThreeD] {
  val data = Array(x, y, z)
}

case class Index1D(i: Int) extends Index[OneD] {
  val data = Array(i)
}

case class Index2D(i: Int, j: Int) extends Index[TwoD] {
  val data = Array(i, j)
}

case class Index3D(i: Int, j: Int, k: Int) extends Index[ThreeD] {
  val data = Array(i, j, k)
}

object implicits {
  implicit def vector1DToDouble(v: Vector[OneD]) = v(0)
  implicit def doubleToVector1De(d: Double) = Vector1D(d)
  implicit def tupleOfDoubleToVector2D(t: (Double, Double)) = Vector2D(t._1, t._2)
  implicit def tupleOfDoubleToVector3D(t: (Double, Double, Double)) = Vector3D(t._1, t._2, t._3)

  implicit def Point1DToDouble(p: Point[OneD]) = p(0)
  implicit def doubleToPoint1De(d: Double) = Point1D(d)
  implicit def tupleOfDoubleToPoint2D(t: (Double, Double)) = Point2D(t._1, t._2)
  implicit def tupleOfDoubleToPoint3D(t: (Double, Double, Double)) = Point3D(t._1, t._2, t._3)

  implicit def index1DToDouble(v: Index[OneD]) = v(0)
  implicit def intToindex1De(i: Int) = Index1D(i)
  implicit def tupleOfDoubleToindex2D(t: (Int, Int)) = Index2D(t._1, t._2)
  implicit def tupleOfDoubleToindex3D(t: (Int, Int, Int)) = Index3D(t._1, t._2, t._3)
}


