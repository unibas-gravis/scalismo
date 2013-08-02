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
}

/*======================================
 * Point definitions
 *=======================================*/

/**
 * An ND Point
 */
abstract class Point[D <: Dim: DimTraits] extends Coordinate[D, Double] { self: Coordinate[D, Double] =>

  def +(that: Vector[D]): Point[D]
  def -(that: Vector[D]): Point[D]
  def -(that: Point[D]): Vector[D]

}


trait PointLike[D <: Dim, PointRepr <: Point[D], VectorRepr <: Vector[D]] { self: Point[D] =>

  def createPoint(data: Array[Double]): PointRepr
  def createVector(data: Array[Double]): VectorRepr

  override def +(that: Vector[D]): PointRepr = createPoint(
    (self.data zip that.data).map { case (s, t) => s + t })

  override def -(that: Vector[D]): PointRepr = createPoint(
    (self.data zip that.data).map { case (s, t) => s - t })

  override def -(that: Point[D]): VectorRepr = createVector(
    (self.data zip that.data).map { case (s, t) => s - t })

}

// Concrete instances for 1D, 2D and 3D
case class Point1D(v: Double) extends Point[OneD] with PointLike[OneD, Point1D, Vector1D] {

  def createPoint(data: Array[Double]) = Point1D(data(0))
  def createVector(data: Array[Double]) = Vector1D(data(0))

  override val data = Array(v)
}

case class Point2D(x: Double, y: Double) extends Point[TwoD] with PointLike[TwoD, Point2D, Vector2D] {

  def createPoint(data: Array[Double]) = Point2D(data(0), data(1))
  def createVector(data: Array[Double]) = Vector2D(data(0), data(1))

  override val data = Array(x, y)
}

case class Point3D(x: Double, y: Double, z: Double) extends Point[ThreeD] with PointLike[ThreeD, Point3D, Vector3D] {

  def createPoint(data: Array[Double]) = Point3D(data(0), data(1), data(2))
  def createVector(data: Array[Double]) = Vector3D(data(0), data(1), data(2))

  override val data = Array(x, y, z)
}


/*======================================
 * Vector definitions
 *=======================================*/

abstract class Vector[D <: Dim: DimTraits] extends Coordinate[D, Double] { self: Vector[D] =>

  def +(that: Vector[D]): Vector[D]
  def -(that: Vector[D]): Vector[D]
  def *(s: Double): Vector[D]
  def norm2: Double = data.map(v => v * v).sum
  def norm = math.sqrt(norm2)

  def toPoint: Point[D]
}

trait VectorLike[D <: Dim, VectorRepr <: Vector[D], PointRepr <: Point[D]] { self: Vector[D] =>

  def createPoint(data: Array[Double]): PointRepr
  def createVector(data: Array[Double]): VectorRepr

  override def +(that: Vector[D]): VectorRepr = createVector(
    (self.data zip that.data).map { case (s, t) => s + t })

  override def -(that: Vector[D]): VectorRepr = createVector(
    (self.data zip that.data).map { case (s, t) => s - t })

  override def *(s: Double): VectorRepr = createVector(
    self.data.map(v => s * v))

  def toPoint: PointRepr = createPoint(
    self.data)

}

case class Vector1D(x: Double) extends Vector[OneD] with VectorLike[OneD, Vector1D, Point1D]{
  def createPoint(data: Array[Double]) = Point1D(data(0))
  def createVector(data: Array[Double]) = Vector1D(data(0))

  val data = Array(x)
}

case class Vector2D(x: Double, y: Double) extends Vector[TwoD] with VectorLike[TwoD, Vector2D, Point2D] {
  type TVector = Vector2D
  type TPoint = Point2D

  def createPoint(data: Array[Double]) = Point2D(data(0), data(1))
  def createVector(data: Array[Double]) = Vector2D(data(0), data(1))

  val data = Array(x, y)
}

case class Vector3D(x: Double, y: Double, z: Double) extends Vector[ThreeD] with VectorLike[ThreeD, Vector3D, Point3D]{
  type TVector = Vector3D
  type TPoint = Point3D

  def createPoint(data: Array[Double]) = Point3D(data(0), data(1), data(2))
  def createVector(data: Array[Double]) = Vector3D(data(0), data(1), data(2))

  val data = Array(x, y, z)
}

/*======================================
 * Index definitions
 *=======================================*/

abstract class Index[D <: Dim: DimTraits] extends Coordinate[D, Int] {}


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


