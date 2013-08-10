package smptk.geometry

import scala.language.implicitConversions
import breeze.linalg.DenseVector
import scala.reflect.ClassTag
import breeze.linalg.DenseMatrix

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
abstract class Point[D <: Dim: DimTraits] extends Coordinate[D, Float] { self: Coordinate[D, Float] =>

  def +(that: Vector[D]): Point[D]
  def -(that: Vector[D]): Point[D]
  def -(that: Point[D]): Vector[D]

}

trait PointLike[D <: Dim, PointRepr <: Point[D], VectorRepr <: Vector[D]] { self: Point[D] =>

  def createPoint(data: Array[Float]): PointRepr
  def createVector(data: Array[Float]): VectorRepr

  override def +(that: Vector[D]): PointRepr = createPoint(
    (self.data zip that.data).map { case (s, t) => s + t })

  override def -(that: Vector[D]): PointRepr = createPoint(
    (self.data zip that.data).map { case (s, t) => s - t })

  override def -(that: Point[D]): VectorRepr = createVector(
    (self.data zip that.data).map { case (s, t) => s - t })

}

// Concrete instances for 1D, 2D and 3D
case class Point1D(v: Float) extends Point[OneD] with PointLike[OneD, Point1D, Vector1D] {

  def createPoint(data: Array[Float]) = Point1D(data(0))
  def createVector(data: Array[Float]) = Vector1D(data(0))

  override val data = Array(v)
  override def apply(i: Int) = if (i == 0) v else throw new ArrayIndexOutOfBoundsException("index $i > 0")
}

case class Point2D(x: Float, y: Float) extends Point[TwoD] with PointLike[TwoD, Point2D, Vector2D] {

  def createPoint(data: Array[Float]) = Point2D(data(0), data(1))
  def createVector(data: Array[Float]) = Vector2D(data(0), data(1))

  override val data = Array(x, y)
  override def apply(i: Int) = {
    if (i == 0) x else if (i == 1) y else throw new ArrayIndexOutOfBoundsException("index $i > 1")
  }
}

case class Point3D(x: Float, y: Float, z: Float) extends Point[ThreeD] with PointLike[ThreeD, Point3D, Vector3D] {

  def createPoint(data: Array[Float]) = Point3D(data(0), data(1), data(2))
  def createVector(data: Array[Float]) = Vector3D(data(0), data(1), data(2))

  override val data = Array(x, y, z)
  override def apply(i: Int) = {
    if (i == 0) x else if (i == 1) y else if (i == 2) z else throw new ArrayIndexOutOfBoundsException("index $i > 2")
  }

}

/*======================================
 * Vector definitions
 *=======================================*/

abstract class Vector[D <: Dim: DimTraits] extends Coordinate[D, Float] { self: Vector[D] =>

  def +(that: Vector[D]): Vector[D]
  def -(that: Vector[D]): Vector[D]
  def *(s: Double): Vector[D]
  def norm2: Double = data.map(v => v * v).sum
  def norm = math.sqrt(norm2)

  def toPoint: Point[D]
  def toBreezeVector = DenseVector(data)
}

trait VectorLike[D <: Dim, VectorRepr <: Vector[D], PointRepr <: Point[D]] { self: Vector[D] =>

  def createPoint(data: Array[Float]): PointRepr
  def createVector(data: Array[Float]): VectorRepr

  override def +(that: Vector[D]): VectorRepr = createVector(
    (self.data zip that.data).map { case (s, t) => s + t })

  override def -(that: Vector[D]): VectorRepr = createVector(
    (self.data zip that.data).map { case (s, t) => s - t })

  override def *(s: Double): VectorRepr = createVector(
    self.data.map(v => s.toFloat * v))

  def toPoint: PointRepr = createPoint(
    self.data)

}

case class Vector1D(x: Float) extends Vector[OneD] with VectorLike[OneD, Vector1D, Point1D] {
  def createPoint(data: Array[Float]) = Point1D(data(0))
  def createVector(data: Array[Float]) = Vector1D(data(0))

  override def apply(i: Int) = {
    if (i == 0) x else throw new ArrayIndexOutOfBoundsException("index $i > 0")
  }

  val data = Array(x)
}

case class Vector2D(x: Float, y: Float) extends Vector[TwoD] with VectorLike[TwoD, Vector2D, Point2D] {
  type TVector = Vector2D
  type TPoint = Point2D

  def createPoint(data: Array[Float]) = Point2D(data(0), data(1))
  def createVector(data: Array[Float]) = Vector2D(data(0), data(1))

  override def apply(i: Int) = {
    if (i == 0) x else if (i == 1) y else throw new ArrayIndexOutOfBoundsException("index $i > 1")
  }

  val data = Array(x, y)
}

case class Vector3D(x: Float, y: Float, z: Float) extends Vector[ThreeD] with VectorLike[ThreeD, Vector3D, Point3D] {
  type TVector = Vector3D
  type TPoint = Point3D

  def createPoint(data: Array[Float]) = Point3D(data(0), data(1), data(2))
  def createVector(data: Array[Float]) = Vector3D(data(0), data(1), data(2))

  override def apply(i: Int) = {
    if (i == 0) x else if (i == 1) y else if (i == 2) z else throw new ArrayIndexOutOfBoundsException("index $i > 2")
  }

  val data = Array(x, y, z)
}

/*======================================
 * Index definitions
 *=======================================*/

abstract class Index[D <: Dim: DimTraits] extends Coordinate[D, Int] {}

case class Index1D(i: Int) extends Index[OneD] {
  val data = Array(i)
  override def apply(id: Int) = {
    if (id == 0) i else throw new ArrayIndexOutOfBoundsException("index $i > 0")
  }

}

case class Index2D(i: Int, j: Int) extends Index[TwoD] {
  val data = Array(i, j)

  override def apply(id: Int) = {
    if (id == 0) i else if (id == 1) j else throw new ArrayIndexOutOfBoundsException("index $i > 2")
  }

}

case class Index3D(i: Int, j: Int, k: Int) extends Index[ThreeD] {
  val data = Array(i, j, k)
  override def apply(id: Int) = {
    if (id == 0) i else if (id == 1) j else if (id == 2) k else throw new ArrayIndexOutOfBoundsException("index $i > 2")
  }

}

/////////////////////////////////////
// Matrix
/////////////////////////////////////
/**
 * Simple matrix class. The data is stored in column major ordering
 */
abstract class MatrixNxN[D <: Dim: DimTraits] {
  val dimensionality: Int = implicitly[DimTraits[D]].dimensionality
  def data: Array[Float]
  def apply(i: Int, j: Int): Float = {
    val d = dimensionality
    data(i + d * j)
  }
  def toBreezeMatrix: DenseMatrix[Float] = DenseMatrix.create(dimensionality, dimensionality, data)
  def *(that: Vector[D]): Vector[D]
  def *(that: MatrixNxN[D]): MatrixNxN[D]
  def :*(that: MatrixNxN[D]): MatrixNxN[D]

  def *(f: Float): MatrixNxN[D]
  def *(d: Double): MatrixNxN[D] = this * d.toFloat
  def +(that: MatrixNxN[D]): MatrixNxN[D]

  override def hashCode = data.deep.hashCode
  override def equals(other: Any): Boolean = other match {
    case that: MatrixNxN[D] => {
      that.canEqual(this) && this.data.deep == that.data.deep
    }
    case _ => false
  }
  def canEqual(other: Any): Boolean = other.isInstanceOf[MatrixNxN[D]]

  override def toString() = {
    val s = new StringBuilder("[")
    for (i <- 0 until dimensionality) {
      s ++= "["
      for (j <- 0 until dimensionality) {
        s ++= this.apply(i, j).toString
        if (j < dimensionality - 1) s ++= ","
      }
      s ++= "]"
      if (i < dimensionality - 1) s ++= "\n"
    }
    s ++= "]"
    s.toString
  }

}

trait MatrixNxNLike[D <: Dim, MatrixRepr <: MatrixNxN[D], VectorRepr <: Vector[D]] extends MatrixNxN[D] { self: MatrixNxN[D] =>

  def createVector(data: Array[Float]): VectorRepr
  def createMatrix(data: Array[Float]): MatrixRepr

  override def *(s: Float): MatrixRepr = {
    val newData = data.map(_ * s)
    createMatrix(newData)
  }

  override def *(that: Vector[D]): VectorRepr = {

    var newVecData = new Array[Float](dimensionality)

    var i = 0
    while (i < dimensionality) {
      var v = 0.0f
      var j = 0;
      while (j < dimensionality) {
        v += self(i, j) * that(j)
        j += 1
      }
      newVecData(i) = v
      i += 1
    }
    createVector(newVecData)
  }

  override def *(that: MatrixNxN[D]): MatrixNxN[D] = {

    var newData = new Array[Float](dimensionality * dimensionality)

    var k = 0
    while (k < dimensionality) {
      var i = 0
      while (i < dimensionality) {
        var v = 0.0f
        var j = 0;
        while (j < dimensionality) {
          v += self(i, j) * that(j, k)
          j += 1
        }
        newData(k * dimensionality + i) = v
        i += 1
      }
      k +=1
    }
    createMatrix(newData)

  }

  override def +(that: MatrixNxN[D]): MatrixRepr = {
    val newData = self.data.zip(that.data).map { case (s, t) => s + t }
    createMatrix(newData)
  }

  override def :*(that: MatrixNxN[D]): MatrixRepr = {
    val newData = self.data.zip(that.data).map { case (s, t) => s * t }
    createMatrix(newData)
  }

}

case class Matrix1x1(val data: Array[Float]) extends MatrixNxNLike[OneD, Matrix1x1, Vector1D] {
  override def createMatrix(data: Array[Float]) = Matrix1x1(data)
  override def createVector(data: Array[Float]) = Vector1D(data(0))
}
case class Matrix2x2(val data: Array[Float]) extends MatrixNxNLike[TwoD, Matrix2x2, Vector2D] {
  override def createMatrix(data: Array[Float]) = Matrix2x2(data)
  override def createVector(data: Array[Float]) = Vector2D(data(0), data(1))
}
case class Matrix3x3(val data: Array[Float]) extends MatrixNxNLike[ThreeD, Matrix3x3, Vector3D] {
  override def createMatrix(data: Array[Float]) = Matrix3x3(data)
  override def createVector(data: Array[Float]) = Vector3D(data(0), data(1), data(2))
}

object Matrix1x1 {
  def eye = Matrix1x1(Array(1))
  def zeros = Matrix1x1(Array(0))
  def ones = Matrix1x1(Array(1))
}
object Matrix2x2 {
  def eye = Matrix2x2(Array(1, 0, 0, 1))
  def zeros = Matrix2x2(Array(0, 0, 0, 0))
  def ones = Matrix2x2(Array(1, 1, 1, 1))
  def apply(row1: Tuple2[Float, Float], row2: Tuple2[Float, Float]) = {
    new Matrix2x2(Array(row1._1, row2._1, row1._2, row2._2))
  }

}
object Matrix3x3 {
  type TupleF = Tuple3[Float, Float, Float]
  def eye = Matrix3x3(Array(1, 0, 0, 0, 1, 0, 0, 0, 1))
  def zeros = Matrix3x3(Array(0, 0, 0, 0, 0, 0, 0, 0, 0))
  def ones = Matrix3x3(Array(1, 1, 1, 1, 1, 1, 1, 1, 1))
  def apply(row1: TupleF, row2: TupleF, row3: TupleF) = {
    new Matrix3x3(Array(row1._1, row2._1, row3._1, row1._2, row2._2, row3._2, row1._3, row2._3, row3._3))
  }
}

object implicits {
  implicit def vector1DToDouble(v: Vector[OneD]) = v(0)
  implicit def doubleToVector1De(d: Double) = Vector1D(d.toFloat)
  implicit def tupleOfDoubleToVector2D(t: (Double, Double)) = Vector2D(t._1.toFloat, t._2.toFloat)
  implicit def tupleOfDoubleToVector3D(t: (Double, Double, Double)) = Vector3D(t._1.toFloat, t._2.toFloat, t._3.toFloat)

  implicit def Point1DToDouble(p: Point[OneD]) = p(0)
  implicit def doubleToPoint1De(d: Double) = Point1D(d.toFloat)
  implicit def tupleOfDoubleToPoint2D(t: (Double, Double)) = Point2D(t._1.toFloat, t._2.toFloat)
  implicit def tupleOfDoubleToPoint3D(t: (Double, Double, Double)) = Point3D(t._1.toFloat, t._2.toFloat, t._3.toFloat)

  implicit def index1DToDouble(v: Index[OneD]) = v(0)
  implicit def intToindex1De(i: Int) = Index1D(i)
  implicit def tupleOfDoubleToindex2D(t: (Int, Int)) = Index2D(t._1, t._2)
  implicit def tupleOfDoubleToindex3D(t: (Int, Int, Int)) = Index3D(t._1, t._2, t._3)
}


