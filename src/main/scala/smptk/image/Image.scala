package smptk.image

import scala.language.higherKinds
import scala.language.implicitConversions
import scala.{ specialized => spec }
import reflect.runtime.universe.{ TypeTag, typeOf }
import scala.reflect.ClassTag
import smptk.numerics.Integration._
import breeze.linalg.DenseVector
import smptk.numerics.Integration
import breeze.linalg.DenseMatrix
import smptk.image.Geometry._
import scala.reflect.ClassTag
import scala.util.Random
import smptk.registration.Transformation

trait ScalarPixel[T] {
  def fromDouble(d: Double): T
  def fromFloat(f: Float): T
  def fromShort(s: Short): T
  def toDouble(t: T): Double
  def toFloat(t: T): Float
  def toShort(t: T): Short
}

trait ContinuousImage[CV[A] <: CoordVector[A], Pixel] extends PartialFunction[CV[Float], Pixel] {
  
  def f(point: CV[Float]): Pixel
  def apply(x: CV[Float]): Pixel = {
    if (!isDefinedAt(x)) throw new Exception(s"Point $x is outside the domain")
    f(x)
  }

  def liftPixelValue: (CV[Float] => Option[Pixel]) = { x =>
    if (isDefinedAt(x)) Some(f(x))
    else None
  }

  def isDefinedAt(pt: CV[Float]) : Boolean
  //def differentiate[Value2](): ContinuousImage[Point, Value2]

  def pixelDimensionality: Int

  //TODO add derivative here (use coordinatesVector for return type)
}

trait ContinuousScalarImage[CV[A] <: CoordVector[A], Repr] extends ContinuousImage[CV, Float] { self =>
  
  
  def newConcreteImageRepr(isDefinedAt: CV[Float] => Boolean, f: CV[Float] => Float, df: CV[Float] => DenseVector[Float]) : Repr
  
  val pixelDimensionality = 1

  def df(point: CV[Float]): DenseVector[Float]


  def takeDerivative(x: CV[Float]): DenseVector[Float] = {
    if (!isDefinedAt(x)) throw new Exception(s"Point $x is outside the domain")
    df(x)
  }

  def liftDerivative: (CV[Float] => Option[DenseVector[Float]]) = { x =>
    if (isDefinedAt(x)) Some(df(x))
    else None
  }

  def -(that: ContinuousScalarImage[CV, Repr]): Repr = {
    def f(x: CV[Float]): Float = ContinuousScalarImage.this.f(x) - that.f(x)
    def df(x: CV[Float]) = ContinuousScalarImage.this.df(x) - that.df(x)
    val newDomain = (pt : CV[Float]) => self.isDefinedAt(pt) && that.isDefinedAt(pt)
    newConcreteImageRepr(newDomain, f, df)  
  }

  def :*(that: ContinuousScalarImage[CV, Repr]): Repr = {
   def f(x: CV[Float]): Float =self.f(x) * that.f(x)
   def df(x : CV[Float]) = self.df(x) * that(x) + that.df(x) * self(x)
   val newDomain = (pt : CV[Float]) => self.isDefinedAt(pt) && that.isDefinedAt(pt)
    newConcreteImageRepr(newDomain, f, df)  
   }

   def *(s: Float): Repr = {
   def f(x: CV[Float]): Float = self.f(x) * s
   def df(x : CV[Float]) = self.df(x) * s 
   val newDomain = (pt : CV[Float]) => self.isDefinedAt(pt)
    newConcreteImageRepr(newDomain, f, df)  
   }

  def warp(t : Transformation[CV]) : Repr  = { 
      def f(x: CV[Float]) = self.f(t(x))
      def df(x : CV[Float]) = self.df(t(x))
      val newDomain = (pt: CV[Float]) => self.isDefinedAt(pt) && self.isDefinedAt(t(pt))

    newConcreteImageRepr(newDomain, f, df)
  }
   
   
 
  def differentiate = new ContinuousVectorImage[CV] {
    def isDefinedAt(pt : CV[Float]) = true
    def f(x: CV[Float]) = df(x)
    def pixelDimensionality = self.pixelDimensionality
  }

}

trait ContinuousVectorImage[CV[A] <: CoordVector[A]] extends ContinuousImage[CV, DenseVector[Float]] { self =>
  type Pixel = DenseVector[Float]

  def apply(point: CV[Float]): DenseVector[Float]

  def pixelDimensionality: Int

}

case class ContinuousScalarImage1D(_isDefinedAt : Point1D => Boolean, _f: Point1D => Float, _df: Point1D => DenseVector[Float]) extends ContinuousScalarImage[CoordVector1D, ContinuousScalarImage1D] {
  override val pixelDimensionality = 1

  def newConcreteImageRepr(isDefinedAt: CoordVector1D[Float] => Boolean, f: CoordVector1D[Float] => Float, df: CoordVector1D[Float] => DenseVector[Float]) : ContinuousScalarImage1D = ContinuousScalarImage1D(isDefinedAt, f, df)
  
  
  def isDefinedAt(pt : Point1D) = _isDefinedAt(pt)
  def f(x: CoordVector1D[Float]) = _f(x)
  def df(x: CoordVector1D[Float]) = _df(x)
}

case class ContinuousScalarImage2D(_isDefinedAt : CoordVector2D[Float] => Boolean, _f: Point2D => Float, _df: Point2D => DenseVector[Float]) extends ContinuousScalarImage[CoordVector2D, ContinuousScalarImage2D] { 
 
   def newConcreteImageRepr(isDefinedAt: CoordVector2D[Float] => Boolean, f: CoordVector2D[Float] => Float, df: CoordVector2D[Float] => DenseVector[Float]) : ContinuousScalarImage2D = ContinuousScalarImage2D(isDefinedAt, f, df)
 
  override val pixelDimensionality = 1
  def f(x: CoordVector2D[Float]) = _f(x)
  def df(x: CoordVector2D[Float]) = _df(x)
  def isDefinedAt(pt : Point2D) = _isDefinedAt(pt)  
}

case class ContinuousScalarImage3D(_isDefinedAt : Point3D => Boolean,  _f: Point3D => Float, _df: Point3D => DenseVector[Float]) extends ContinuousScalarImage[CoordVector3D, ContinuousScalarImage3D] {

   def newConcreteImageRepr(isDefinedAt: CoordVector3D[Float] => Boolean, f: CoordVector3D[Float] => Float, df: CoordVector3D[Float] => DenseVector[Float]) : ContinuousScalarImage3D = ContinuousScalarImage3D(isDefinedAt, f, df)
 
  override val pixelDimensionality = 1
  def f(x: CoordVector3D[Float]) = _f(x)
  def df(x: CoordVector3D[Float]) = _df(x)
  def isDefinedAt(pt : Point3D) = _isDefinedAt(pt)
}

/////////////////////////////////////////////
// Vector Images
/////////////////////////////////////////////

case class ContinousVectorImage1D(val _isDefinedAt : CoordVector1D[Float] => Boolean, val pixelDimensionality: Int, val domain: ContinuousImageDomain1D, _f: Point1D => DenseVector[Float], _df: Point1D => DenseMatrix[Float]) extends ContinuousVectorImage[CoordVector1D] {
  def f(x: CoordVector1D[Float]) = _f(x)
  def df(x: CoordVector1D[Float]) = _df(x)
  def isDefinedAt(x : CoordVector1D[Float]) = _isDefinedAt(x)
}

/////////////////////////////////////////////
// Discrete Images
/////////////////////////////////////////////

trait DiscreteImage[CV[A] <: CoordVector[A], Pixel] extends PartialFunction[Int, Pixel] {
  def domain: DiscreteImageDomain[CV]
  def pixelDimensionality: Int
  def pixelValues: IndexedSeq[Pixel]
  def apply(idx: Int): Pixel = pixelValues(idx)
  def apply(idx: CV[Int]): Pixel = pixelValues(domain.indexToLinearIndex(idx))
  def isDefinedAt(idx: Int) = idx >= 0 && idx <= pixelValues.size
  def isDefinedAt(idx: CV[Int]): Boolean = {
    (0 until domain.dimensionality).foldLeft(true)((res, d) => res && idx(d) >= 0 && idx(d) <= domain.size(d))
  }

  def map[Pixel2: ScalarPixel](f: Pixel => Pixel2): DiscreteScalarImage[CV, Pixel2]
  def foreach[A](f: Pixel => A): Unit = pixelValues.foreach(f)
}

trait DiscreteScalarImage[CV[A] <: CoordVector[A], Pixel] extends DiscreteImage[CV, Pixel] {
  def pixelDimensionality = 1

}

case class DiscreteScalarImage1D[@specialized(Short, Float) Pixel: ScalarPixel](val domain: DiscreteImageDomain1D, val pixelValues: IndexedSeq[Pixel]) extends DiscreteScalarImage[CoordVector1D, Pixel] {
  require(domain.points.size == pixelValues.size)
  def map[@specialized(Short, Float) A: ScalarPixel](f: Pixel => A) = DiscreteScalarImage1D(this.domain, this.pixelValues.map(f))
}

case class DiscreteScalarImage2D[@specialized(Short, Float) Pixel: ScalarPixel](val domain: DiscreteImageDomain2D, val pixelValues: IndexedSeq[Pixel]) extends DiscreteScalarImage[CoordVector2D, Pixel] {
  require(domain.points.size == pixelValues.size)
  def map[@specialized(Short, Float) A: ScalarPixel](f: Pixel => A) = DiscreteScalarImage2D(this.domain, this.pixelValues.map(f))
}

case class DiscreteScalarImage3D[@specialized(Short, Float) Pixel: ScalarPixel](val domain: DiscreteImageDomain3D, val pixelValues: IndexedSeq[Pixel]) extends DiscreteScalarImage[CoordVector3D, Pixel] {
  require(domain.points.size == pixelValues.size)
  def map[@specialized(Short, Float) A: ScalarPixel](f: Pixel => A) = DiscreteScalarImage3D(this.domain, this.pixelValues.map(f))
}

object Image {
  implicit val pixelFloatConversions = new ScalarPixel[Float] {
    def fromDouble(d: Double) = d.toFloat
    def fromFloat(f: Float) = f
    def fromShort(s: Short) = s.toFloat
    def toDouble(t: Float) = t.toDouble
    def toFloat(t: Float) = t
    def toShort(t: Float) = t.toShort
  }

  implicit val pixelShortConversions = new ScalarPixel[Short] {
    def fromDouble(d: Double) = d.toShort
    def fromFloat(f: Float) = f.toShort
    def fromShort(s: Short) = s
    def toDouble(t: Short) = t.toDouble
    def toFloat(t: Short) = t
    def toShort(t: Short) = t.toShort
  }

  implicit val pixelDoubleConversions = new ScalarPixel[Double] {
    def fromDouble(d: Double) = d
    def fromFloat(f: Float) = f.toDouble
    def fromShort(s: Short) = s.toDouble
    def toDouble(t: Double) = t
    def toFloat(t: Double) = t.toFloat
    def toShort(t: Double) = t.toShort
  }
}

object DiscreteImage1D {

  def random[Pixel: ScalarPixel](domain: DiscreteImageDomain1D): DiscreteScalarImage1D[Pixel] = {
    val scalarPixel = implicitly[ScalarPixel[Pixel]]
    val N = domain.points.size
    val values = for (i <- 0 to N) yield Random.nextFloat
    DiscreteScalarImage1D(domain, values.map(scalarPixel.fromFloat(_)))
  }

}

object DiscreteImage2D {

  def random[Pixel: ScalarPixel](domain: DiscreteImageDomain2D): DiscreteScalarImage2D[Pixel] = {
    val scalarPixel = implicitly[ScalarPixel[Pixel]]
    val N = domain.points.size
    val values = for (i <- 0 to N) yield Random.nextFloat
    DiscreteScalarImage2D(domain, values.map(scalarPixel.fromFloat(_)))
  }

}

object DiscreteImage3D {

  def random[Pixel: ScalarPixel](domain: DiscreteImageDomain3D): DiscreteScalarImage3D[Pixel] = {
    val scalarPixel = implicitly[ScalarPixel[Pixel]]
    val N = domain.points.size
    val values = for (i <- 0 to N) yield Random.nextFloat
    DiscreteScalarImage3D(domain, values.map(scalarPixel.fromFloat(_)))
  }

}

