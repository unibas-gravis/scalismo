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

trait ScalarPixel[T] { 
  def fromDouble(d : Double) : T
  def fromFloat(f : Float) : T 
  def fromShort(s : Short) : T
  def toDouble(t : T) : Double
  def toFloat(t : T) : Float
  def toShort(t : T) : Short
}




trait ContinuousImage[CV[A] <: CoordVector[A], Pixel] extends PartialFunction[CV[Float], Pixel] {

  def f(point : CV[Float]) : Pixel  

  def domain: ContinuousImageDomain[CV]
  def apply(x: CV[Float]): Pixel = {
    if (! isDefinedAt(x)) throw new Exception(s"Point $x is outside the domain")
    f(x)
  }
    
  def liftPixelValue: (CV[Float] => Option[Pixel])  = { x=>
    if (isDefinedAt(x)) Some(f(x)) 
    else None
  }
  
  
  def isDefinedAt(pt: CV[Float]) = domain.isInside(pt)
  //def differentiate[Value2](): ContinuousImage[Point, Value2]

  def pixelDimensionality: Int

  
  //TODO add derivative here (use coordinatesVector for return type)
}




trait ContinuousScalarImage[CV[A] <: CoordVector[A]] extends ContinuousImage[CV, Float] { self =>

  val pixelDimensionality = 1

  def df(point : CV[Float]) : DenseVector[Float]

  def takeDerivative(x: CV[Float]): DenseVector[Float] = {
      if (! isDefinedAt(x)) throw new Exception(s"Point $x is outside the domain")
      df(x)
  }
  
  def liftDerivative : (CV[Float] => Option[DenseVector[Float]])  = { x=>
    if (isDefinedAt(x)) Some(df(x)) 
    else None
  }
  
 
  def -(that: ContinuousScalarImage[CV]): ContinuousScalarImage[CV] = {

    require(ContinuousScalarImage.this.domain == that.domain)
    new ContinuousScalarImage[CV] {
      val domain = that.domain
      def f(x: CV[Float]): Float = ContinuousScalarImage.this.f(x) - that.f(x)
      def df(x: CV[Float]) = ContinuousScalarImage.this.df(x) - that.df(x)
    }
  }

  def :*(that: ContinuousScalarImage[CV]): ContinuousScalarImage[CV] = {
    require(ContinuousScalarImage.this.domain == that.domain)
    new ContinuousScalarImage[CV] {
      val domain = that.domain
      def f(x: CV[Float]): Float = {
        ContinuousScalarImage.this.f(x) * that.f(x)
      }
      def df(x: CV[Float]): DenseVector[Float] = {
        ContinuousScalarImage.this.df(x) * that(x) + that.df(x) * ContinuousScalarImage.this(x)
      }
    }
  }

  def *(s: Float) = new ContinuousScalarImage[CV] {
    def f(x: CV[Float]) = ContinuousScalarImage.this(x) * s
    def domain = ContinuousScalarImage.this.domain
    def df(x: CV[Float]): DenseVector[Float] = {
      ContinuousScalarImage.this.df(x) * s
    }
  }

  def integrate: Float = {
    Integration.integrate(ContinuousScalarImage.this)

  }

  def squaredNorm: Float = {
    (ContinuousScalarImage.this :* ContinuousScalarImage.this).integrate
  }

  def differentiate = new ContinuousVectorImage[CV] {
    def domain = ContinuousScalarImage.this.domain
    def f(x: CV[Float]) = df(x)
    def pixelDimensionality = this.domain.dimensionality
  }
  

}

trait ContinuousVectorImage[CV[A] <: CoordVector[A]] extends ContinuousImage[CV, DenseVector[Float]] { self =>
  type Pixel = DenseVector[Float]

  def apply(point: CV[Float]): DenseVector[Float]

  def pixelDimensionality: Int

 
  
}

case class ContinuousScalarImage1D(val domain: ContinuousImageDomain1D, _f: Point1D => Float, _df: Point1D => DenseVector[Float]) extends ContinuousScalarImage[CoordVector1D] {
  override val pixelDimensionality = 1
  def f(x: CoordVector1D[Float]) = _f(x)		  
  def df(x: CoordVector1D[Float]) = _df(x)  
}



case class ContinuousScalarImage2D(val domain: ContinuousImageDomain2D, _f: Point2D => Float, _df: Point2D => DenseVector[Float]) extends ContinuousScalarImage[CoordVector2D] {
  override val pixelDimensionality = 1
  def f(x: CoordVector2D[Float]) = _f(x)		  
  def df(x: CoordVector2D[Float]) = _df(x)  
}

case class ContinuousScalarImage3D(val domain: ContinuousImageDomain3D, _f: Point3D => Float, _df: Point3D => DenseVector[Float]) extends ContinuousScalarImage[CoordVector3D] {
  override val pixelDimensionality = 1
  def f(x: CoordVector3D[Float]) = _f(x)		  
  def df(x: CoordVector3D[Float]) = _df(x)  
}

/////////////////////////////////////////////
// Vector Images
/////////////////////////////////////////////

case class ContinousVectorImage1D(val pixelDimensionality: Int, val domain: ContinuousImageDomain1D, _f: Point1D => DenseVector[Float], _df: Point1D => DenseMatrix[Float]) extends ContinuousVectorImage[CoordVector1D] {
  def f(x: CoordVector1D[Float]) = _f(x)		  
  def df(x: CoordVector1D[Float]) = _df(x)  
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

  def map[Pixel2 : ScalarPixel](f: Pixel => Pixel2): DiscreteScalarImage[CV, Pixel2]
}

trait DiscreteScalarImage[CV[A] <: CoordVector[A], Pixel] extends DiscreteImage[CV, Pixel] {
  def pixelDimensionality = 1

}

case class DiscreteScalarImage1D[Pixel : ScalarPixel](val domain: DiscreteImageDomain1D, val pixelValues: IndexedSeq[Pixel]) extends DiscreteScalarImage[CoordVector1D, Pixel] {
  require(domain.points.size == pixelValues.size)
  def map[A : ScalarPixel](f: Pixel => A) = DiscreteScalarImage1D(this.domain, this.pixelValues.map(f))
}

case class DiscreteScalarImage2D[Pixel : ScalarPixel](val domain: DiscreteImageDomain2D, val pixelValues: IndexedSeq[Pixel]) extends DiscreteScalarImage[CoordVector2D, Pixel] {
  require(domain.points.size == pixelValues.size)
  def map[A  : ScalarPixel](f: Pixel => A) = DiscreteScalarImage2D(this.domain, this.pixelValues.map(f))
}

case class DiscreteScalarImage3D[Pixel : ScalarPixel](val domain: DiscreteImageDomain3D, val pixelValues: IndexedSeq[Pixel]) extends DiscreteScalarImage[CoordVector3D, Pixel] {
  require(domain.points.size == pixelValues.size)
  def map[A  : ScalarPixel](f: Pixel => A) = DiscreteScalarImage3D(this.domain, this.pixelValues.map(f))
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

  
  
  def random[Pixel : ScalarPixel](domain: DiscreteImageDomain1D): DiscreteScalarImage1D[Pixel] = {
    val scalarPixel = implicitly[ScalarPixel[Pixel]]
    val N = domain.points.size
    val values = for (i <- 0 to N) yield Random.nextFloat
    DiscreteScalarImage1D(domain, values.map(scalarPixel.fromFloat(_)))
  }

  

}

object DiscreteImage2D {

  def random[Pixel : ScalarPixel](domain: DiscreteImageDomain2D): DiscreteScalarImage2D[Pixel] = {
    val scalarPixel = implicitly[ScalarPixel[Pixel]]
    val N = domain.points.size
    val values = for (i <- 0 to N) yield Random.nextFloat
    DiscreteScalarImage2D(domain, values.map(scalarPixel.fromFloat(_)))
  }

  
  
 
}

object DiscreteImage3D {

  def random[Pixel : ScalarPixel](domain: DiscreteImageDomain3D): DiscreteScalarImage3D[Pixel] = {
    val scalarPixel = implicitly[ScalarPixel[Pixel]]
    val N = domain.points.size
    val values = for (i <- 0 to N) yield Random.nextFloat
    DiscreteScalarImage3D(domain, values.map(scalarPixel.fromFloat(_)))
  }


 

}

