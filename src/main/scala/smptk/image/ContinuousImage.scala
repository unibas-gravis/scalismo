package smptk
package image

import scala.language.higherKinds
import scala.language.implicitConversions
import scala.{ specialized => spec }
import reflect.runtime.universe.{ TypeTag, typeOf }
import scala.reflect.ClassTag
import numerics.Integration._
import breeze.linalg.DenseVector
import numerics.Integration
import breeze.linalg.DenseMatrix
import Geometry._
import scala.reflect.ClassTag
import scala.util.Random
import registration.Transformation


/** 
 * The generic interface for continuous images
 */
trait ContinuousImage[CV[A] <: CoordVector[A], @specialized(Short, Float) Pixel] {
  
  def f(point: CV[Double]): Pixel
  def apply(x: CV[Double]): Pixel = {
    if (!isDefinedAt(x)) throw new Exception(s"Point $x is outside the domain")
    f(x)
  }

  def liftPixelValue: (CV[Double] => Option[Pixel]) = { x =>
    if (isDefinedAt(x)) Some(f(x))
    else None
  }

  def isDefinedAt(pt: CV[Double]) : Boolean
  //def differentiate[Value2](): ContinuousImage[Point, Value2]

  def pixelDimensionality: Int

  //TODO add derivative here (use coordinatesVector for return type)
}

trait ContinuousScalarImage[CV[A] <: CoordVector[A]] extends ContinuousImage[CV, Double] { 
  type CI = ContinuousScalarImage[CV]
  
  val pixelDimensionality = 1

  def df(point: CV[Double]): DenseVector[Double]


  def takeDerivative(x: CV[Double]): DenseVector[Double] 

  def liftDerivative: (CV[Double] => Option[DenseVector[Double]]) 

  def +(that: CI): CI 

  
  def -(that:CI): CI 

  def :*(that: CI): CI 

   def *(s: Double): CI

    def square: CI 

   
  def compose(t : Transformation[CV]) : CI   
  def warp(t : Transformation[CV], imageDomainIndFunc : CV[Double] => Boolean) : CI 
  
 
  def differentiate : ContinuousVectorImage[CV]  
}


/**
 * Implementation trait which can be used to specialize the functionality of ContinuousScalarImages for different subtypes 
 */
trait ContinuousScalarImageLike[CV[A] <: CoordVector[A], Repr <: ContinuousScalarImage[CV]]  { self : ContinuousScalarImage[CV] =>
 
  def newConcreteImageRepr(isDefinedAt: CV[Double] => Boolean, f: CV[Double] => Double, df: CV[Double] => DenseVector[Double]) : Repr 

  def df(point: CV[Double]): DenseVector[Double]


  def takeDerivative(x: CV[Double]): DenseVector[Double] = {
    if (!isDefinedAt(x)) throw new Exception(s"Point $x is outside the domain")
    df(x)
  }

  def liftDerivative: (CV[Double] => Option[DenseVector[Double]]) = { x =>
    if (isDefinedAt(x)) Some(df(x))
    else None
  }

  def +(that: CI): Repr = {
    def f(x: CV[Double]): Double = self.f(x) + that.f(x)
    def df(x: CV[Double]) = self.df(x) + that.df(x)
    val newDomain = (pt : CV[Double]) => self.isDefinedAt(pt) && that.isDefinedAt(pt)
    newConcreteImageRepr(newDomain, f, df)  
  }

  
  def -(that: CI): Repr = {
    def f(x: CV[Double]): Double = self.f(x) - that.f(x)
    def df(x: CV[Double]) = self.df(x) - that.df(x)
    val newDomain = (pt : CV[Double]) => self.isDefinedAt(pt) && that.isDefinedAt(pt)
    newConcreteImageRepr(newDomain, f, df)  
  }

  def :*(that: CI): Repr = {
   def f(x: CV[Double]): Double =self.f(x) * that.f(x)
   def df(x : CV[Double]) = self.df(x) * that(x) + that.df(x) * self.f(x)
   val newDomain = (pt : CV[Double]) => self.isDefinedAt(pt) && that.isDefinedAt(pt)
    newConcreteImageRepr(newDomain, f, df)  
   }

   def *(s: Double): Repr = {
   def f(x: CV[Double]): Double = self.f(x) * s
   def df(x : CV[Double]) = self.df(x) * s 
   val newDomain = (pt : CV[Double]) => self.isDefinedAt(pt)
    newConcreteImageRepr(newDomain, f, df)  
   }

    def square: Repr = {
    	def f(x: CV[Double]): Double = self.f(x) * self.f(x) 
    	def df(x : CV[Double]) = self.df(x) * self.f(x) * 2.
    	val newDomain = (pt : CV[Double]) => self.isDefinedAt(pt)
    newConcreteImageRepr(newDomain, f, df)  
   }

   
  def compose(t : Transformation[CV]) : Repr  = { 
      def f(x: CV[Double]) = self.f(t(x))
      def df(x : CV[Double]) = t.takeDerivative(x) * self.df(t(x))
      val newDomain = (pt: CV[Double]) => self.isDefinedAt(pt) && self.isDefinedAt(t(pt))

    newConcreteImageRepr(newDomain, f, df)
  }
   
  def warp(t : Transformation[CV], imageDomainIndFunc : CV[Double] => Boolean) : Repr  = { 
      def f(x: CV[Double]) = self.f(t(x))
      def df(x : CV[Double]) = t.takeDerivative(x) * self.df(t(x))
      val newDomain = (pt: CV[Double]) => imageDomainIndFunc(pt) &&  self.isDefinedAt(t(pt))

    newConcreteImageRepr(newDomain, f, df)
  }

  
 
  def differentiate = new ContinuousVectorImage[CV] {
    def isDefinedAt(pt : CV[Double]) = true
    def f(x: CV[Double]) = df(x)
    def pixelDimensionality = self.pixelDimensionality
  }

}

trait ContinuousVectorImage[CV[A] <: CoordVector[A]] extends ContinuousImage[CV, DenseVector[Double]] { self =>

  type Pixel = DenseVector[Double]

  def apply(point: CV[Double]): DenseVector[Double]

  def pixelDimensionality: Int

}

case class ContinuousScalarImage1D(_isDefinedAt : Point1D => Boolean, _f: Point1D => Double, _df: Point1D => DenseVector[Double]) extends ContinuousScalarImage[CoordVector1D] with ContinuousScalarImageLike[CoordVector1D, ContinuousScalarImage1D] {
 
  override val pixelDimensionality = 1
  def newConcreteImageRepr(isDefinedAt: CoordVector1D[Double] => Boolean, f: CoordVector1D[Double] => Double, df: CoordVector1D[Double] => DenseVector[Double]) : ContinuousScalarImage1D = ContinuousScalarImage1D(isDefinedAt, f, df)
  
  
  def isDefinedAt(pt : Point1D) = _isDefinedAt(pt)
  def f(x: CoordVector1D[Double]) = _f(x)
  def df(x: CoordVector1D[Double]) = _df(x)
}

case class ContinuousScalarImage2D(_isDefinedAt : CoordVector2D[Double] => Boolean, _f: Point2D => Double, _df: Point2D => DenseVector[Double]) extends ContinuousScalarImage[CoordVector2D] with ContinuousScalarImageLike[CoordVector2D, ContinuousScalarImage2D] { 

   def newConcreteImageRepr(isDefinedAt: Point2D => Boolean, f: Point2D => Double, df: Point2D => DenseVector[Double]) : ContinuousScalarImage2D = ContinuousScalarImage2D(isDefinedAt, f, df)
 
  override val pixelDimensionality = 1
  def f(x: CoordVector2D[Double]) = _f(x)
  def df(x: CoordVector2D[Double]) = _df(x)
  def isDefinedAt(pt : Point2D) = _isDefinedAt(pt)  
}

case class ContinuousScalarImage3D(_isDefinedAt : Point3D => Boolean,  _f: Point3D => Double, _df: Point3D => DenseVector[Double]) extends ContinuousScalarImage[CoordVector3D] with ContinuousScalarImageLike[CoordVector3D, ContinuousScalarImage3D] {


   def newConcreteImageRepr(isDefinedAt: Point3D => Boolean, f: Point3D => Double, df: Point3D => DenseVector[Double]) : ContinuousScalarImage3D = ContinuousScalarImage3D(isDefinedAt, f, df)
 
  override val pixelDimensionality = 1
  def f(x: Point3D) = _f(x)
  def df(x: Point3D) = _df(x)
  def isDefinedAt(pt : Point3D) = _isDefinedAt(pt)
}

/////////////////////////////////////////////
// Vector Images
/////////////////////////////////////////////

case class ContinousVectorImage1D(val _isDefinedAt : Point1D => Boolean, val pixelDimensionality: Int,  _f: Point1D => DenseVector[Double], _df: Point1D => DenseMatrix[Double]) extends ContinuousVectorImage[CoordVector1D] {
type CV[A] = CoordVector1D[A]
  
  def f(x: Point1D) = _f(x)
  def df(x: Point1D) = _df(x)
  def isDefinedAt(x : Point1D) = _isDefinedAt(x)
}
