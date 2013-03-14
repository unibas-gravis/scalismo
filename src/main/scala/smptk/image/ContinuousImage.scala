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

trait ContinuousScalarImage[CV[A] <: CoordVector[A]] extends ContinuousImage[CV, Float] { 
  type CI = ContinuousScalarImage[CV]
  
  val pixelDimensionality = 1

  def df(point: CV[Float]): DenseVector[Float]


  def takeDerivative(x: CV[Float]): DenseVector[Float] 

  def liftDerivative: (CV[Float] => Option[DenseVector[Float]]) 

  def +(that: CI): CI 

  
  def -(that:CI): CI 

  def :*(that: CI): CI 

   def *(s: Float): CI

    def square: CI 

   
  def compose(t : Transformation[CV]) : CI   
  def warp(t : Transformation[CV], imageDomainIndFunc : CV[Float] => Boolean) : CI 
  
 
  def differentiate : ContinuousVectorImage[CV]  
}


/**
 * Implementation trait which can be used to specialize the functionality of ContinuousScalarImages for different subtypes 
 */
trait ContinuousScalarImageLike[CV[A] <: CoordVector[A], Repr <: ContinuousScalarImage[CV]]  { self : ContinuousScalarImage[CV] =>
 
  def newConcreteImageRepr(isDefinedAt: CV[Float] => Boolean, f: CV[Float] => Float, df: CV[Float] => DenseVector[Float]) : Repr 

  def df(point: CV[Float]): DenseVector[Float]


  def takeDerivative(x: CV[Float]): DenseVector[Float] = {
    if (!isDefinedAt(x)) throw new Exception(s"Point $x is outside the domain")
    df(x)
  }

  def liftDerivative: (CV[Float] => Option[DenseVector[Float]]) = { x =>
    if (isDefinedAt(x)) Some(df(x))
    else None
  }

  def +(that: CI): Repr = {
    def f(x: CV[Float]): Float = self.f(x) + that.f(x)
    def df(x: CV[Float]) = self.df(x) + that.df(x)
    val newDomain = (pt : CV[Float]) => self.isDefinedAt(pt) && that.isDefinedAt(pt)
    newConcreteImageRepr(newDomain, f, df)  
  }

  
  def -(that: CI): Repr = {
    def f(x: CV[Float]): Float = self.f(x) - that.f(x)
    def df(x: CV[Float]) = self.df(x) - that.df(x)
    val newDomain = (pt : CV[Float]) => self.isDefinedAt(pt) && that.isDefinedAt(pt)
    newConcreteImageRepr(newDomain, f, df)  
  }

  def :*(that: CI): Repr = {
   def f(x: CV[Float]): Float =self.f(x) * that.f(x)
   def df(x : CV[Float]) = self.df(x) * that(x) + that.df(x) * self.f(x)
   val newDomain = (pt : CV[Float]) => self.isDefinedAt(pt) && that.isDefinedAt(pt)
    newConcreteImageRepr(newDomain, f, df)  
   }

   def *(s: Float): Repr = {
   def f(x: CV[Float]): Float = self.f(x) * s
   def df(x : CV[Float]) = self.df(x) * s 
   val newDomain = (pt : CV[Float]) => self.isDefinedAt(pt)
    newConcreteImageRepr(newDomain, f, df)  
   }

    def square: Repr = {
    	def f(x: CV[Float]): Float = self.f(x) * self.f(x) 
    	def df(x : CV[Float]) = self.df(x) * self.f(x) * 2f
    	val newDomain = (pt : CV[Float]) => self.isDefinedAt(pt)
    newConcreteImageRepr(newDomain, f, df)  
   }

   
  def compose(t : Transformation[CV]) : Repr  = { 
      def f(x: CV[Float]) = self.f(t(x))
      def df(x : CV[Float]) = t.takeDerivative(x) * self.df(t(x))
      val newDomain = (pt: CV[Float]) => self.isDefinedAt(pt) && self.isDefinedAt(t(pt))

    newConcreteImageRepr(newDomain, f, df)
  }
   
  def warp(t : Transformation[CV], imageDomainIndFunc : CV[Float] => Boolean) : Repr  = { 
      def f(x: CV[Float]) = self.f(t(x))
      def df(x : CV[Float]) = t.takeDerivative(x) * self.df(t(x))
      val newDomain = (pt: CV[Float]) => imageDomainIndFunc(pt) &&  self.isDefinedAt(t(pt))

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

case class ContinuousScalarImage1D(_isDefinedAt : Point1D => Boolean, _f: Point1D => Float, _df: Point1D => DenseVector[Float]) extends ContinuousScalarImage[CoordVector1D] with ContinuousScalarImageLike[CoordVector1D, ContinuousScalarImage1D] {
 
  override val pixelDimensionality = 1
  def newConcreteImageRepr(isDefinedAt: CoordVector1D[Float] => Boolean, f: CoordVector1D[Float] => Float, df: CoordVector1D[Float] => DenseVector[Float]) : ContinuousScalarImage1D = ContinuousScalarImage1D(isDefinedAt, f, df)
  
  
  def isDefinedAt(pt : Point1D) = _isDefinedAt(pt)
  def f(x: CoordVector1D[Float]) = _f(x)
  def df(x: CoordVector1D[Float]) = _df(x)
}

case class ContinuousScalarImage2D(_isDefinedAt : CoordVector2D[Float] => Boolean, _f: Point2D => Float, _df: Point2D => DenseVector[Float]) extends ContinuousScalarImage[CoordVector2D] with ContinuousScalarImageLike[CoordVector2D, ContinuousScalarImage2D] { 

   def newConcreteImageRepr(isDefinedAt: CoordVector2D[Float] => Boolean, f: CoordVector2D[Float] => Float, df: CoordVector2D[Float] => DenseVector[Float]) : ContinuousScalarImage2D = ContinuousScalarImage2D(isDefinedAt, f, df)
 
  override val pixelDimensionality = 1
  def f(x: CoordVector2D[Float]) = _f(x)
  def df(x: CoordVector2D[Float]) = _df(x)
  def isDefinedAt(pt : Point2D) = _isDefinedAt(pt)  
}

case class ContinuousScalarImage3D(_isDefinedAt : Point3D => Boolean,  _f: Point3D => Float, _df: Point3D => DenseVector[Float]) extends ContinuousScalarImage[CoordVector3D] with ContinuousScalarImageLike[CoordVector3D, ContinuousScalarImage3D] {


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
type CV[A] = CoordVector1D[A]
  
  def f(x: CoordVector1D[Float]) = _f(x)
  def df(x: CoordVector1D[Float]) = _df(x)
  def isDefinedAt(x : CoordVector1D[Float]) = _isDefinedAt(x)
}
