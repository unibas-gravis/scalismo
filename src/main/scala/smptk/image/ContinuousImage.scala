package smptk
package image

import scala.language.implicitConversions
import scala.{ specialized => spec }
import reflect.runtime.universe.{ TypeTag, typeOf }
import scala.reflect.ClassTag
import breeze.linalg.DenseVector
import numerics.Integrator
import breeze.linalg.DenseMatrix
import scala.reflect.ClassTag
import scala.util.Random
import registration.Transformation
import smptk.common.BoxedRegion
import smptk.numerics.Integrator
import smptk.geometry._


/**
 * The generic interface for continuous images
 */
trait ContinuousImage[D <: Dim, @specialized(Short, Float) Pixel] extends Function1[Point[D], Pixel] { self =>

  /** the function that defines the image values */
  val f: Point[D] => Pixel

  def apply(x: Point[D]): Pixel = {
    if (!isDefinedAt(x)) throw new Exception(s"Point $x is outside the domain")
    f(x)
  }

  def liftPixelValue: (Point[D] => Option[Pixel]) = { x =>
    if (isDefinedAt(x)) Some(f(x))
    else None
  }

  def isDefinedAt(pt: Point[D]): Boolean
  //def differentiate[Value2](): ContinuousImage[Point, Value2]

  def pixelDimensionality: Int

  def lift(fl : Pixel => Pixel) : ContinuousImage[D, Pixel] => ContinuousImage[D, Pixel] = { 
    img : ContinuousImage[D, Pixel] => new ContinuousImage[D,  Pixel] {
      override def apply(x : Point[D]) = fl(img.apply(x))
      def pixelDimensionality = img.pixelDimensionality
      val f = img.f
      def isDefinedAt(pt : Point[D]) = img.isDefinedAt(pt)
    }
  }
  	
  //TODO add derivative here (use coordinatesVector for return type)
}

trait ContinuousVectorImage[D <: Dim] extends ContinuousImage[D, DenseVector[Double]] { self =>

  type Pixel = DenseVector[Double]

  def apply(point: Point[D]): DenseVector[Double]

  def pixelDimensionality: Int

}

trait ContinuousScalarImage[D <: Dim] extends ContinuousImage[D, Double] { self: ContinuousImage[D, Double] =>
  type CI = ContinuousScalarImage[D]

  val pixelDimensionality = 1

  /** the function that defines the derivative, if it exists */
  val df: Option[Point[D] => DenseVector[Double]]

  def differentiate: Option[ContinuousVectorImage[D]] = {
    self.df.map(df =>
      new ContinuousVectorImage[D] {
        val f = (x: Point[D]) => df(x)
        def isDefinedAt(pt: Point[D]) = self.isDefinedAt(pt)
        def pixelDimensionality = self.pixelDimensionality
      })
  }

  def +(that: CI): CI

  def -(that: CI): CI

  def :*(that: CI): CI

  def *(s: Double): CI

  def square: CI

  def compose(t: Transformation[D]): CI
  def backwardWarp(t: Transformation[D], imageDomainIndFunc: Point[D] => Boolean): CI

}

/**
 * Implementation trait which can be used to specialize the functionality of ContinuousScalarImages for different subtypes
 */
trait ContinuousScalarImageLike[D <: Dim, Repr <: ContinuousScalarImage[D]] { self: ContinuousScalarImage[D] =>

  def newConcreteImageRepr(isDefinedAt: Point[D] => Boolean, f: Point[D] => Double, df: Option[Point[D] => DenseVector[Double]]): Repr

  def +(that: CI): Repr = {
    def f(x: Point[D]): Double = self.f(x) + that.f(x)
    def df = for (selfdf <- self.df; thatdf <- that.df) yield ((x: Point[D]) => selfdf(x) + thatdf(x))
    val newDomain = (pt: Point[D]) => self.isDefinedAt(pt) && that.isDefinedAt(pt)
    newConcreteImageRepr(newDomain, f, df)
  }

  def -(that: CI): Repr = {
    def f(x: Point[D]): Double = self.f(x) - that.f(x)
    def df = for (seldf <- self.df; thatdf <- that.df) yield (x: Point[D]) => seldf(x) - thatdf(x)
    val newDomain = (pt: Point[D]) => self.isDefinedAt(pt) && that.isDefinedAt(pt)
    newConcreteImageRepr(newDomain, f, df)
  }

  def :*(that: CI): Repr = {
    def f(x: Point[D]): Double = self.f(x) * that.f(x)
    def df = for (selfdf <- self.df; thatdf <- that.df) yield ((x: Point[D]) => selfdf(x) * that(x) + thatdf(x) * self.f(x))
    val newDomain = (pt: Point[D]) => self.isDefinedAt(pt) && that.isDefinedAt(pt)
    newConcreteImageRepr(newDomain, f, df)
  }

  def *(s: Double): Repr = {
    def f(x: Point[D]): Double = self.f(x) * s
    val df = for (selfdf <- self.df) yield (x: Point[D]) => selfdf(x) * s
    val newDomain = (pt: Point[D]) => self.isDefinedAt(pt)
    newConcreteImageRepr(newDomain, f, df)
  }

  def square: Repr = {
    def f(x: Point[D]): Double = self.f(x) * self.f(x)
    val df = for (selfdf <- self.df) yield (x: Point[D]) => selfdf(x) * self.f(x) * 2.
    val newDomain = (pt: Point[D]) => self.isDefinedAt(pt)
    newConcreteImageRepr(newDomain, f, df)
  }

  def compose(t: Transformation[D]): Repr = {
    def f(x: Point[D]) = self.f(t(x))
    val df = for (selfdf <- self.df) yield ((x: Point[D]) => t.takeDerivative(x) * selfdf(t(x)))
    val newDomain = (pt: Point[D]) => self.isDefinedAt(pt) && self.isDefinedAt(t(pt))

    newConcreteImageRepr(newDomain, f, df)
  }

  def backwardWarp(t: Transformation[D], imageDomainIndFunc: Point[D] => Boolean): Repr = {
    def f(x: Point[D]) = self.f(t(x))
    def df(x: Point[D]) = for (selfdf <- self.df) yield t.takeDerivative(x) * selfdf(t(x))
    val newDomain = (pt: Point[D]) => imageDomainIndFunc(pt) && self.isDefinedAt(t(pt))

    newConcreteImageRepr(newDomain, f, self.df)
  }

}

case class ContinuousScalarImage1D(_isDefinedAt: Point[OneD] => Boolean, val f: Point[OneD] => Double, val df: Option[Point[OneD] => DenseVector[Double]] = None) extends ContinuousScalarImage[OneD] with ContinuousScalarImageLike[OneD, ContinuousScalarImage1D] {

  override val pixelDimensionality = 1
  def newConcreteImageRepr(isDefinedAt: Point[OneD] => Boolean, f: Point[OneD] => Double, df: Option[Point[OneD] => DenseVector[Double]]): ContinuousScalarImage1D = ContinuousScalarImage1D(isDefinedAt, f, df)

  def isDefinedAt(pt: Point[OneD]) = _isDefinedAt(pt)

  def convolve(filter: Filter[OneD], integrator: Integrator[OneD]): ContinuousScalarImage1D = {
    def convolvedImgFun(x: Point[OneD]) = {

      // f(x) = Int (Img(x-t)*G(t) dt
      def intermediateF(t: Point[OneD]) = {
        val p = Point1D(x(0) - t(0))
        this.liftPixelValue(p).getOrElse(0.) * filter(t)
      }

      val intermediateContinuousImage = ContinuousScalarImage1D(this.isDefinedAt, intermediateF)
      integrator.integrateScalar(intermediateContinuousImage, filter.support)

    }

    def convolvedImgDerivative: Option[Point[OneD] => DenseVector[Double]] = {
      if (this.df.isDefined)
        Some((x: Point[OneD]) => {
          val thisDF = this.df.get
          def intermediateDF(t: Point[OneD]) = {
            val p = Point1D(x(0) - t(0))
            if(this.isDefinedAt(p))
            	thisDF(p) * filter(t)
            else DenseVector.zeros[Double](pixelDimensionality)
            	  
          }
          val intermediateContinuousImage = ContinuousVectorImage1D((t: Point[OneD]) => true, pixelDimensionality, intermediateDF, None)
          integrator.integrateVector(intermediateContinuousImage, filter.support)
        })

      else None
    }

    ContinuousScalarImage1D(this.isDefinedAt, convolvedImgFun, convolvedImgDerivative)
  }

}

case class ContinuousScalarImage2D(_isDefinedAt: Point[TwoD] => Boolean, val f: Point[TwoD] => Double, val df: Option[Point[TwoD] => DenseVector[Double]] = None) extends ContinuousScalarImage[TwoD] with ContinuousScalarImageLike[TwoD, ContinuousScalarImage2D] {

  def newConcreteImageRepr(isDefinedAt: Point[TwoD] => Boolean, f: Point[TwoD] => Double, df: Option[Point[TwoD] => DenseVector[Double]]): ContinuousScalarImage2D = ContinuousScalarImage2D(isDefinedAt, f, df)

  override val pixelDimensionality = 1
  def isDefinedAt(pt: Point[TwoD]) = _isDefinedAt(pt)

  def convolve(filter:Filter[TwoD], integrator: Integrator[TwoD]): ContinuousScalarImage2D = {

    def f(x: Point[TwoD]) = {

      def intermediateF(t: Point[TwoD]) = {
        val p = Point2D(x(0) - t(0), x(1) - t(1))
        this.liftPixelValue(p).getOrElse(0.) * filter(t)
      }

      val intermediateContinuousImage = ContinuousScalarImage2D((t: Point[TwoD]) => true, intermediateF)
      integrator.integrateScalar(intermediateContinuousImage, filter.support)

    }
    
    def convolvedImgDerivative: Option[Point[TwoD] => DenseVector[Double]] = {
      if (this.df.isDefined)
        Some((x: Point[TwoD]) => {
          val thisDF = this.df.get
          def intermediateDF(t: Point[TwoD]) = {
            val p = Point2D(x(0) - t(0), x(1) - t(1))
            
            if(this.isDefinedAt(p))
            	thisDF(p) * filter(t)
            else DenseVector.zeros[Double](2)
            	  
          }
          val intermediateContinuousImage = ContinuousVectorImage2D((t: Point[TwoD]) => true, 2, intermediateDF, None)
          integrator.integrateVector(intermediateContinuousImage, filter.support)
        })

      else None
      
    }


    ContinuousScalarImage2D(this.isDefinedAt, f,convolvedImgDerivative)
  }

}

case class ContinuousScalarImage3D(_isDefinedAt: Point[ThreeD] => Boolean, val f: Point[ThreeD] => Double, val df: Option[Point[ThreeD] => DenseVector[Double]] = None) extends ContinuousScalarImage[ThreeD] with ContinuousScalarImageLike[ThreeD, ContinuousScalarImage3D] {

  def newConcreteImageRepr(isDefinedAt: Point[ThreeD] => Boolean, f: Point[ThreeD] => Double, df: Option[Point[ThreeD] => DenseVector[Double]]): ContinuousScalarImage3D = ContinuousScalarImage3D(isDefinedAt, f, df)

  override val pixelDimensionality = 1
  def isDefinedAt(pt: Point[ThreeD]) = _isDefinedAt(pt)
  
    def convolve(filter:Filter[ThreeD], integrator: Integrator[ThreeD]): ContinuousScalarImage3D = {

    def f(x: Point[ThreeD]) = {

      def intermediateF(t: Point[ThreeD]) = {
        val p = Point3D(x(0) - t(0), x(1) - t(1), x(2) - t(2))
        this.liftPixelValue(p).getOrElse(0.) * filter(t)
      }

      val intermediateContinuousImage = ContinuousScalarImage3D((t: Point[ThreeD]) => true, intermediateF)
      integrator.integrateScalar(intermediateContinuousImage, filter.support)

    }
    
    def convolvedImgDerivative: Option[Point[ThreeD] => DenseVector[Double]] = {
      if (this.df.isDefined)
        Some((x: Point[ThreeD]) => {
          val thisDF = this.df.get
          def intermediateDF(t: Point[ThreeD]) = {
            val p = Point3D(x(0) - t(0), x(1) - t(1), x(2) - t(2))
            
            if(this.isDefinedAt(p))
            	thisDF(p) * filter(t)
            else DenseVector.zeros[Double](3)
            	  
          }
          val intermediateContinuousImage = ContinuousVectorImage3D((t: Point[ThreeD]) => true, 3, intermediateDF, None)
          integrator.integrateVector(intermediateContinuousImage, filter.support)
        })

      else None
      
    }


    ContinuousScalarImage3D(this.isDefinedAt, f,convolvedImgDerivative)
  }
}

/////////////////////////////////////////////
// Vector Images
/////////////////////////////////////////////

case class ContinuousVectorImage1D(val _isDefinedAt: Point[OneD] => Boolean, val pixelDimensionality: Int, val f: Point[OneD] => DenseVector[Double], val df: Option[Point[OneD] => DenseMatrix[Double]]) extends ContinuousVectorImage[OneD] {

 
  def isDefinedAt(x: Point[OneD]) = _isDefinedAt(x)
}

case class ContinuousVectorImage2D(val _isDefinedAt: Point[TwoD] => Boolean, val pixelDimensionality: Int, val f: Point[TwoD] => DenseVector[Double], val df: Option[Point[TwoD] => DenseMatrix[Double]]) extends ContinuousVectorImage[TwoD] {


  def isDefinedAt(x: Point[TwoD]) = _isDefinedAt(x)
}

case class ContinuousVectorImage3D(val _isDefinedAt: Point[ThreeD] => Boolean, val pixelDimensionality: Int, val f: Point[ThreeD] => DenseVector[Double], val df: Option[Point[ThreeD] => DenseMatrix[Double]]) extends ContinuousVectorImage[ThreeD] {

  def isDefinedAt(x: Point[ThreeD]) = _isDefinedAt(x)
}

