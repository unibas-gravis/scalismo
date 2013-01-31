package smptk.image


import smptk.numerics.Integration._
import breeze.linalg.DenseVector
import smptk.numerics.Integration
import breeze.linalg.DenseMatrix
import smptk.image.Geometry._
import scala.language.higherKinds


trait ContinuousImageLike[CoordVector[A]<:CoordVectorLike[A], Pixel] extends PartialFunction[CoordVector[Float], Pixel] {

  def domain: ContinuousImageDomain[CoordVector]
  def apply(point: CoordVector[Float]): Pixel
  def isDefinedAt(pt: CoordVector[Float]) = domain.isInside(pt)
  //def differentiate[Value2](): ContinuousImage[Point, Value2]

  def pixelDimensionality: Int

}

trait ContinuousScalarImageLike[CoordVector[A] <: CoordVectorLike[A]] extends ContinuousImageLike[CoordVector, Float] { self => 

  val pixelDimensionality = 1

  def -(that: ContinuousScalarImageLike[CoordVector]): ContinuousScalarImageLike[CoordVector] = {

    require(this.domain == that.domain)
    new ContinuousScalarImageLike[CoordVector] {
      val domain = that.domain
      def apply(x: CoordVector[Float]): Float = self(x)-that(x)
      def takeDerivative(x: CoordVector[Float]) = self.takeDerivative(x) - that.takeDerivative(x)
    }
  }

  def :*(that: ContinuousScalarImageLike[CoordVector]): ContinuousScalarImageLike[CoordVector] = {
    require(this.domain == that.domain)
    new ContinuousScalarImageLike[CoordVector] {
      val domain = that.domain
      def apply(x: CoordVector[Float]): Float = {
        self(x) * that(x)
      }
      def takeDerivative(x: CoordVector[Float]): DenseVector[Float] = {
        self.takeDerivative(x) * that(x) + that.takeDerivative(x) * self(x)
      }
    }
  }

  def *(s: Float) = new ContinuousScalarImageLike[CoordVector] {
    def apply(x: CoordVector[Float]) = self(x) * s
    def domain = self.domain
    def takeDerivative(x: CoordVector[Float]): DenseVector[Float] = {
      self.takeDerivative(x) *s
    }
  }

  def integrate: Float = {
    Integration.integrate(this)

  }

  def squaredNorm: Float = {
    (this :* this).integrate
  }

  def differentiate = new ContinuousVectorImageLike[CoordVector] {
    def domain = self.domain
    def apply(x: CoordVector[Float]) = takeDerivative(x)
    def pixelDimensionality = this.domain.dimensionality
  }
  def takeDerivative(x: CoordVector[Float]): DenseVector[Float]
}
 
trait ContinuousVectorImageLike[CoordVector[A] <: CoordVectorLike[A]] extends ContinuousImageLike[CoordVector, DenseVector[Float]] { self => 
  type Pixel = DenseVector[Float]

  def apply(point:CoordVector[Float]): DenseVector[Float]

  def pixelDimensionality: Int
 
}


case class ContinuousScalarImage1D(val domain : ContinuousImageDomain1D, f : Point1D => Float, df : Point1D => DenseVector[Float]) extends ContinuousScalarImageLike[CoordVector1D] {
  override val pixelDimensionality = 1  
  def apply(x : CoordVector1D[Float]) = f(x)
  def takeDerivative(x : CoordVector1D[Float]) = df(x)
}

case class ContinuousScalarImage2D(val domain : ContinuousImageDomain2D, f : Point2D => Float, df : Point2D => DenseVector[Float]) extends ContinuousScalarImageLike[CoordVector2D] {
  override val pixelDimensionality = 1  
  def apply(x : CoordVector2D[Float]) = f(x)
  def takeDerivative(x : CoordVector2D[Float]) = df(x)
}

/////////////////////////////////////////////
// Discrete Images
/////////////////////////////////////////////


trait DiscreteImageLike[CoordVector[A] <: CoordVectorLike[A], Pixel] extends PartialFunction[Int, Pixel] {
  def domain: DiscreteImageDomain[CoordVector]
  def pixelValues : IndexedSeq[Pixel]
  def apply(idx: Int) = pixelValues(idx)
  def isDefinedAt(idx : Int) = idx >= 0 && idx <= pixelValues.size
}


case class DiscreteScalarImage1D(val domain : DiscreteImageDomain1D, val pixelValues : IndexedSeq[Float]) extends DiscreteImageLike[CoordVector1D, Float] { 
  require(domain.points.size == pixelValues.size)
}


case class DiscreteScalarImage2D(val domain : DiscreteImageDomain2D, val pixelValues : IndexedSeq[Float]) extends DiscreteImageLike[CoordVector2D, Float] { 
  require(domain.points.size == pixelValues.size)  
} 

case class ContinousVectorImage1D(val pixelDimensionality : Int, val domain : ContinuousImageDomain1D, f :Point1D => DenseVector[Float], df : Point1D => DenseMatrix[Float]) extends ContinuousVectorImageLike[CoordVector1D] {
  def apply(x : Point1D) = f(x)
  def takeDerivative(x : Point1D) = df(x)
}


