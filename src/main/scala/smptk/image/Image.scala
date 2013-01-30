package smptk.image


import smptk.numerics.Integration._
import breeze.linalg.DenseVector
import smptk.numerics.Integration
import breeze.linalg.DenseMatrix



trait DiscreteImageLike[CoordVector <: CoordVectorLike, Pixel] extends PartialFunction[CoordVector, Pixel] {
  def domain: DiscreteImageDomain[CoordVector]

  //def apply(idx: Space#Index)
  def apply(idx: DenseVector[Int]): Pixel
}

trait ContinuousImageLike[CoordVector<:CoordVectorLike, Pixel] extends PartialFunction[CoordVector, Pixel] {

  def domain: ContinuousImageDomain[CoordVector]
  def apply(point: CoordVector): Pixel
  def isDefinedAt(pt: CoordVector) = domain.isInside(pt)
  //def differentiate[Value2](): ContinuousImage[Point, Value2]

  def pixelDimensionality: Int

}

trait ContinuousScalarImageLike[CoordVector <: CoordVectorLike] extends ContinuousImageLike[CoordVector, Float] { self => 

  val pixelDimensionality = 1

  def -(that: ContinuousScalarImageLike[CoordVector]): ContinuousScalarImageLike[CoordVector] = {

    require(this.domain == that.domain)
    new ContinuousScalarImageLike[CoordVector] {
      val domain = that.domain
      def apply(x: CoordVector): Float = self(x)-that(x)
      def takeDerivative(x: CoordVector) = self.takeDerivative(x) - that.takeDerivative(x)
    }
  }

  def :*(that: ContinuousScalarImageLike[CoordVector]): ContinuousScalarImageLike[CoordVector] = {
    require(this.domain == that.domain)
    new ContinuousScalarImageLike[CoordVector] {
      val domain = that.domain
      def apply(x: CoordVector): Float = {
        self(x) * that(x)
      }
      def takeDerivative(x: CoordVector): DenseVector[Float] = {
        self.takeDerivative(x) * that(x) + that.takeDerivative(x) * self(x)
      }
    }
  }

  def *(s: Float) = new ContinuousScalarImageLike[CoordVector] {
    def apply(x: CoordVector) = self(x) * s
    def domain = self.domain
    def takeDerivative(x: CoordVector): DenseVector[Float] = {
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
    def apply(x: CoordVector) = takeDerivative(x)
    def pixelDimensionality = this.domain.dimensionality
  }
  def takeDerivative(x: CoordVector): DenseVector[Float]
}

trait ContinuousVectorImageLike[CoordVector <: CoordVectorLike] extends ContinuousImageLike[CoordVector, DenseVector[Float]] { self => 
  type Pixel = DenseVector[Float]

  def apply(point:CoordVector): DenseVector[Float]

  def pixelDimensionality: Int
 
}


case class ContinuousScalarImage1D(val domain : ContinuousImageDomain1D, f : CoordVector1D => Float, df : CoordVector1D => DenseVector[Float]) extends ContinuousScalarImageLike[CoordVector1D] {
  val pixelDimensionality = 1  
  def apply(x : CoordVector1D) = f(x)
  def takeDerivative(x : CoordVector1D) = df(x)
}

case class ContinuousScalarImage2D(val domain : ContinuousImageDomain2D, f : CoordVector2D => Float, df : CoordVector2D => DenseVector[Float]) extends ContinuousScalarImageLike[CoordVector1D] {
  val pixelDimensionality = 1  
  def apply(x : CoordVector2D) = f(x)
  def takeDerivative(x : CoordVector2D) = df(x)
}

case class DiscreteScalarImage1D(val domain : DiscreteImageDomain1D, val values : IndexedSeq[Float]) extends DiscreteImageLike[CoordVector1D, Float]{
  def apply(i : DenseVector[Int]) = values(i(0))
}

case class DiscreteScalarImage2D(val domain : DiscreteImageDomain2D, val values : IndexedSeq[Float]) extends DiscreteImageLike[CoordVector2D, Float]{
  def apply(i : DenseVector[Int]) = 0f // for now,. need to agree on a vectorization method
}


case class ContinousVectorImage1D(val pixelDimensionality : Int, val domain : ContinuousImageDomain1D, f :CoordVector1D => DenseVector[Float], df : CoordVector1D => DenseMatrix[Float]) extends ContinuousVectorImageLike[CoordVector1D] {
  def apply(x : CoordVector1D) = f(x)
  def takeDerivative(x : CoordVector1D) = df(x)
}


object ContinuousImage {

  //  type Interpolator[Domain, Pixel] = DiscreteImage[Domain, Pixel] => ContinuousImage[Domain, Pixel]
  //  type Resampler[Domain, Pixel] = DiscreteImage[Domain, Pixel] => DiscreteImage[Domain, Pixel]
}
