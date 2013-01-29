package smptk.image


import smptk.numerics.Integration._
import breeze.linalg.DenseVector
import smptk.numerics.Integration
import breeze.linalg.DenseMatrix



trait DiscreteImageLike[Point <:PointLike, Vector<: VectorLike, Pixel] extends PartialFunction[Point, Pixel] {
  def domain: DiscreteImageDomain[Point, Vector]

  //def apply(idx: Space#Index)
  def apply(id: Int): Pixel
}

trait ContinuousImageLike[Point <:PointLike, Vector <: VectorLike, Pixel] extends PartialFunction[Point, Pixel] {

  def domain: ImageDomain[Point]
  def apply(point: Point): Pixel
  def isDefinedAt(pt: Point) = domain.isInside(pt)
  //def differentiate[Value2](): ContinuousImage[Point, Value2]

  def pixelDimensionality: Int

}



trait ContinuousScalarImageLike[Point <:PointLike, Vector <: VectorLike]extends ContinuousImageLike[Point, Vector, Float] { self => // TODO change it back to Pixel

  val pixelDimensionality = 1

  def -(that: ContinuousScalarImageLike[Point, Vector]): ContinuousScalarImageLike[Point, Vector] = {

    require(this.domain == that.domain)
    new ContinuousScalarImageLike[Point, Vector] {
      val domain = that.domain
      def apply(x: Point): Float = self(x)-that(x)
      def takeDerivative(x: Point) = self.takeDerivative(x) - that.takeDerivative(x)
    }
  }

  def :*(that: ContinuousScalarImageLike[Point, Vector]): ContinuousScalarImageLike[Point, Vector] = {
    require(this.domain == that.domain)
    new ContinuousScalarImageLike[Point, Vector] {
      val domain = that.domain
      def apply(x: Point): Float = {
        self(x) * that(x)
      }
      def takeDerivative(x: Point): DenseVector[Float] = {
        self.takeDerivative(x) * that(x) + that.takeDerivative(x) * self(x)
      }
    }
  }

  def *(s: Float) = new ContinuousScalarImageLike[Point, Vector] {
    def apply(x: Point) = self(x) * s
    def domain = self.domain
    def takeDerivative(x: Point): DenseVector[Float] = {
      self.takeDerivative(x) *s
    }
  }

  def integrate: Float = {
    Integration.integrate(this)

  }

  def squaredNorm: Float = {
    (this :* this).integrate
  }

  def differentiate = new ContinuousVectorImageLike[Point, Vector] {
    def domain = self.domain
    def apply(x: Point) = takeDerivative(x)
    def pixelDimensionality = this.domain.dimensionality
  }
  def takeDerivative(x: Point): DenseVector[Float]
}

trait ContinuousVectorImageLike[Point <: PointLike, Vector <: VectorLike] extends ContinuousImageLike[Point, Vector, DenseVector[Float]] { self => // TODO change it back to Pixel
  type Pixel = DenseVector[Float]

  def apply(point:Point): DenseVector[Float]

  def pixelDimensionality: Int
 
}


case class ContinuousScalarImage1D(val domain : ImageDomain1D, f : Point1D => Float, df : Point1D => DenseVector[Float]) extends ContinuousScalarImageLike[Point1D,Vector1D] {
  val pixelDimensionality = 1  
  def apply(x : Point1D) = f(x)
  def takeDerivative(x : Point1D) = df(x)
}


case class DiscreteScalarImage1D(val domain : DiscreteImageDomain1D, val values : IndexedSeq[Float]) extends DiscreteImageLike[Point1D, Vector1D, Float]{
  def apply(i : Int) = values(i)
}

case class ContinousVectorImage1D(val pixelDimensionality : Int, val domain : ImageDomain1D, f : Point1D => DenseVector[Float], df : Point1D => DenseMatrix[Float]) extends ContinuousVectorImageLike[Point1D, Vector1D] {
  def apply(x : Point1D) = f(x)
  def takeDerivative(x : Point1D) = df(x)
}


object ContinuousImage {

  //  type Interpolator[Domain, Pixel] = DiscreteImage[Domain, Pixel] => ContinuousImage[Domain, Pixel]
  //  type Resampler[Domain, Pixel] = DiscreteImage[Domain, Pixel] => DiscreteImage[Domain, Pixel]
}
