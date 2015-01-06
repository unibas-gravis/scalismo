package org.statismo.stk.core
package image
import org.statismo.stk.core.geometry._
import scala.reflect.ClassTag
import spire.math.Numeric

object Resample {


  def sample[D <: Dim : DiscreteScalarImage.Create, @specialized(Short, Float, Double) Pixel: Numeric: ClassTag](img: ContinuousScalarImage[D], domain: DiscreteImageDomain[D], outsideValue: Double): DiscreteScalarImage[D, Pixel] = {
    val numeric = implicitly[Numeric[Pixel]]
    val sampledValues = domain.points.toIndexedSeq.par.map((pt: Point[D]) => {
      if (img.isDefinedAt(pt)) numeric.fromDouble(img(pt))
      else numeric.fromDouble(outsideValue)
    })


    implicitly[DiscreteScalarImage.Create[D]].createDiscreteScalarImage(domain, sampledValues.toArray)
  }

}

