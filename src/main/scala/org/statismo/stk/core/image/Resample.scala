package org.statismo.stk.core
package image
import org.statismo.stk.core.geometry._
import scala.reflect.ClassTag
import scala.reflect.ClassTag
import scala.reflect.runtime.universe._
import org.statismo.stk.core.common.ScalarValue

object Resample {


  def sample[@specialized(Short, Float, Double) Pixel: ScalarValue: ClassTag](img: ContinuousScalarImage3D, domain: DiscreteImageDomain3D, outsideValue: Double): DiscreteScalarImage3D[Pixel] = {    
    val ScalarValue = implicitly[ScalarValue[Pixel]]
    val sampledValues = domain.points.par.map((pt: Point[_3D]) => {
      if (img.isDefinedAt(pt)) ScalarValue.fromDouble(img(pt))
      else ScalarValue.fromDouble(outsideValue)
    })

    DiscreteScalarImage3D(domain, sampledValues.toArray)
  }

  def sample[@specialized(Short, Float, Double) Pixel: ScalarValue: ClassTag](img: ContinuousScalarImage2D, domain: DiscreteImageDomain2D, outsideValue: Double): DiscreteScalarImage2D[Pixel] = {
    val ScalarValue = implicitly[ScalarValue[Pixel]]
    val sampledValues = domain.points.par.map((pt: Point[_2D]) => {
      if (img.isDefinedAt(pt)) ScalarValue.fromDouble(img(pt))
      else ScalarValue.fromDouble(outsideValue)
    })

    DiscreteScalarImage2D(domain, sampledValues.toArray)
  }

  def sample[@specialized(Short, Float, Double) Pixel: ScalarValue: ClassTag](img: ContinuousScalarImage1D, domain: DiscreteImageDomain1D, outsideValue: Double): DiscreteScalarImage1D[Pixel] = {
    val ScalarValue = implicitly[ScalarValue[Pixel]]
    val sampledValues = domain.points.map((pt: Point[_1D]) => {
      if (img.isDefinedAt(pt)) ScalarValue.fromDouble(img(pt))
      else ScalarValue.fromDouble(outsideValue)
    })
    DiscreteScalarImage1D(domain, sampledValues.toArray)
  }
}

