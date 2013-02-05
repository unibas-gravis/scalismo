package smptk.image
import Image._

object Resample {
	def sample2D[Pixel : ScalarPixel](img : ContinuousScalarImage2D, domain : DiscreteImageDomain2D) : DiscreteScalarImage2D[Pixel]  = { 
	val scalarPixel = implicitly[ScalarPixel[Pixel]]
	  val sampledValues = domain.points.map(pt => scalarPixel.fromFloat(img(pt)))
	  DiscreteScalarImage2D(domain, sampledValues)
	}
}