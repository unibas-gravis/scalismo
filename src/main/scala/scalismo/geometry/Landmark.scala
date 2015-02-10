package scalismo.geometry

import scalismo.io.LandmarkIO
import scalismo.statisticalmodel.NDimensionalNormalDistribution

/* It's explicitly forbidden to extend this class (which is possible, but ugly anyway),
 * to force "extensions" to use composition instead of subclassing.
 *
 * The primary reason is to force implementations to provide the correct extension encode/decode functions when using
 * the LandmarkIO methods for reading and writing JSON representations of landmarks.
 */
final case class Landmark[D <: Dim : NDSpace](id: String, point: Point[D], description: Option[String] = None, uncertainty: Option[NDimensionalNormalDistribution[D]] = None)

object Landmark {

  import scala.language.implicitConversions

  implicit def noExtensionsEncodeFunction[D <: Dim : NDSpace]: LandmarkIO.ExtensionEncodeFunction[D, Landmark[D]] = { lm => (lm, None) }

  implicit def noExtensionsDecodeFunction[D <: Dim : NDSpace]: LandmarkIO.ExtensionDecodeFunction[D, Landmark[D]] = {
    case (lm, _) => lm
  }
}