package org.statismo.stk.core.geometry

import org.statismo.stk.core.io.LandmarkIO
import org.statismo.stk.core.statisticalmodel.NDimensionalNormalDistribution

/* It's explicitly forbidden to extend this class (which is possible, but ugly anyway),
 * to force "extensions" to use composition instead of subclassing.
 *
 * The primary reason is to force implementations to provide the correct extension encode/decode functions when using
 * the LandmarkIO methods for reading and writing JSON representations of landmarks.
 */
final case class Landmark[D <: Dim : DimOps](point: Point[D], name: String, description: Option[String] = None, uncertainty: Option[NDimensionalNormalDistribution[D]] = None)

object Landmark {
  import scala.language.implicitConversions
  implicit def legacyTupleAsLandmark[D <: Dim: DimOps](tuple: (String, Point[D])) : Landmark[D] = Landmark(tuple._2, tuple._1)

  implicit def extensionEncodeFunction[D <: Dim: DimOps]: LandmarkIO.ExtensionEncodeFunction[D, Landmark[D]] = { lm => (lm, None) }
  implicit def extensionDecodeFunction[D <: Dim: DimOps]: LandmarkIO.ExtensionDecodeFunction[D, Landmark[D]] = { case (lm, _) => lm }
}