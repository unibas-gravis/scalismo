package scalismo.statisticalmodel

/**
 * Strategies for working with NaNValues.
 */
sealed trait NaNStrategy

object NaNStrategy {

  /**
   * No special treatment of NaN is performed.
   */
  case object NanIsNumericValue extends NaNStrategy

  /**
   * NaN values are treated as missing values. This is for example useful in regression computations.
   */
  case object NaNAsMissingValue extends NaNStrategy

}
