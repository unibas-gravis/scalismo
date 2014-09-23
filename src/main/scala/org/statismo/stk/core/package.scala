package org.statismo.stk

import org.statismo.support.nativelibs._

package object core {

  private var initialized = false

  /**
   * Initialize and load the required native libraries
   * @param ignoreErrors ignore failures when trying to load libraries. Only set this if you know what you are doing!
   */
  def initialize(ignoreErrors: Boolean = false) = {
    if (!initialized) {
      val mode = if (ignoreErrors) NativeLibraryBundles.InitializationMode.WARN_ON_FAIL else NativeLibraryBundles.InitializationMode.TERMINATE_ON_FAIL
      NativeLibraryBundles.initialize(mode)
      initialized = true
    }
  }
}
