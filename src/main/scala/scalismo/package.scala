import org.statismo.support.nativelibs._

package object scalismo {

  // this is a hacky way to get an object that can be synchronized on, with a mutable value.
  private val initialized = Array.fill(1)(false)

  /**
   * Initialize and load the required native libraries
   * @param ignoreErrors ignore failures when trying to load libraries. Only set this if you know what you are doing!
   */
  def initialize(ignoreErrors: Boolean = false) = initialized.synchronized {
    if (!initialized(0)) {
      val mode = if (ignoreErrors) NativeLibraryBundles.InitializationMode.WARN_ON_FAIL else NativeLibraryBundles.InitializationMode.TERMINATE_ON_FAIL
      NativeLibraryBundles.initialize(mode)
      initialized(0) = true
    }
  }
}
