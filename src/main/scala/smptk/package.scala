import ch.unibas.gravis.nativelib.NativeLibraryBundles
package object smptk {

  var initialized = false

  def initialize(): Unit = {
    if (initialized == false) {
      try { // TODO exception should not be caught. This is only until the classloader stuff is fixed in NativeLib 
        NativeLibraryBundles.initialize(NativeLibraryBundles.InitializationMode.WARNONFAIL)
      } catch {
        case e : Exception => println(e.getMessage())
      }
      initialized = true
    }

  }
}