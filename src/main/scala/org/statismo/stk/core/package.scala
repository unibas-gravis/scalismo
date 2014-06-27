package org.statismo.stk

import org.statismo.support.nativelibs._

package object core {

  private var initialized = false

  def initialize(): Unit = {
    if (initialized == false) {
      NativeLibraryBundles.initialize(NativeLibraryBundles.InitializationMode.WARNONFAIL)
      initialized = true
    }
  }
}
