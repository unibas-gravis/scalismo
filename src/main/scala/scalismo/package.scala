/*
 * Copyright 2015 University of Basel, Graphics and Vision Research Group
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

import java.util.concurrent.TimeUnit

import scalismo.support.nativelibs._
import vtk.vtkObjectBase

package object scalismo {

  // this is a hacky way to get an object that can be synchronized on, with a mutable value.
  private val initialized = Array.fill(1)(false)

  /**
   * Initialize and load the required native libraries
   *
   * @param ignoreErrors ignore failures when trying to load libraries. Only set this if you know what you are doing!
   */
  def initialize(ignoreErrors: Boolean = false) = initialized.synchronized {
    if (!initialized(0)) {
      val mode = if (ignoreErrors) InitializationMode.WARN_ON_FAIL else InitializationMode.TERMINATE_ON_FAIL
      NativeLibraryBundles.initialize(mode)

      val gc = vtkObjectBase.JAVA_OBJECT_MANAGER.getAutoGarbageCollector
      gc.SetScheduleTime(60, TimeUnit.SECONDS)
      gc.Start()

      // we need to stop the garbage collection when the JVM shutdowns.
      // Otherwise it prevents the JVM from shutting down correctly.
      Runtime.getRuntime().addShutdownHook(new Thread { gc.Stop() })

      initialized(0) = true
    }
  }
}
