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

import javax.swing.SwingUtilities
import scalismo.support.nativelibs._
import vtk.vtkObjectBase

package object scalismo {

  // this is a hacky way to get an object that can be synchronized on, with a mutable value.
  private val initialized = Array.fill(1)(false)

  /**
   * Initialize and load the required native libraries
   *
   * @param ignoreErrors ignore failures when trying to load libraries. Only set this if you know what you are doing!
   * @param gcInterval time interval (in milliseconds) for running the vtk garbage collection.
   *                   A value <= 0 means that garbage collection is not run automatically.
   */
  def initialize(ignoreErrors: Boolean = false, gcInterval: Long = 60 * 1000) = initialized.synchronized {
    if (!initialized(0)) {
      val mode = if (ignoreErrors) InitializationMode.WARN_ON_FAIL else InitializationMode.TERMINATE_ON_FAIL
      NativeLibraryBundles.initialize(mode)

      if (gcInterval > 0) {
        setupVTKGCThread(gcInterval)
      }
      initialized(0) = true
    }

  }

  private def setupVTKGCThread(gcInterval: Long): Unit = {

    val runGC = new Runnable() {
      override def run() {
        vtkObjectBase.JAVA_OBJECT_MANAGER.gc(false)
      }
    }

    val gcThread = new Thread {
      override def run() {
        while (true) {
          Thread.sleep(gcInterval)

          // As vtk is very sensitive to threading issues, we run the gc on the EDT thread.
          SwingUtilities.invokeLater(runGC);

        }
      }
    }

    gcThread.setDaemon(true)
    gcThread.start()
  }

}
