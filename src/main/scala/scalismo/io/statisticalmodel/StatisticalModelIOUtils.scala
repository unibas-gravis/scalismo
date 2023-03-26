/*
 * Copyright 2023 University of Basel, Graphics and Vision Research Group
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
package scalismo.io.statisticalmodel

import scalismo.hdf5json.HDF5Json

import java.io.{File, IOException}
import scala.util.Try

// make it a proper class and wrapper around the object
object StatisticalModelIOUtils {

  // map untyped FileAccessModel of HDF5 (which is just a string)
  // to typed values
  object FileAccessMode extends Enumeration {
    type FileAccessMode = Value
    val WRITE, CREATE = Value
  }
  import FileAccessMode._

  def hdf5Version = "to be defined"

  private def openFileWriterOrCreateFile(file: File, mode: FileAccessMode): Try[HDF5Writer] = Try {
    if (mode == WRITE && file.exists()) {
      new HDF5Writer(file, Some(HDF5Json.readFromFile(file).get))
    } else {
      new HDF5Writer(file)
    }

  }

  def openFileForReading(file: File): Try[StatisticalModelReader] = Try {

    if (file.getName.endsWith(".h5")) {
      val hdfFile = new io.jhdf.HdfFile(file)
      new HDF5Reader(hdfFile)
    } else if (file.getName.endsWith(".json")) {
      HDF5Json.readFromFile(file).map(hdfjson => HDF5JsonReader(hdfjson)).get
    } else {
      throw new IOException(s"File ${file.getName} is not a valid HDF5 or JSON file")
    }
  }

  def openFileForWriting(file: File): Try[HDF5Writer] = openFileWriterOrCreateFile(file, WRITE)
  def createFile(file: File): Try[HDF5Writer] = openFileWriterOrCreateFile(file, CREATE)

}
