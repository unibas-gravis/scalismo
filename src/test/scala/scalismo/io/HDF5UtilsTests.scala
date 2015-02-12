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
package scalismo.io

import org.scalatest.{Matchers, FunSpec}
import org.scalatest.matchers.ShouldMatchers
import java.io.File

/**
 * Created by luethi on 2/19/14.
 */
class HDF5UtilsTests extends FunSpec with Matchers {

  scalismo.initialize()

  def createTmpH5File() : File = {
    val f= File.createTempFile("hdf5file", ".h5")
    f.deleteOnExit()
    f
  }

  describe("The hdf5 utils") {
    it("can write and read an int") {

      val h5file =  createTmpH5File()
      val h5 = HDF5Utils.createFile(h5file).get
      val anInt = 5
      h5.writeInt("/agroup/anInt", anInt)
      val h5new = HDF5Utils.openFileForReading(h5file).get
      h5new.readInt("/agroup/anInt").get should be(anInt)
    }
  }
}