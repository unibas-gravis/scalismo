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

import scalismo.ScalismoTestSuite
import scalismo.hdf5json.HDFPath
import scalismo.io.statisticalmodel.{HDF5Writer, NDArray, StatisticalModelIOUtils}

import java.io.File
import scala.util.Try

class HDF5UtilsTests extends ScalismoTestSuite {

  def createTmpH5File(): File = {
    val f = File.createTempFile("hdf5file", ".h5.json")
    f.deleteOnExit()
    f
  }

  describe("The hdf5 utils") {
    it("can write and read an int") {

      val h5file = createTmpH5File()
      val h5 = StatisticalModelIOUtils.createFile(h5file).get
      val anInt = 5
      h5.writeInt(HDFPath("/agroup/anInt"), anInt)
      h5.write()
      val h5new = StatisticalModelIOUtils.openFileForReading(h5file).get
      h5new.readInt(HDFPath("/agroup/anInt")).get should be(anInt)
    }

    it("can write and read an NDArray[Int]") {
      val h5file = createTmpH5File()
      val h5: HDF5Writer = StatisticalModelIOUtils.createFile(h5file).get
      val arr = NDArray(IndexedSeq(2, 3), Array(1, 2, 3, 4, 5, 6))
      h5.writeNDArray(HDFPath("/aGroup/array"), arr).get
      h5.write()
      val h5new = StatisticalModelIOUtils.openFileForReading(h5file).get
      val dataAsArray = h5new.readNDArrayInt(HDFPath("/aGroup/array")).get.data
      dataAsArray.sameElements(arr.data) should be(true)
    }

    it("can write and read an NDArray[Float]") {
      val h5file = createTmpH5File()
      val h5: HDF5Writer = StatisticalModelIOUtils.createFile(h5file).get
      val arr = NDArray(IndexedSeq(2, 3), Array(1f, 2f, 3f, 4f, 5f, 6f))
      h5.writeNDArray(HDFPath("/aGroup/array"), arr).get
      h5.write()
      val h5new = StatisticalModelIOUtils.openFileForReading(h5file).get
      val dataAsArray = h5new.readNDArrayFloat(HDFPath("/aGroup/array")).get.data
      dataAsArray.sameElements(arr.data) should be(true)
    }
  }
}
