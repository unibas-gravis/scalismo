package org.statismo.stk.core.io

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import java.io.File

/**
 * Created by luethi on 2/19/14.
 */
class HDF5UtilsTests extends FunSpec with ShouldMatchers {

  org.statismo.stk.core.initialize()

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