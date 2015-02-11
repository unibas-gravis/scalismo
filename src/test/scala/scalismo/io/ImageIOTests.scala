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

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import java.io.File
import scalismo.image.{DiscreteImageDomain, DiscreteScalarImage, DiscreteImageDomain3D}
import scalismo.geometry._
import scala.util.Success
import scala.util.Failure
import niftijio.NiftiVolume
import breeze.linalg.DenseMatrix
import breeze.linalg.DenseVector
import org.apache.commons.math3.exception.ZeroException

class ImageIOTests extends FunSpec with ShouldMatchers {

  scalismo.initialize()

  def equalImages(img1: DiscreteScalarImage[_3D, _], img2: DiscreteScalarImage[_3D, _]): Boolean = {

    val valFlag = (0 until img1.values.size by img1.values.size / 1000).forall { i =>
      img1.data(i) == img2.data(i)
    }

    valFlag && ((img1.domain.origin - img2.domain.origin).norm < 0.01f) &&
      ((img1.domain.spacing - img2.domain.spacing).norm < 0.01f) && ((img1.domain.size == img2.domain.size))
  }

  describe("A 1D scalar image") {
    it("can be stored and read again") {
      val domain = DiscreteImageDomain[_1D](Point(0), Vector(0.02f), Index(50))
      val values = domain.points.map(x => math.sin(2 * math.Pi * x(0))).map(_.toFloat).toArray
      val discreteImage = DiscreteScalarImage[_1D, Float](domain, values)

      val tmpImgFile = File.createTempFile("image1D", ".h5")
      tmpImgFile.deleteOnExit()

      ImageIO.writeHDF5(discreteImage, tmpImgFile)
      val restoredDiscreteImgOrFailure = ImageIO.read1DScalarImage[Float](tmpImgFile)

      restoredDiscreteImgOrFailure.isSuccess should equal(true)
      discreteImage should equal(restoredDiscreteImgOrFailure.get)

      tmpImgFile.delete()
    }
  }

  describe("A 2D scalar image") {
    it("can be converted to vtk and back and yields the same image") {
      val path = getClass.getResource("/lena.h5").getPath
      val lena = ImageIO.read2DScalarImage[Short](new File(path)).get
      val tmpImgFile = File.createTempFile("image2D", ".vtk")
      ImageIO.writeVTK(lena, tmpImgFile) match {
        case Failure(ex) => throw new Exception(ex)
        case Success(_) =>
      }
      val lenaFromVTK = ImageIO.read2DScalarImage[Short](tmpImgFile).get
      lena should equal(lenaFromVTK)
    }
  }

  describe("A 3D scalar image") {

    it("can be stored to VTK and re-read in right precision") {
      val domain = DiscreteImageDomain[_3D](Point(-72.85742f, -72.85742f, -273.0f), Vector(0.85546875f, 0.85546875f, 1.5f), Index(15, 15, 15))
      val values = DenseVector.zeros[Short](15 * 15 * 15).data
      val discreteImage = DiscreteScalarImage(domain, values)
      val f = File.createTempFile("dummy", ".vtk")
      f.deleteOnExit()
      ImageIO.writeVTK(discreteImage, f)
      val readImg = ImageIO.read3DScalarImage[Short](f).get

      readImg.data should equal(discreteImage.data)

      assert(equalImages(readImg, discreteImage))

    }

    it("can be converted to vtk and back and yields the same image") {
      val path = getClass.getResource("/3dimage.h5").getPath
      val discreteImage = ImageIO.read3DScalarImage[Short](new File(path)).get
      val f = File.createTempFile("dummy", ".vtk")
      f.deleteOnExit()
      ImageIO.writeVTK(discreteImage, f)
      val readImg = ImageIO.read3DScalarImage[Short](f).get
      assert(equalImages(readImg, discreteImage))
    }

    describe("in Nifti format") {

      it("returns the same data as the niftijio reader when using FastReadOnlyNiftiVolume") {
        val filename = getClass.getResource("/3dimage.nii").getPath
        val o = NiftiVolume.read(filename)
        val n = FastReadOnlyNiftiVolume.read(filename).get

        for (i <- 0 until 8) {
          n.header.dim(i) should equal(o.header.dim(i))
          n.header.pixdim(i) should equal(o.header.pixdim(i))
        }

        n.header.vox_offset should equal(o.header.vox_offset)
        n.header.scl_inter should equal(o.header.scl_inter)
        n.header.scl_slope should equal(o.header.scl_slope)
        n.header.sform_code should equal(o.header.sform_code)

        val om = DenseMatrix.create(4, 4, o.header.sform_to_mat44().flatten)
        val nm = DenseMatrix.create(4, 4, n.header.sformArray)

        val oq = o.header.qform_to_mat44()
        val nq = n.header.qform_to_mat44

        oq.deep should equal(nq.deep)
        om.toString() should equal(nm.toString())

        val oh = {
          val nx = o.header.dim(1)
          val ny = o.header.dim(2)
          val nz = o.header.dim(3)
          var dim = o.header.dim(4)

          if (dim == 0)
            dim = 1
          val data = for (d <- 0 until dim; k <- 0 until nz; j <- 0 until ny; i <- 0 until nx) yield o.data(i)(j)(k)(d)
          data.hashCode()
        }
        val nh = n.dataArray.deep.hashCode()
        nh should equal(oh)
      }

      it("can be written and read again") {
        val pathH5 = getClass.getResource("/3dimage.nii").getPath
        val origImg = ImageIO.read3DScalarImage[Short](new File(pathH5)).get
        val tmpfile = File.createTempFile("dummy", ".nii")
        tmpfile.deleteOnExit()

        ImageIO.writeNifti(origImg, tmpfile).get

        val rereadImg = ImageIO.read3DScalarImage[Short](tmpfile).get

        (origImg.domain.origin - (rereadImg.domain.origin)).norm should be(0.0 plusOrMinus 1e-2)

        (origImg.domain.spacing - rereadImg.domain.spacing).norm should be(0.0 plusOrMinus 1e-2)
        origImg.domain.size should equal(rereadImg.domain.size)
        for (i <- 0 until origImg.values.size by origImg.values.size / 1000) {
          origImg(i) should equal(rereadImg(i))
        }
      }
    }
  }

}