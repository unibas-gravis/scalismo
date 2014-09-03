package org.statismo.stk.core.io

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import org.statismo.stk.core.image._
import org.statismo.stk.core.geometry.implicits._
import java.io.File
import scala.util.Success
import scala.util.Failure
import niftijio.NiftiVolume
import breeze.linalg.DenseMatrix

class ImageIOTests extends FunSpec with ShouldMatchers {

  org.statismo.stk.core.initialize()

  describe("A 1D scalar image") {
    it("can be stored and read again") {
      val domain = DiscreteImageDomain1D(0, 0.02, 50)
      val values = domain.points.map(x => math.sin(2 * math.Pi * x)).map(_.toFloat).toArray
      val discreteImage = DiscreteScalarImage1D[Float](domain, values)

      val tmpImgFile = File.createTempFile("image1D", ".h5")
      tmpImgFile.deleteOnExit()

      ImageIO.writeImage(discreteImage, tmpImgFile)
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
      ImageIO.writeImage(lena, tmpImgFile) match {
        case Failure(ex) => throw new Exception(ex)
        case Success(_) =>
      }
      val lenaFromVTK = ImageIO.read2DScalarImage[Short](tmpImgFile).get
      lena should equal(lenaFromVTK)
    }
  }

  describe("A 2D vector image") {
    it("can be stored and read again") {
      val domain = DiscreteImageDomain2D((1.0, 0.0), (0.5, 1.0), (2, 3))
      val discreteImage = DiscreteScalarImage2D[Float](domain, Array(1.4f, 2.1f, 7.5f, 9f, 8f, 0f))

      val tmpImgFile = File.createTempFile("image2D", ".h5")
      tmpImgFile.deleteOnExit()

      ImageIO.writeImage(discreteImage, tmpImgFile)
      val restoredDiscreteImgOrFailure = ImageIO.read2DScalarImage[Float](tmpImgFile)

      restoredDiscreteImgOrFailure.isSuccess should be(right = true)
      discreteImage should equal(restoredDiscreteImgOrFailure.get)

      tmpImgFile.delete()
    }
  }

  describe("A 3D scalar image") {
    it("can be read and written again") {
      val path = getClass.getResource("/3dimage.h5").getPath

      val discreteImage = ImageIO.read3DScalarImage[Short](new File(path)).get
      //   Utils.show3D[Short](discreteImage)

      val f = File.createTempFile("dummy", ".h5")
      f.deleteOnExit()
      val t = ImageIO.writeImage(discreteImage, f)

      assert(t.isSuccess)
      f.delete()
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

        ImageIO.writeImage(origImg, tmpfile)
        val rereadImg = ImageIO.read3DScalarImage[Short](tmpfile).get
        origImg.domain.origin should equal(rereadImg.domain.origin)
        (origImg.domain.spacing - rereadImg.domain.spacing).norm should be(0.0 plusOrMinus 1e-2)
        origImg.domain.size should equal(rereadImg.domain.size)
        for (i <- 0 until origImg.values.size by origImg.values.size / 1000) {
          origImg.values(i) should equal(rereadImg.values(i))
        }
      }
    }
  }

}