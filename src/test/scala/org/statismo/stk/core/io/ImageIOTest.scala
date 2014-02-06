package org.statismo.stk.core.io

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import org.statismo.stk.core.image.Interpolation._
import org.statismo.stk.core.image._
import org.statismo.stk.core.geometry._
import org.statismo.stk.core.geometry.implicits._
import java.io.File
import scala.util.Success
import scala.util.Failure
import org.statismo.stk.core.numerics.UniformDistributionRandomSampler3D

class ImageIOTest extends FunSpec with ShouldMatchers {

  org.statismo.stk.core.initialize()

  describe("A 1D scalar image") {
    it("can be stored and read again") {
      val domain = DiscreteImageDomain1D(0, 0.02, 50)
      val values = domain.points.map(x => math.sin(2 * math.Pi * x)).map(_.toFloat).toArray
      val discreteImage = DiscreteScalarImage1D[Float](domain, values)

      val tmpImgFile = File.createTempFile("image1D", ".h5")

      ImageIO.writeImage(discreteImage, tmpImgFile)
      val restoredDiscreteImgOrFailure = ImageIO.read1DScalarImage[Float](tmpImgFile)

      restoredDiscreteImgOrFailure.isSuccess should be(true)
      discreteImage should equal(restoredDiscreteImgOrFailure.get)

      tmpImgFile.delete()
    }
  }

  describe("A 2D scalar image") {
    it("can be converted to vtk and back and yields the same image") {
      val path = getClass().getResource("/lena.h5").getPath()
      val lena = ImageIO.read2DScalarImage[Short](new File(path)).get
      val tmpImgFile = File.createTempFile("image2D", ".vtk")
      ImageIO.writeImage(lena, tmpImgFile) match {
        case Failure(ex) => throw new Exception(ex)
        case Success(_) => {}
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

      ImageIO.writeImage(discreteImage, tmpImgFile)
      val restoredDiscreteImgOrFailure = ImageIO.read2DScalarImage[Float](tmpImgFile)

      restoredDiscreteImgOrFailure.isSuccess should be(true)
      discreteImage should equal(restoredDiscreteImgOrFailure.get)

      tmpImgFile.delete()
    }
  }

  describe("A 3D scalar image") {
    it("Can be read and written again") {
      val path = getClass().getResource("/3dimage.h5").getPath()

      val discreteImage = ImageIO.read3DScalarImage[Short](new File(path)).get
      //   Utils.show3D[Short](discreteImage)

      val f = File.createTempFile("dummy", ".h5")
      f.deleteOnExit()
      val t = ImageIO.writeImage(discreteImage, f)

      assert(t.isSuccess == true)
      f.delete()
    }

 
    it("can write nifti and read it again") {

      val pathH5 = getClass().getResource("/3dimage.nii").getPath()
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