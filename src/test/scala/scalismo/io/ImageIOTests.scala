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

import java.io.File

import breeze.linalg.{ DenseMatrix, DenseVector }
import niftijio.NiftiVolume
import org.scalatest.{ FunSpec, Matchers }
import scalismo.common.Scalar
import scalismo.geometry._
import scalismo.image.{ DiscreteImageDomain, DiscreteScalarImage }
import scalismo.utils.ImageConversion.CanConvertToVtk
import spire.math.{ ULong, UInt, UShort, UByte }

import scala.reflect.ClassTag
import scala.reflect.runtime.universe.{ TypeTag, typeOf }
import scala.util.{ Try, Failure, Success }

class ImageIOTests extends FunSpec with Matchers {

  scalismo.initialize()

  def equalImages(img1: DiscreteScalarImage[_3D, _], img2: DiscreteScalarImage[_3D, _]): Boolean = {

    val valFlag = (0 until img1.values.size by img1.values.size / 1000).forall { i =>
      img1.data(i) == img2.data(i)
    }

    valFlag && ((img1.domain.origin - img2.domain.origin).norm < 0.01f) &&
      ((img1.domain.spacing - img2.domain.spacing).norm < 0.01f) && (img1.domain.size == img2.domain.size)
  }

  describe("A 1D scalar image") {
    it("can be stored and read again") {
      import scalismo.common.ScalarArray.implicits._
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

  private class DataReadWrite[D <: Dim: NDSpace: CanConvertToVtk] {

    val dim = implicitly[NDSpace[D]].dimensionality

    def typeAsString[T: TypeTag](): String = {
      typeOf[T] match {
        case t if t =:= typeOf[Byte] => "char"
        case t if t =:= typeOf[Short] => "short"
        case t if t =:= typeOf[Int] => "int"
        case t if t =:= typeOf[Long] => "long"
        case t if t =:= typeOf[Float] => "float"
        case t if t =:= typeOf[Double] => "double"
        case t if t =:= typeOf[UByte] => "uchar"
        case t if t =:= typeOf[UShort] => "ushort"
        case t if t =:= typeOf[UInt] => "uint"
        case t if t =:= typeOf[ULong] => "ulong"
        case _ => throw new NotImplementedError("" + typeOf[T])
      }
    }

    def readImage[T: Scalar: TypeTag: ClassTag](f: File): Try[DiscreteScalarImage[D, T]] = {
      val r = if (dim == 2) ImageIO.read2DScalarImage[T](f) else ImageIO.read3DScalarImage[T](f)
      r.asInstanceOf[Try[DiscreteScalarImage[D, T]]]
    }

    def testReadWrite[T: Scalar: TypeTag: ClassTag]() = {
      val path = getClass.getResource("/images/vtk").getPath
      val source = new File(s"$path/${dim}d_${typeAsString[T]()}.vtk")

      // read
      val read = readImage[T](source)
      read match {
        case Failure(e) => e.printStackTrace()
        case Success(img) =>
          val doubles = img.data.map(v => implicitly[Scalar[T]].toDouble(v)).iterator.toArray
          (doubles.length, doubles.min, doubles.max) should equal((8, 42.0, 49.0))
        //println("vtk " + typeOf[T] + " " + dim+ " " + img.data.getClass + " " + img.data.deep)

      }
      read should be a 'Success

      // write out, and read again
      val vtk = File.createTempFile("imageio", ".vtk")
      vtk.deleteOnExit()
      ImageIO.writeVTK[D, T](read.get, vtk) should be a 'Success

      val reread = readImage[T](vtk)
      reread match {
        case Failure(e) => e.printStackTrace()
        case Success(img) =>
          val doubles = img.data.map(v => implicitly[Scalar[T]].toDouble(v)).iterator.toArray
          (doubles.length, doubles.min, doubles.max) should equal((8, 42.0, 49.0))
        //println("vtk " + typeOf[T] + " " + dim+ " " + img.data.getClass + " " + img.data.deep)
      }
      reread should be a 'Success
      vtk.delete()

      // if in 3D, write out as nifti and read again
      if (dim == 3) {
        val nii = File.createTempFile("imageio", ".nii")
        nii.deleteOnExit()
        ImageIO.writeNifti(read.get.asInstanceOf[DiscreteScalarImage[_3D, T]], nii) should be a 'Success
        val reread = ImageIO.read3DScalarImage[T](nii)
        reread match {
          case Failure(e) => e.printStackTrace()
          case Success(img) =>
            val doubles = img.data.map(v => implicitly[Scalar[T]].toDouble(v)).iterator.toArray
            (doubles.length, doubles.min, doubles.max) should equal((8, 42.0, 49.0))
          //println("nii " + typeOf[T] + " " + dim+ " " + img.data.getClass + " " + img.data.deep)
        }
        nii.delete()
      }
    }

    def run() = {
      testReadWrite[Short]()
      testReadWrite[Int]()
      testReadWrite[Long]()
      testReadWrite[Float]()
      testReadWrite[Double]()
      testReadWrite[Byte]()
      testReadWrite[UByte]()
      testReadWrite[UShort]()
      testReadWrite[UInt]()
      testReadWrite[ULong]()
    }
  }

  describe("A 2D scalar image") {
    it("can be read and written in various signed and unsigned VTK and Nifti data formats") {
      new DataReadWrite[_2D]().run()
    }

    it("can be converted to vtk and back and yields the same image") {
      val path = getClass.getResource("/lena.vtk").getPath
      val lena = ImageIO.read2DScalarImage[Short](new File(path)).get
      val tmpImgFile = File.createTempFile("image2D", ".vtk")
      tmpImgFile.deleteOnExit()
      ImageIO.writeVTK(lena, tmpImgFile) match {
        case Failure(ex) => throw new Exception(ex)
        case Success(_) =>
      }
      val lenaFromVTK = ImageIO.read2DScalarImage[Short](tmpImgFile).get
      lena should equal(lenaFromVTK)
      tmpImgFile.delete()
    }
  }

  describe("A 3D scalar image") {

    it("can be read and written in various signed and unsigned VTK and Nifti data formats") {
      new DataReadWrite[_3D]().run()
    }

    it("can be stored to VTK and re-read in right precision") {
      import scalismo.common.ScalarArray.implicits._
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
      val path = getClass.getResource("/3dimage.nii").getPath
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

        (origImg.domain.origin - rereadImg.domain.origin).norm should be(0.0 +- 1e-2)

        (origImg.domain.spacing - rereadImg.domain.spacing).norm should be(0.0 +- 1e-2)
        origImg.domain.size should equal(rereadImg.domain.size)
        for (i <- 0 until origImg.values.size by origImg.values.size / 1000) {
          origImg(i) should equal(rereadImg(i))
        }
      }
    }
  }

}