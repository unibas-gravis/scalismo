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
import scalismo.ScalismoTestSuite
import scalismo.common.{ PointId, Scalar, ScalarArray }
import scalismo.geometry._
import scalismo.image.{ DiscreteImageDomain, DiscreteScalarImage }
import scalismo.utils.CanConvertToVtk
import spire.math.{ UByte, UInt, UShort }

import scala.reflect.ClassTag
import scala.reflect.runtime.universe.{ TypeTag, typeOf }
import scala.util.{ Failure, Success, Try }

class ImageIOTests extends ScalismoTestSuite {

  def equalImages(img1: DiscreteScalarImage[_3D, _], img2: DiscreteScalarImage[_3D, _]): Boolean = {

    val valFlag = (0 until img1.values.size by img1.values.size / 1000).forall { i =>
      img1.data(i) == img2.data(i)
    }

    valFlag && ((img1.domain.origin - img2.domain.origin).norm < 0.01f) &&
      ((img1.domain.spacing - img2.domain.spacing).norm < 0.01f) && (img1.domain.size == img2.domain.size)
  }

  private class DataReadWrite[D: NDSpace: CanConvertToVtk] {

    val dim = implicitly[NDSpace[D]].dimensionality

    def typeAsString[T: TypeTag](): String = {
      typeOf[T] match {
        case t if t =:= typeOf[Byte] => "char"
        case t if t =:= typeOf[Short] => "short"
        case t if t =:= typeOf[Int] => "int"
        case t if t =:= typeOf[Float] => "float"
        case t if t =:= typeOf[Double] => "double"
        case t if t =:= typeOf[UByte] => "uchar"
        case t if t =:= typeOf[UShort] => "ushort"
        case t if t =:= typeOf[UInt] => "uint"
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
      testReadWrite[Float]()
      testReadWrite[Double]()
      testReadWrite[Byte]()
      testReadWrite[UByte]()
      testReadWrite[UShort]()
      testReadWrite[UInt]()
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
      val domain = DiscreteImageDomain[_3D](Point(-72.85742f, -72.85742f, -273.0f), EuclideanVector(0.85546875f, 0.85546875f, 1.5f), IntVector(15, 15, 15))
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
          val data = for (d <- 0 until dim; k <- 0 until nz; j <- 0 until ny; i <- 0 until nx) yield o.data.get(i, j, k, d)
          data.hashCode()
        }
        val nh = n.dataAsScalarArray[Short].map[Double](_.toDouble).iterator.toArray.deep.hashCode()
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
          origImg(PointId(i)) should equal(rereadImg(PointId(i)))
        }
      }
    }
  }

  describe("ImageIO") {
    it("is type safe") {

      case class ImageWithType[D: NDSpace: CanConvertToVtk, T: Scalar: TypeTag: ClassTag](img: DiscreteScalarImage[D, T], typeName: String) {
        def writeVtk(file: File) = ImageIO.writeVTK(img, file)
        def writeNii(file: File) = {
          if (implicitly[NDSpace[D]].dimensionality == 3) ImageIO.writeNifti(img.asInstanceOf[DiscreteScalarImage[_3D, T]], file)
          else Failure(new NotImplementedError)
        }
      }

      def convertTo[D: NDSpace: CanConvertToVtk, OUT: Scalar: TypeTag: ClassTag](in: DiscreteScalarImage[D, Int]): ImageWithType[D, OUT] = {
        val img = in.map(implicitly[Scalar[OUT]].fromInt)
        ImageWithType(img, ImageIO.ScalarType.fromType[OUT].toString)
      }

      val data = (1 to 8).toArray
      val dom2 = DiscreteImageDomain(Point(0, 0), EuclideanVector(1, 1), IntVector(2, 2))
      val img2 = DiscreteScalarImage(dom2, ScalarArray(data.take(4)))
      val dom3 = DiscreteImageDomain(Point(0, 0, 0), EuclideanVector(1, 1, 1), IntVector(2, 2, 2))
      val img3 = DiscreteScalarImage(dom3, ScalarArray(data))

      def imageSeq[D: NDSpace: CanConvertToVtk](img: DiscreteScalarImage[D, Int]) = Seq(
        convertTo[D, Byte](img),
        convertTo[D, Short](img),
        convertTo[D, Int](img),
        convertTo[D, Double](img),
        convertTo[D, Float](img),
        convertTo[D, UByte](img),
        convertTo[D, UShort](img),
        convertTo[D, UInt](img)
      )

      def read[D: NDSpace, T: Scalar: TypeTag: ClassTag](file: File): Try[DiscreteScalarImage[D, T]] = {
        implicitly[NDSpace[D]].dimensionality match {
          case 3 => ImageIO.read3DScalarImage[T](file).asInstanceOf[Try[DiscreteScalarImage[D, T]]]
          case 2 => ImageIO.read2DScalarImage[T](file).asInstanceOf[Try[DiscreteScalarImage[D, T]]]
          case _ => Failure(new NotImplementedError())
        }
      }

      def check[D: NDSpace, T: Scalar: TypeTag: ClassTag](result: Try[DiscreteScalarImage[D, T]], actualType: String): Unit = {
        val tryType = ImageIO.ScalarType.fromType[T].toString
        if (tryType == actualType) {
          result should be a 'Success
        } else {
          result should be a 'Failure
          result.failed.get.getMessage.contains(s"expected $tryType") should be(true)
          result.failed.get.getMessage.contains(s"found $actualType") should be(true)
        }
      }

      def eval[D: NDSpace](data: Seq[ImageWithType[D, _]]): Unit = {
        for (c <- data) {
          val vtk = File.createTempFile(c.typeName, ".vtk")
          vtk.deleteOnExit()

          def checkAll(file: File) = {
            check(read[D, Byte](file), c.typeName)
            check(read[D, Short](file), c.typeName)
            check(read[D, Int](file), c.typeName)
            check(read[D, Float](file), c.typeName)
            check(read[D, Double](file), c.typeName)
            check(read[D, UByte](file), c.typeName)
            check(read[D, UShort](file), c.typeName)
            check(read[D, UInt](file), c.typeName)
          }

          c.writeVtk(vtk) should be a 'Success
          ImageIO.ScalarType.ofFile(vtk).get.toString should equal(c.typeName)

          checkAll(vtk)
          vtk.delete()

          if (implicitly[NDSpace[D]].dimensionality == 3) {
            val nii = File.createTempFile(c.typeName, ".nii")
            nii.deleteOnExit()

            c.writeNii(nii) should be a 'Success
            ImageIO.ScalarType.ofFile(nii).get.toString should equal(c.typeName)
            checkAll(nii)
            nii.delete()
          }
        }
      }

      eval(imageSeq(img2))
      eval(imageSeq(img3))
    }
  }

}