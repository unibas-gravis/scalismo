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

import niftijio.NiftiHeader
import scalismo.common.Scalar
import scalismo.utils.VtkHelpers
import spire.math.{UByte, UInt, UShort}
import vtk.{vtkObjectBase, vtkStructuredPointsReader}

import java.io.{File, IOException}
import scala.reflect.ClassTag
import scala.util.{Failure, Try}

/**
 * An enumeration comprising all the data types that we can read and write, in VTK and Nifti formats.
 */
private[scalismo] object ScalarDataType extends Enumeration {

  import NiftiHeader._
  import VtkHelpers._

  import scala.language.implicitConversions

  protected case class TheValue[O: Scalar: ClassTag](vtkId: Int, niftiId: Short) extends super.Val

  implicit def valueToVal[T](x: Value): TheValue[T] = x.asInstanceOf[TheValue[T]]

  val Byte = TheValue[Byte](VTK_CHAR, NIFTI_TYPE_INT8)
  val Short = TheValue[Short](VTK_SHORT, NIFTI_TYPE_INT16)
  val Int = TheValue[Int](VTK_INT, NIFTI_TYPE_INT32)
  val Float = TheValue[Float](VTK_FLOAT, NIFTI_TYPE_FLOAT32)
  val Double = TheValue[Double](VTK_DOUBLE, NIFTI_TYPE_FLOAT64)
  val UByte = TheValue[UByte](VTK_UNSIGNED_CHAR, NIFTI_TYPE_UINT8)
  val UShort = TheValue[UShort](VTK_UNSIGNED_SHORT, NIFTI_TYPE_UINT16)
  val UInt = TheValue[UInt](VTK_UNSIGNED_INT, NIFTI_TYPE_UINT32)

  /**
   * Return the ScalarType value corresponding to a given type
   * @tparam T a scalar type
   * @return the corresponding ScalarType value
   * @throws IllegalArgumentException if no corresponding value was found.
   */
  def fromType[T: Scalar]: Value = {
    Scalar[T].scalarType match {
      case Scalar.ByteScalar   => Byte
      case Scalar.ShortScalar  => Short
      case Scalar.IntScalar    => Int
      case Scalar.FloatScalar  => Float
      case Scalar.DoubleScalar => Double
      case Scalar.UByteScalar  => UByte
      case Scalar.UShortScalar => UShort
      case Scalar.UIntScalar   => UInt
      case _                   => throw new IllegalArgumentException(s"Unsupported datatype ${Scalar[T].scalarType}")
    }
  }

  /**
   * Return the ScalarType value corresponding to a given VTK type constant
   * @param vtkId a VTK type constant
   * @return the corresponding ScalarType value
   * @throws IllegalArgumentException if no corresponding value was found.
   */
  def fromVtkId(vtkId: Int): Value = {
    // there are two ways in VTK to represent a (signed) byte.
    if (vtkId == VTK_SIGNED_CHAR) Byte
    else
      values.find(v => v.vtkId == vtkId).getOrElse(throw new IllegalArgumentException(s"Unsupported VTK ID $vtkId"))
  }

  /**
   * Return the ScalarType value corresponding to a given Nifti type constant
   * @param niftiId a Nifti type constant
   * @return the corresponding ScalarType value
   * @throws IllegalArgumentException if no corresponding value was found.
   */
  def fromNiftiId(niftiId: Short): ScalarDataType.Value = {
    values
      .find(v => v.niftiId == niftiId)
      .getOrElse(throw new IllegalArgumentException(s"Unsupported Nifti ID $niftiId"))
  }

  /**
   * Return the ScalarType value corresponding to the data present in a given file. Only .vtk, .nii and .nia files are supported.
   * @param file the file to check
   * @return the scalar type present in the given file, wrapped in a [[scala.util.Success]], or a [[scala.util.Failure]] explaining the error.
   */
  def ofFile(file: File): Try[ScalarDataType.Value] = {
    val fn = file.getName
    if (fn.endsWith(".nii") || fn.endsWith(".nia")) {
      FastReadOnlyNiftiVolume.getScalarType(file)
    } else if (fn.endsWith(".vtk")) Try {
      val reader = new vtkStructuredPointsReader
      reader.SetFileName(file.getAbsolutePath)
      reader.Update()
      val errCode = reader.GetErrorCode()
      if (errCode != 0) {
        reader.Delete()
        throw new IOException(
          s"Failed to read vtk file ${file.getAbsolutePath}. (error code from vtkReader = $errCode)"
        )
      }
      val st = reader.GetOutput().GetScalarType()
      reader.Delete()
      // prevent memory leaks
      vtkObjectBase.JAVA_OBJECT_MANAGER.gc(false)
      ScalarDataType.fromVtkId(st)
    } else {
      Failure(new Exception(s"File $file: unsupported file extension"))
    }
  }
}
