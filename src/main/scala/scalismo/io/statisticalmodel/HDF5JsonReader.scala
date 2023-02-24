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

import io.jhdf.HdfFile
import scalismo.hdfjson.{FloatArray1D, FloatArray2D, HDFJson, HDFPath, IntArray1D, IntArray2D}

import java.io.Closeable
import scala.jdk.CollectionConverters.MapHasAsScala
import scala.util.{Failure, Success, Try}

class HDF5JsonReader(hdfjson: HDFJson) extends Closeable with StatisticalModelReader {

  override def close(): Unit = {
    /* there is no need to close this file, as hdfjson is  string */
  }

  def exists(path: HDFPath): Boolean = hdfjson.exists(path)

  def readString(path: HDFPath): Try[String] = {

    hdfjson.getDataset[String](path.parent, path.lastComponent) match {
      case Some(s) => Success(s)
      case None    => Failure(new Exception(s"Could not read string at path $path"))
    }
  }

  def readStringAttribute(path: HDFPath, attrName: String): Try[String] = {
    hdfjson.getAttribute[String](path, attrName) match {
      case Some(s) => Success(s)
      case None    => Failure(new Exception(s"Could not read string attribute $attrName at path $path"))
    }
  }

  def readIntAttribute(path: HDFPath, attrName: String): Try[Int] = {
    hdfjson.getAttribute[Int](path, attrName) match {
      case Some(s) => Success(s)
      case None    => Failure(new Exception(s"Could not read string attribute $attrName at path $path"))
    }
  }

  def getPathOfChildren(path: HDFPath): Try[Seq[HDFPath]] = {
    val numComponentsPath = path.components.size
    Success(hdfjson.groups.filter(groupPath => {
      groupPath.components.size > numComponentsPath && HDFPath(groupPath.components.take(numComponentsPath)) == path
    }))
  }

  /**
   * Reads an ndArray from the path. The dataCast is a caster (usually done with asInstance[T])
   * which casts a type Object into an Array[T]. The reason this has to be provided is that
   * it is not possible to cast to a generic type, due to type erasure.
   */
  def readNDArrayInt(path: HDFPath): Try[NDArray[Int]] = {

    hdfjson.getDataset[IntArray2D](path.parent, path.lastComponent) match {
      case Some(intArray2D) =>
        val array = intArray2D.toArray
        Success(NDArray(IndexedSeq(array.size, if (array.length > 0) array(0).size else 0), array.flatten))
      case None => {
        Failure(new Exception(s"ndarray at $path is not a 2d int array. Cannot raed it"))
      }
    }
  }

  def readNDArrayFloat(path: HDFPath): Try[NDArray[Float]] = {

    hdfjson.getDataset[FloatArray2D](path.parent, path.lastComponent) match {
      case Some(floatArray2D) =>
        val array = floatArray2D.toArray
        Success(NDArray(IndexedSeq(array.size, if (array.length > 0) array(0).size else 0), array.flatten))
      case None => {
        Failure(new Exception(s"ndarray at $path is not a 2d float array. Cannot read it"))
      }
    }
  }

  /*
   * Reads an Array from the path.
   * The dataCast is a caster (usually done with asInstance[T])
   * which casts a type Object into an Array[T]. The reason this has to be provided is that
   * it is not possible to cast to a generic type, due to type erasure.
   */
  def readArrayInt(path: HDFPath): Try[Array[Int]] = {
    // the interface was written for the version of hdf, which was not written for hdfjson
    // to not change too much at the time, we simply do a dirty hack here and try all the
    // possible types
    hdfjson.getDataset[IntArray1D](path.parent, path.lastComponent) match {
      case Some(intArray1D) => Success(intArray1D.toArray)
      case None             => Failure(new Exception(s"ndarray at $path is not a 1d int array. Cannot raed it"))
    }
  }

  def readArrayFloat(path: HDFPath): Try[Array[Float]] = {
    // the interface was written for the version of hdf, which was not written for hdfjson
    // to not change too much at the time, we simply do a dirty hack here and try all the
    // possible types
    hdfjson.getDataset[FloatArray1D](path.parent, path.lastComponent) match {
      case Some(floatArray1D) => Success(floatArray1D.toArray)
      case None               => Failure(new Exception(s"ndarray at $path is not  a 1d float array. Cannot raed it"))
    }
  }

  def readInt(path: HDFPath): Try[Int] = {
    hdfjson.getDataset[Int](path.parent, path.lastComponent) match {
      case Some(i: Int) => Success(i)
      case None         => Failure(new Exception(s"Could not read int at path $path"))
    }
  }

  def readFloat(path: HDFPath): Try[Float] = {
    hdfjson.getDataset[Int](path.parent, path.lastComponent) match {
      case Some(i: Int) => Success(i)
      case None         => Failure(new Exception(s"Could not float int at path $path"))
    }
  }
}
