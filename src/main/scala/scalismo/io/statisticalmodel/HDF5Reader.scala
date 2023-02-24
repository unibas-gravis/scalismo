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
import scalismo.hdfjson.HDFPath

import java.io.Closeable
import scala.jdk.CollectionConverters.MapHasAsScala
import scala.util.Try

class HDF5Reader(h5file: HdfFile) extends Closeable with StatisticalModelReader {

  override def close(): Unit = { h5file.close() }

  def exists(path: HDFPath): Boolean = Try { h5file.getByPath(path.toString) }.isSuccess

  def readString(path: HDFPath): Try[String] = {

    Try {
      h5file.getDatasetByPath(path.toString).getData.asInstanceOf[String]
    }
  }

  def readStringAttribute(path: HDFPath, attrName: String): Try[String] = {
    Try {
      val attribute = h5file
        .getByPath(path.toString)
        .getAttribute(attrName)
        .getData()

      if (attribute.isInstanceOf[String])
        attribute.asInstanceOf[String]
      else attribute.asInstanceOf[Array[String]].head
    }
  }

  def readIntAttribute(path: HDFPath, attrName: String): Try[Int] = {

    Try {
      val attribute = h5file
        .getByPath(path.toString)
        .getAttribute(attrName)
        .getData()

      if (attribute.isInstanceOf[Int])
        attribute.asInstanceOf[Int]
      else attribute.asInstanceOf[Array[Int]].head
    }

  }

  def getPathOfChildren(path: HDFPath): Try[Seq[HDFPath]] = {
    Try {
      h5file
        .getByPath(path.toString)
        .asInstanceOf[io.jhdf.api.Group]
        .getChildren
        .asScala
        .keys
        .toSeq
        .map(childPath => HDFPath(path, childPath))
    }
  }

  /**
   * Reads an ndArray from the path. The dataCast is a caster (usually done with asInstance[T])
   * which casts a type Object into an Array[T]. The reason this has to be provided is that
   * it is not possible to cast to a generic type, due to type erasure.
   */
  def readNDArrayFloat(path: HDFPath): Try[NDArray[Float]] = {
    Try {
      val node = h5file.getDatasetByPath(path.toString)
      val dims = node.getDimensions

      val arrayND = node.getData()
      val flattened = arrayND.asInstanceOf[Array[Array[Float]]].flatten
      NDArray(dims.map(_.toLong).toIndexedSeq, flattened)
    }
  }

  def readNDArrayInt(path: HDFPath): Try[NDArray[Int]] = {
    Try {
      val node = h5file.getDatasetByPath(path.toString)
      val dims = node.getDimensions

      val arrayND = node.getData()
      val flattened = arrayND.asInstanceOf[Array[Array[Int]]].flatten
      NDArray(dims.map(_.toLong).toIndexedSeq, flattened)
    }
  }

  /*
   * Reads an Array from the path.
   * The dataCast is a caster (usually done with asInstance[T])
   * which casts a type Object into an Array[T]. The reason this has to be provided is that
   * it is not possible to cast to a generic type, due to type erasure.
   */
  def readArrayInt(path: HDFPath): Try[Array[Int]] = {
    Try {
      val node = h5file.getDatasetByPath(path.toString)
      val dims = node.getDimensions
      node.getData().asInstanceOf[Array[Int]];
    }
  }

  def readArrayFloat(path: HDFPath): Try[Array[Float]] = {
    Try {
      val node = h5file.getDatasetByPath(path.toString)
      val dims = node.getDimensions
      node.getData().asInstanceOf[Array[Float]];
    }
  }

  def readInt(path: HDFPath): Try[Int] = {
    Try {
      val node = h5file.getDatasetByPath(path.toString)
      node.getData().asInstanceOf[Int]
    }
  }

  def readFloat(path: HDFPath): Try[Float] = {
    Try {
      val node = h5file.getDatasetByPath(path.toString)
      node.getData().asInstanceOf[Float]
    }
  }
}
