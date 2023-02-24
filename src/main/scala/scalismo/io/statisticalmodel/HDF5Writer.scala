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

import scalismo.hdfjson.{FloatArray1D, FloatArray2D, HDFAble, HDFJson, HDFPath, IntArray1D, IntArray2D}

import java.io.Closeable
import scala.util.{Failure, Success, Try}
import scala.jdk.CollectionConverters.*

class HDF5Writer(file: java.io.File, val initialHdfJson: Option[HDFJson] = None) extends Closeable {

  // hdfjson is immutable, but this interface requires mutable state
  // the var here is to reflect that
  private var hdfjson: HDFJson = initialHdfJson.getOrElse(HDFJson.createEmpty)

  override def close(): Unit = { write() }

  def write(): Try[Unit] = {
    HDFJson.writeToFile(hdfjson, file)
  }
  def exists(path: HDFPath): Boolean = {
    hdfjson.exists(path)
  }

  def writeIntAttribute(path: HDFPath, attrName: String, attrValue: Int): Try[Unit] = {
    Try {
      hdfjson = hdfjson.addAttribute(path, attrName, attrValue)
    }
  }
  // should be merged with some type magic
  def writeStringAttribute(path: HDFPath, attrName: String, attrValue: String): Try[Unit] = {
    Try {
      hdfjson = hdfjson.addAttribute(path, attrName, attrValue)
    }
  }

  def writeNDArray[T](path: HDFPath, ndArray: NDArray[T]): Try[Unit] = Try {

    if (ndArray.dims.size == 1) {
      ndArray.data match {
        case intArray: Array[Int] => {
          hdfjson = hdfjson.addDataset(path.parent, path.lastComponent, IntArray1D.from(intArray))
          Success(())
        }
        case floatArray: Array[Float] => {
          hdfjson = hdfjson.addDataset(path.parent, path.lastComponent, FloatArray1D.from(floatArray))
          Success(())
        }
        case _ => {
          // TODO error handling
          Failure(new Exception("unknown type for path " + path))
        }
      }
    } else if (ndArray.dims.size == 2) {
      ndArray.data match {
        case floatArray: Array[Float] => {
          val array2d = for (i <- (0 until ndArray.dims(0).toInt).toArray) yield {
            floatArray.slice(i * ndArray.dims(1).toInt, (i + 1) * ndArray.dims(1).toInt)
          }
          hdfjson = hdfjson.addDataset(path.parent, path.lastComponent, FloatArray2D.from(array2d))
          Success(())
        }
        case intArray: Array[Int] => {
          val array2d = for (i <- (0 until ndArray.dims(0).toInt).toArray) yield {
            intArray.slice(i * ndArray.dims(1).toInt, (i + 1) * ndArray.dims(1).toInt)
          }
          hdfjson = hdfjson.addDataset(path.parent, path.lastComponent, IntArray2D.from(array2d))
          Success(())
        }
        case _ => {
          Failure(new Exception("unknown type for path " + path))
        }
      }
    } else {
      Failure(new Exception("NDArray of dimension " + ndArray.dims + " not supported"))
    }
  }

  def writeArray[T](path: HDFPath, data: Array[T]): Try[Unit] = {
    writeNDArray[T](path, NDArray(Vector[Long](data.length), data))
  }

  def writeString(path: HDFPath, value: String): Try[Unit] = Try {
    hdfjson = hdfjson.addDataset(path.parent, path.lastComponent, value)
  }

  def writeInt(path: HDFPath, value: Int): Try[Unit] = Try {
    hdfjson = hdfjson.addDataset(path.parent, path.lastComponent, value)
  }

  def writeFloat(path: HDFPath, value: Float): Try[Unit] = Try {
    hdfjson = hdfjson.addDataset(path.parent, path.lastComponent, value)
  }

  def createGroup(absolutePath: HDFPath): Try[Unit] = Try {
    hdfjson = hdfjson.addGroup(absolutePath)
  }

}
