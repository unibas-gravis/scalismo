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

import java.io.{Closeable, File, IOException}

import ncsa.hdf.`object`._
import ncsa.hdf.`object`.h5._

import scala.jdk.CollectionConverters._
import scala.util.{Failure, Success, Try}
import io.jhdf.HdfFile

case class NDArray[T](dims: IndexedSeq[Long], data: Array[T]) {
  require(dims.product == data.length, s"${dims.product} != ${data.length}")
}

class HDF5Reader(h5file: HdfFile) extends Closeable {

  override def close(): Unit = { h5file.close() }

  def exists(path: String): Boolean = Try { h5file.getByPath(path) }.isSuccess

  def readString(path: String): Try[String] = {

    Try {
      h5file.getDatasetByPath(sanitizePath(path)).getData.asInstanceOf[String]
    }
  }

  private def sanitizePath(path: String): String = {
    val pathWithoutTrailingSlash = if (path.endsWith("/")) {
      path.dropRight(1)
    } else {
      path
    }
    pathWithoutTrailingSlash.replaceAll("//", "/")
  }

  def readStringAttribute(path: String, attrName: String): Try[String] = {
    Try {
      val attribute = h5file
        .getByPath(sanitizePath(path))
        .getAttribute(attrName)
        .getData()

      if (attribute.isInstanceOf[String])
        attribute.asInstanceOf[String]
      else attribute.asInstanceOf[Array[String]].head
    }
  }

  def readIntAttribute(path: String, attrName: String): Try[Int] = {

    Try {
      val attribute = h5file
        .getByPath(sanitizePath(path))
        .getAttribute(attrName)
        .getData()

      if (attribute.isInstanceOf[Int])
        attribute.asInstanceOf[Int]
      else attribute.asInstanceOf[Array[Int]].head
    }

  }

  def getPathOfChildren(path: String): Try[Seq[String]] = {
    Try {
      h5file
        .getByPath(sanitizePath(path))
        .asInstanceOf[io.jhdf.api.Group]
        .getChildren
        .asScala
        .keys
        .toSeq
    }
  }

  /**
   * Reads an ndArray from the path. The dataCast is a caster (usually done with asInstance[T])
   * which casts a type Object into an Array[T]. The reason this has to be provided is that
   * it is not possible to cast to a generic type, due to type erasure.
   */
  def readNDArray[T](path: String)(implicit dataCast: ObjectToArrayCast[T]): Try[NDArray[T]] = {
    Try {
      val node = h5file.getDatasetByPath(sanitizePath(path))
      val dims = node.getDimensions

      val array = dataCast.cast(node.getData());
      NDArray(dims.map(_.toLong).toIndexedSeq, array)
    }
  }

  /*
   * Reads an Array from the path.
   * The dataCast is a caster (usually done with asInstance[T])
   * which casts a type Object into an Array[T]. The reason this has to be provided is that
   * it is not possible to cast to a generic type, due to type erasure.
   */
  def readArray[T](path: String)(implicit dataCast: ObjectToArrayCast[T]): Try[Array[T]] = {
    Try {
      val node = h5file.getDatasetByPath(sanitizePath(path))
      val dims = node.getDimensions
      dataCast.cast(node.getData());
    }
  }

  def readInt(path: String): Try[Int] = {
    Try {
      val node = h5file.getDatasetByPath(sanitizePath(path))
      node.getData().asInstanceOf[Int]
    }
  }

  def readFloat(path: String): Try[Float] = {
    Try {
      val node = h5file.getDatasetByPath(sanitizePath(path))
      node.getData().asInstanceOf[Float]
    }
  }

  def getGroup(path: String): Try[io.jhdf.api.Group] = Try {
    h5file.getByPath(sanitizePath(path)).asInstanceOf[io.jhdf.api.Group]
  }

  def getGroup(group: io.jhdf.api.Group, groupName: String): Try[io.jhdf.api.Group] = Try {
    h5file.getByPath(sanitizePath(group.getPath + "/" + groupName)).asInstanceOf[io.jhdf.api.Group]
  }
}

class HDF5Writer(h5file: FileFormat) extends Closeable {

  override def close(): Unit = { h5file.close() }

  def exists(path: String): Boolean = h5file.get(path) != null

  def writeIntAttribute(path: String, attrName: String, attrValue: Int): Try[Unit] = {
    Try {
      val s = h5file.get(path)
      val fileFormat: FileFormat = FileFormat.getFileFormat(FileFormat.FILE_TYPE_HDF5)
      val dtype: Datatype = fileFormat.createDatatype(Datatype.CLASS_INTEGER, 4, Datatype.NATIVE, Datatype.NATIVE)

      val attr = new Attribute(attrName, dtype, Array(1))
      attr.setValue(Array(attrValue))
      s.writeMetadata(attr)
    }
  }
  // should be merged with some type magic
  def writeStringAttribute(path: String, attrName: String, attrValue: String): Try[Unit] = {
    Try {
      val s = h5file.get(path)
      val fileFormat: FileFormat = FileFormat.getFileFormat(FileFormat.FILE_TYPE_HDF5)
      val dtype: Datatype =
        fileFormat.createDatatype(Datatype.CLASS_STRING, attrValue.length + 1, Datatype.NATIVE, Datatype.NATIVE)
      val attr = new Attribute(attrName, dtype, Array(1))
      attr.setValue(Array(attrValue))
      s.writeMetadata(attr)
    }
  }

  def writeNDArray[T](path: String, ndArray: NDArray[T]): Try[Unit] = {

    val (groupname, datasetname) = splitpath(path)
    val maybeGroup: Try[Group] = createGroup(groupname)
    maybeGroup.flatMap { group =>
      val dtypeOrFailure = ndArray.data match {
        case _: Array[Byte] => {
          Success(h5file.createDatatype(Datatype.CLASS_INTEGER, 1, Datatype.NATIVE, Datatype.NATIVE))
        }
        case _: Array[Short] => {
          Success(h5file.createDatatype(Datatype.CLASS_INTEGER, 2, Datatype.NATIVE, Datatype.NATIVE))
        }
        case _: Array[Int] => {
          Success(h5file.createDatatype(Datatype.CLASS_INTEGER, 4, Datatype.NATIVE, Datatype.NATIVE))
        }
        case _: Array[Long] => {
          Success(h5file.createDatatype(Datatype.CLASS_INTEGER, 8, Datatype.NATIVE, Datatype.NATIVE))
        }
        case _: Array[Float] => {
          Success(h5file.createDatatype(Datatype.CLASS_FLOAT, 4, Datatype.NATIVE, Datatype.NATIVE))
        }
        case _ => {
          // TODO error handling
          Failure(new Exception("unknown type for path " + path))
        }
      }

      val dsOrFailure = dtypeOrFailure.map(dtype =>
        h5file.createScalarDS(datasetname, group, dtype, ndArray.dims.toArray, null, null, 0, ndArray.data)
      )
      dsOrFailure.map(_ => ()) // ignore result and return either failure or unit
    }
  }

  def getGroup(parentGroup: Group, childGroupName: String): Try[Group] = {
    getGroup(parentGroup.getFullName + "/" + childGroupName)
  }

  def getGroup(groupName: String): Try[Group] = {
    if (exists(groupName))
      h5file.get(groupName) match {
        case g: Group => Success(g)
        case _        => Failure(new Throwable(s"object $groupName is not a group"))
      }
    else {
      Failure(new Throwable(s"group with name $groupName does not exist"))
    }
  }

  def writeArray[T](path: String, data: Array[T]): Try[Unit] = {
    writeNDArray[T](path, NDArray(Vector[Long](data.length), data))
  }

  def writeString(path: String, value: String): Try[Unit] = {
    val (groupname, datasetname) = splitpath(path)
    val groupOrFailure = createGroup(groupname)

    groupOrFailure.map { group =>
      val dtype = h5file.createDatatype(Datatype.CLASS_STRING, value.length, Datatype.NATIVE, Datatype.NATIVE)
      h5file.createScalarDS(datasetname, group, dtype, Array[Long](1), null, null, 0, Array[String](value))
      ()
    }
  }

  def writeInt(path: String, value: Int): Try[Unit] = {
    val (groupname, datasetname) = splitpath(path)
    val groupOrFailure = createGroup(groupname)

    groupOrFailure.map { group =>
      val fileFormat: FileFormat = group.getFileFormat
      val dtype: Datatype = fileFormat.createDatatype(Datatype.CLASS_INTEGER, 4, Datatype.NATIVE, Datatype.NATIVE)
      h5file.createScalarDS(datasetname, group, dtype, Array[Long](), null, null, 0, value, Array(value))
      ()
    }
  }

  def writeFloat(path: String, value: Float): Try[Unit] = {
    val (groupname, datasetname) = splitpath(path)
    val groupOrFailure = createGroup(groupname)

    groupOrFailure.map { group =>
      val fileFormat: FileFormat = group.getFileFormat
      val dtype: Datatype = fileFormat.createDatatype(Datatype.CLASS_FLOAT, 4, Datatype.NATIVE, Datatype.NATIVE)
      h5file.createScalarDS(datasetname, group, dtype, Array[Long](), null, null, 0, value, Array(value))
      ()
    }
  }

  def createGroup(parent: Group, relativePath: String): Try[Group] = {

    // remove trailing /
    val trimmedPath =
      if (relativePath.trim().last == '/') {
        relativePath.trim.dropRight(1)
      } else {
        relativePath
      }

    assert(!trimmedPath.startsWith("/"))
    if (trimmedPath == "") return Success(parent)

    val groupnames = trimmedPath.split("/", 2)

    def getMember(name: String) = parent.getMemberList.asScala.find(_.getName == name.trim())

    val newgroup = getMember(groupnames(0)) match {
      case Some(g) => g.asInstanceOf[Group]
      case None    => h5file.createGroup(groupnames(0), parent)
    }

    if (groupnames.length == 1) {
      // the path is just the group name, we are done and return
      Success(newgroup)
    } else {
      // otherwise we do a recursive call
      createGroup(newgroup, groupnames(1))
    }

  }

  def createGroup(absolutePath: String): Try[Group] = {

    val normalizedPath = absolutePath.replaceAll("//*", "/")

    val root =
      if (h5file.getRootNode == null)
        return Failure(new Throwable("file not correctly opened"))
      else {
        h5file.getRootNode
          .asInstanceOf[javax.swing.tree.DefaultMutableTreeNode]
          .getUserObject
          .asInstanceOf[Group]
      }
    if (!normalizedPath.startsWith("/"))
      return Failure(new Exception("expected absolute path, but found relative path: " + absolutePath))
    if (absolutePath.trim == "/")
      return Success(root)

    createGroup(root, normalizedPath.stripPrefix("/"))

  }

  private def splitpath(path: String): Tuple2[String, String] = {

    val pos = path.lastIndexOf("/")
    val (groupname, datasetname) = path.splitAt(pos)
    if (groupname == "") ("/", datasetname)
    else (groupname, datasetname)
  }

}

// make it a proper class and wrapper around the object
object HDF5Utils {

  // map untyped FileAccessModel of HDF5 (which is just a string)
  // to typed values
  object FileAccessMode extends Enumeration {
    type FileAccessMode = Value
    val WRITE, CREATE = Value
  }
  import FileAccessMode._

  def hdf5Version = "to be defined"

  private def openFileWriterOrCreateFile(file: File, mode: FileAccessMode): Try[HDF5Writer] = Try {
    val filename = file.getAbsolutePath
    val h5fileAccessMode = mode match {
      case WRITE  => FileFormat.WRITE
      case CREATE => FileFormat.CREATE
    }

    val fileFormat = FileFormat.getFileFormat(FileFormat.FILE_TYPE_HDF5)
    val h5file = fileFormat.createInstance(filename, h5fileAccessMode)

    if (h5file.open() == -1) {
      throw new IOException("could not open file " + file.getAbsolutePath)
    }
    new HDF5Writer(h5file)
  }

  def openFileForReading(file: File): Try[HDF5Reader] = {
    Try {
      val hdfFile = new HdfFile(file)
      new HDF5Reader(hdfFile)
    }
  }

  def openFileForWriting(file: File): Try[HDF5Writer] = openFileWriterOrCreateFile(file, WRITE)
  def createFile(file: File): Try[HDF5Writer] = openFileWriterOrCreateFile(file, CREATE)

}

/**
 * Typeclasses for reading, writing to hdf5 file
 */
trait HDF5Read[A] {
  def read(h5file: HDF5Reader, group: Group): Try[A]
}

trait HDF5Write[A] {
  def write(value: A, h5file: HDF5Writer, group: Group): Try[Unit]
}

trait ObjectToArrayCast[A] {
  def cast(arr: Object): Array[A]
}

object ObjectToArrayCast {
  implicit object ObjectToStringArrayCast extends ObjectToArrayCast[String] {
    override def cast(arr: Object): Array[String] = {
      if (arr.isInstanceOf[Array[Array[String]]]) {
        arr.asInstanceOf[Array[Array[String]]].flatten
      } else if (arr.isInstanceOf[Array[Array[Array[String]]]]) {
        arr.asInstanceOf[Array[Array[Array[String]]]].flatten.flatten
      } else {
        arr.asInstanceOf[Array[String]]
      }
    }
  }

  implicit object ObjectToFloatArrayCast extends ObjectToArrayCast[Float] {
    override def cast(arr: Object): Array[Float] = {
      if (arr.isInstanceOf[Array[Array[Float]]]) {
        arr.asInstanceOf[Array[Array[Float]]].flatten
      } else if (arr.isInstanceOf[Array[Array[Array[Float]]]]) {
        arr.asInstanceOf[Array[Array[Array[Float]]]].flatten.flatten
      } else {
        arr.asInstanceOf[Array[Float]]
      }
    }
  }

  implicit object ObjectToByteArrayCast extends ObjectToArrayCast[Byte] {
    override def cast(arr: Object): Array[Byte] = {
      if (arr.isInstanceOf[Array[Array[Byte]]]) {
        arr.asInstanceOf[Array[Array[Byte]]].flatten
      } else if (arr.isInstanceOf[Array[Array[Array[Byte]]]]) {
        arr.asInstanceOf[Array[Array[Array[Byte]]]].flatten.flatten
      } else {
        arr.asInstanceOf[Array[Byte]]
      }
    }
  }

  implicit object ObjectToIntArrayCast extends ObjectToArrayCast[Int] {
    override def cast(arr: Object): Array[Int] = {
      if (arr.isInstanceOf[Array[Array[Int]]]) {
        arr.asInstanceOf[Array[Array[Int]]].flatten
      } else if (arr.isInstanceOf[Array[Array[Array[Int]]]]) {
        arr.asInstanceOf[Array[Array[Array[Int]]]].flatten.flatten
      } else {
        if (arr.isInstanceOf[Array[Array[Long]]])
          arr.asInstanceOf[Array[Array[Long]]].flatten.map(_.toInt)
        else
          arr.asInstanceOf[Array[Int]]
      }
    }
  }

  implicit object ObjectToDoubleArrayCast extends ObjectToArrayCast[Double] {
    override def cast(arr: Object): Array[Double] = {
      if (arr.isInstanceOf[Array[Array[Double]]]) {
        arr.asInstanceOf[Array[Array[Double]]].flatten
      } else if (arr.isInstanceOf[Array[Array[Array[Double]]]]) {
        arr.asInstanceOf[Array[Array[Array[Double]]]].flatten.flatten
      } else {
        arr.asInstanceOf[Array[Double]]
      }
    }
  }
}
