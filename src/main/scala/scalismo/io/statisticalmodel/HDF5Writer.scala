package scalismo.io.statisticalmodel

import ncsa.hdf.`object`.{Attribute, Datatype, FileFormat, Group}

import java.io.Closeable
import scala.util.{Try, Success, Failure}
import scala.jdk.CollectionConverters._

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
