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
package scalismo.statisticalmodel.asm

import ncsa.hdf.`object`.Group
import scalismo.io.HDF5File

import scala.collection.immutable.TreeMap
import scala.util.{ Failure, Success, Try }

// FIXME: naming to be discussed (also within the entire project). Right now, I'm using a mix of both styles
// ("Hdf5", but "IO"), according to the rule "Camel-case acronyms, but only if they're longer than 2 characters."
// See for instance http://stackoverflow.com/questions/1176950/acronyms-in-camel-back

/**
 * A trait signifying that the implementing object provides
 * a unique identifier used for serialization purposes.
 */
trait HasIOIdentifier {
  /**
   * An identifier uniquely identifying the kind of object.
   * Note: the prefix "builtin::" is reserved for identifiers of objects that are shipped with the scalismo framework.
   * For the purpose of compatibility with other implementations (e.g., the statismo C++ implementation), it is recommended
   * to restrict the characters used to the ASCII range.
   * @return the unique serialization identifier for this kind of object.
   */
  def identifier: String
}

/**
 * Metadata about an object, used for serialization purposes.
 * In addition to the unique identifier, attributes concerning the implementation version are provided.
 * @param identifier a unique IO identifier.
 * @param majorVersion major implementation version.
 * @param minorVersion minor implementation version.
 * @see HasIOIdentifier
 */
case class IOMetadata(override val identifier: String, majorVersion: Int, minorVersion: Int) extends HasIOIdentifier

/**
 * A trait signifying that the implementing object provides
 * identity and version information for (de)serialization purposes.
 */
trait HasIOMetadata {
  def ioMetadata: IOMetadata
}

/**
 * Trait providing methods for serializing/deserializing objects of type T
 * to/from HDF5 files.
 * @tparam T the type of objects which can be constructed from the information present in HDF5 files, and saved into such files.
 */
trait Hdf5IOHandler[T <: HasIOMetadata] {
  /**
   * Load (instantiate) an object of type T from the information in an HDF5 file.
   * The IO metadata present in the file, as well as the file and group with the file, are provided as arguments, so that implementations can read additional data that
   * might be required for correct object instantiation.
   * @param meta IO Metadata about the concrete implementation, as present in the HDF5 file.
   * @param h5File the HDF5 file containing the information about the object to be constructed.
   * @param h5Group the HDF5 group containing the information about the object to be constructed.
   * @return an object of type T corresponding to the provided IO metadata and initialized according to the information present in the file
   *         (wrapped in a [[Success]]]), or a [[Failure]] indicating the cause of the failure
   */
  def load(meta: IOMetadata, h5File: HDF5File, h5Group: Group): Try[T]

  /**
   * Save all required information about an object to an HDF5 file, so that the object can later be reconstructed using the [[Hdf5IOHandler.load]] method.
   * Note that implementations do not need to care about saving the object's IO metadata, as this is taken care of by the framework.
   * Thus, if the object to be stored does not require any further parameterization, this method does not need to be overridden (there is a default no-op implementation).
   * Otherwise, it is strongly advised to follow these rules in the implementation:
   *
   * - Do not write data anywhere except in the provided group (or subgroups thereof). In other words, do not store data in parent of sibling groups of <code>h5Group</code>.
   * - Do not store attributes directly attached to <code>h5Group</code>, as they might clash with the attributes used internally by the framework.
   * - There are no further limitations, i.e., you are free to create values and subgroups, or anything else, in <code>h5Group</code,
   * and attach attributes to anything except <code>h5Group</code> itself.
   *
   * @param t the object about which information is to be stored.
   * @param h5File the HDF5 file to save the information to.
   * @param h5Group the group under which to save the information in the HDF5 file.
   * @return [[Success]] or [[Failure]]
   */
  def save(t: T, h5File: HDF5File, h5Group: Group): Try[Unit] = Success(())
}

/**
 * Companion object of the [[Hdf5IOHandler]] trait, providing implementations for
 * reading/writing IO metadata from/to HDF5 files.
 */
object Hdf5IOHandler {
  final val IdentifierAttributeName = "identifier"
  final val MajorVersionAttributeName = "majorVersion"
  final val MinorVersionAttributeName = "minorVersion"

  /**
   * Saves a given IO metadata object to an HDF5 group.
   * The metadata is stored as attributes attached to the given group.
   * @param meta the metadata to be saved.
   * @param h5File the HDF5 file to save the metadata to.
   * @param h5Group the HDF5 Group within the file to save the metadata to.
   * @return [[Success]] or [[Failure]]
   */
  def saveMetadata(meta: IOMetadata, h5File: HDF5File, h5Group: Group): Try[Unit] = {
    val groupName = h5Group.getFullName
    for {
      _ <- h5File.writeStringAttribute(groupName, IdentifierAttributeName, meta.identifier)
      _ <- h5File.writeIntAttribute(groupName, MajorVersionAttributeName, meta.majorVersion)
      _ <- h5File.writeIntAttribute(groupName, MinorVersionAttributeName, meta.minorVersion)
    } yield ()
  }

  /**
   * Loads IO metadata from an HDF5 group.
   * The metadata is retrieved by reading the identifier and version information from attributes attached to the given group.
   * @param h5File the HDF5 file to read the metadata from.
   * @param h5Group the HDF5 Group within the file to read metadata from.
   * @return an IO medata object, wrapped in a [[Success]], or a [[Failure]] object indicating the failure that occurred.
   */
  def loadMetadata(h5File: HDF5File, h5Group: Group): Try[IOMetadata] = {
    val groupName = h5Group.getFullName
    for {
      identifier <- h5File.readStringAttribute(groupName, IdentifierAttributeName)
      majorVersion <- h5File.readIntAttribute(groupName, MajorVersionAttributeName)
      minorVersion <- h5File.readIntAttribute(groupName, MinorVersionAttributeName)
    } yield IOMetadata(identifier, majorVersion, minorVersion)
  }

}

/**
 * Trait specifying that the implementing object can load and save objects of a given type from/to files.
 * In addition, the object specifies which IO Identifier it can handle.
 *
 * Currently, we require that the HDF5 file format be supported.
 *
 * @tparam T the type of objects which can be constructed from the information present in files, and saved into files.
 * @see IOCollection
 */
trait IOHandler[T <: HasIOMetadata] extends Hdf5IOHandler[T] with HasIOIdentifier

/**
 * An object representing a collection of IO handlers providing load/save capabilities for a given object type.
 * If there are multiple implementations for the given type (e.g., multiple image preprocessor implementations,
 * multiple feature extractor implementations), they are distinguished (and uniquely identified) by their IO Identifiers
 * (see [[HasIOIdentifier]]).
 *
 * For every supported IO Identifier, a corresponding [[IOHandler]] must be registered using the <code>register()</code> method. The identifiers/handlers that come
 * built into scalismo are automatically registered and available, but manual registration is required for user-defined handlers.
 *
 * @tparam T the type of objects that can be loaded/saved
 * @tparam IO the type of the corresponding IO handlers
 */
class IOHandlers[T <: HasIOMetadata, IO <: IOHandler[T]] {
  private var instances = new TreeMap[String, IO]()

  /**
   * Register an IO handler to make it available for loading and saving object instances.
   * @param handler the IO Handler to register.
   */
  def register(handler: IO): Unit = {
    instances = instances + ((handler.identifier, handler))
  }

  /**
   * Find the handler corresponding to the given IO identifier.
   * @param identifier an IO identifier.
   * @return the corresponding IO handler, or a [[Failure]] if no handler was registered for the identifier.
   */
  def find(identifier: String): Try[IO] = {
    instances.get(identifier) match {
      case Some(value) => Success(value)
      case None => Failure(new IllegalArgumentException(s"No instance found for identifier=$identifier." +
        " You may need to call " + this.getClass.getName + ".register() once to make the implementation available."))
    }
  }

  /**
   * Convenience method to load an object from an HDF5 group.
   * This method loads the IO metadata from the file, then uses the list of registered handlers to automatically find the corresponding IO handler, and finally uses that handler to load the object.
   * @param h5File the HDF5 file containing the group.
   * @param h5Group the HDF5 group within the file to load the object from.
   * @return the object corresponding to the information in the HDF5 group.
   */
  def load(h5File: HDF5File, h5Group: Group): Try[T] = {
    for {
      meta <- Hdf5IOHandler.loadMetadata(h5File, h5Group)
      io <- find(meta.identifier)
      instance <- io.load(meta, h5File, h5Group)
    } yield instance
  }

  /**
   * Convenience method to store an object to an HDF5 group.
   * This method uses the list of registered handlers to automatically find the correct IO handler, then saves the object's metadata and uses the IO handler to save the object.
   * @param t the object to save.
   * @param h5File the HDF5 file containing the group.
   * @param h5Group the HDF5 group to save the object to.
   * @return [[Success]] or [[Failure]]
   */
  def save(t: T, h5File: HDF5File, h5Group: Group): Try[Unit] = {
    val meta = t.ioMetadata
    for {
      io <- find(meta.identifier)
      _ <- Hdf5IOHandler.saveMetadata(meta, h5File, h5Group)
      _ <- io.save(t, h5File, h5Group)
    } yield ()
  }
}

