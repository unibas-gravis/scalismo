/*
 * Copyright University of Basel, Graphics and Vision Research Group
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */
package scalismo.io.plyScalismoFaces

/**
 * The PLYHelpers object encapsulates all well defined tokens, format specifier or types used to describe the specific
 * ply format in the header of the file.
 */
object PlyHelpers {

  /**
   * File formats supported so far.
   */
  object PlyFormat extends Enumeration {
    type PlyFormat = Value
    val ASCII = Value("ascii")
    val BinaryLittleEndian = Value("binary_little_endian")
    val BinaryBigEndian = Value("binary_big_endian")
  }

  /**
   * Different header types expected by common programs.
   */
  object PlyHeader extends Enumeration {
    type PlyHeader = Value
    val meshlab = Value("meshlab")
    val blender = Value("blender")
  }

  /**
   * Used parameter type names. (newer version not yet supported)
   */
  private[plyScalismoFaces] object PlyTypes extends Enumeration {
    type PlyTypes = Value
    val none = Value("none")
    val char = Value("char")
    val short = Value("short")
    val int = Value("int")
    val uchar = Value("uchar")
    val ushort = Value("ushort")
    val uint = Value("uint")
    val float = Value("float")
    val double = Value("double")
    val long = Value("long")
    val int8 = Value("int8")
    val uint8 = Value("uint8")
    val int16 = Value("int16")
    val int32 = Value("int32")
    val int64 = Value("int64")
    val float32 = Value("float32")
    val float64 = Value("float64")
  }

  /**
   * String tokens occuring in ply header descriptions
   */
  private[plyScalismoFaces] object PLY {
    val startHeader = "ply"
    val endHeader = "end_header"
    val format = "format"
    val comment = "comment"
    val textureFile = "TextureFile"
    val element = "element"
    val vertex = "vertex"
    val textureCoordinates = "texcoord"
    val vertexColor = "vertexcolor"
    val normals = "normals"
    val face = "face"
    val vertexIndices = "vertex_indices"
    val property = "property"
    val list = "list"
    val blenderU = "s"
    val blenderV = "t"
    val meshlabU = "texture_u"
    val meshlabV = "texture_v"
    val green = "green"
    val red = "red"
    val blue = "blue"
    val alpha = "alpha"
    val nx = "nx"
    val ny = "ny"
    val nz = "nz"
    val xCoordinate = "x"
    val yCoordinate = "y"
    val zCoordinate = "z"
  }

}
