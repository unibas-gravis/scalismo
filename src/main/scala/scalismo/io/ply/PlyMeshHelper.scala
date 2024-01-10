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
package scalismo.io.ply

import scalismo.common.PointId
import scalismo.geometry.{_2D, _3D, EuclideanVector, Point}

/**
 * Some helper function to transform data.
 */
object PlyMeshHelper {

  private[ply] def zeroOne2Int(d: Double): Int = {
    (d * 255).toInt
  }

  private[ply] def zeroOne2Byte(d: Double): Byte = {
    (d * 255).toByte
  }

  private[ply] def byte2ZeroOne(b: Byte): Double = {
    ((b.toDouble + 256) % 256) / 255
  }

  private[ply] def any2Double(any: Any): Double = {
    any match {
      case d: Double  => d
      case f: Float   => f.toDouble
      case l: Long    => l.toDouble
      case i: Int     => i.toDouble
      case s: Short   => s.toDouble
      case c: Char    => c.toDouble
      case b: Byte    => b.toDouble
      case b: Boolean => if (b) 1.0 else 0.0
      case _ =>
        throw new NumberFormatException("Could not make double from " + any)
    }
  }

  private[ply] def any2Byte(any: Any): Byte = {
    any match {
      case c: Char    => c.toByte
      case b: Byte    => b
      case b: Boolean => if (b) 1.toByte else 0.toByte
      case _ =>
        throw new NumberFormatException("Could not make double from " + any)
    }
  }

  private[ply] def any2Int(any: Any): Int = {
    any match {
      case i: Int     => i
      case s: Short   => s.toInt
      case c: Char    => c.toInt
      case b: Byte    => b.toInt
      case b: Boolean => if (b) 1 else 0
      case _ =>
        throw new NumberFormatException("Could not make int from " + any)
    }
  }

  private[ply] def listOfAny2ListOfPointIDs(any: Any): List[PointId] = {
    any match {
      case list: List[Any] =>
        list
          .map { a =>
            any2Int(a)
          }
          .map(a => PointId(a))
      case _ =>
        throw new NumberFormatException("Could not make list of doubles from " + any)
    }
  }

  private[ply] def listOfAny2ListOfPoint2D(any: Any): Seq[Point[_2D]] = {
    any match {
      case list: List[Any] =>
        list
          .map { a =>
            any2Double(a)
          }
          .grouped(2)
          .map(a => Point(a(0), a(1)))
          .toSeq
      case _ =>
        throw new NumberFormatException("Could not make list of doubles from " + any)
    }
  }

  private[ply] def listOfAny2ListOfVector3D(any: Any): Seq[EuclideanVector[_3D]] = {
    any match {
      case list: List[Any] =>
        list
          .map { a =>
            any2Double(a)
          }
          .grouped(3)
          .map(a => EuclideanVector(a(0), a(1), a(2)))
          .toSeq
      case _ =>
        throw new NumberFormatException("Could not make list of doubles from " + any)
    }
  }

}
