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
package scalismo.numerics

object BSpline {

  private val twoByThree = 2.0 / 3.0; // a constant used later on

  /**
   * The bspline basis function for degrees 0 to 3
   */
  def nthOrderBSpline(n: Int)(x: Double): Double = {
    val absX: Double = scala.math.abs(x)
    val absXSquared: Double = absX * absX
    val absXCube: Double = absXSquared * absX
    val twoMinAbsX: Double = 2.0 - absX

    n match {
      case 0 => {
        if (-0.5 < x && x < 0.5) 1.0
        else if (absX == 0.5) 0.5
        else 0

      }
      case 1 => {
        if (-1 <= x && x <= 0) 1.0 + x
        else if (0 < x && x <= 1) 1.0 - x
        else 0
      }
      case 2 => {
        if (-1.5 <= x && x < -0.5) 0.5 * (x + 1.5) * (x + 1.5)
        else if (-0.5 <= x && x < 0.5) -(x + 0.5) * (x + 0.5) + (x - 0.5) + 1.5
        else if (x >= 0.5 && x < 1.5) 0.5 * (1 - (x - 0.5)) * (1 - (x - 0.5))
        else 0

      }
      case 3 => {

        if (absX >= 0 && absX < 1)
          twoByThree - absXSquared + 0.5 * absXCube
        else if (absX >= 1 && absX < 2)
          twoMinAbsX * twoMinAbsX * twoMinAbsX / 6.0
        else 0
      }
      case _ => throw new NotImplementedError("Bspline of order " + n + " is not implemented yet")
    }
  }

}
