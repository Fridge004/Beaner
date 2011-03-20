/*
 * Copyright (c) Oliver Tearne (tearne at gmail dot com)
 *
 * This program is free software: you can redistribute it and/or modify it under the terms of
 * the GNU General Public License as published by the Free Software Foundation, either version
 * 3 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
 * without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with this program.
 * If not, see <http://www.gnu.org/licenses/>.
 */

package org.tearne.beaner.model

import scala.math._

class HaldaneRecombinationModel extends RecombinationModel{

  def probInAtDistance(distance: Int): Double = {
    val dist = abs(distance)
    1.0-0.5*(1.0-exp(-2.0*dist/100.0))

    //if (distance > 1) {
    //  val probIN = probInAtDist(distance - 1)
    //  0.99 * probIN + 0.01 * (1 - probIN)
    //} else
    //  0.99
  }
}