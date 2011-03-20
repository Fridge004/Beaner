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

package org.tearne.beaner.cross

import org.tearne.beaner.model._

import org.scalatest.junit.JUnitSuite
import org.junit.Test
import org.junit.Assert._
import scala.math._

class HaldaneRecombinationModelTest extends JUnitSuite {
  val tolerance = 1e-16

  @Test def probInAtDistance {
    val model = new HaldaneRecombinationModel()

    var prob = 0.0
    for (distance <- -100 to 100) {
      prob = 1.0-0.5*(1.0-exp(-2.0*abs(distance)/100.0))
      assertEquals("At distance "+distance, prob, model.probInAtDistance(distance), tolerance)
    }
  }
}