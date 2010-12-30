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

package org.tearne.beaner.report

import org.junit.Test
import org.scalatest.junit.JUnitSuite
import org.scalatest.mock.MockitoSugar
import org.tearne.beaner.plant.{Name, Plant}

class NameTest extends JUnitSuite with MockitoSugar{
  @Test def naming{
    val myPlant = mock[Plant]

    val named = Name(myPlant,"bert")

    assert("bert" === named.name)
    assert(myPlant === named.plant)
  }
}