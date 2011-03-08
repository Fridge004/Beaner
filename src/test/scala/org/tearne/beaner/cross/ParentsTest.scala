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

import org.junit.{ Test, Before }
import org.scalatest.junit.JUnitSuite
import org.mockito.Mockito._
import org.mockito.Matchers._
import org.scalatest.mock.MockitoSugar
import org.tearne.beaner.plant._

class ParentsTest extends JUnitSuite with MockitoSugar {
  var p1, p2: Plant = null
  var c1: Cross = null

  @Before def setup {
    p1 = mock[Plant]
    p2 = mock[Plant]
  }

  @Test def checkedNamedPlants(){
    when(p1.name).thenReturn(Option("myName"))
    when(p2.name).thenReturn(None)

    assert(Parents(p1,p2).bothNamed === false)
    assert(Parents(p1,p1).bothNamed === true)
    assert(Parents(p2,p2).bothNamed === false)
  }

  @Test def holdsPairOfPlants() {
    var pair = new PlantPair(p1, p2)
    assert(p1 === pair.first)
    assert(p2 === pair.second)

    pair = new PlantPair(p1, p2)
    assert(p1 === pair.first)
    assert(p2 === pair.second)
  }
}
