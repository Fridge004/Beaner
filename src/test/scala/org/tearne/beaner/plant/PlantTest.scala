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

package org.tearne.beaner.plant

import org.scalatest.junit.JUnitSuite
import org.junit.Test
import org.mockito.Mockito._
import org.mockito.Spy._
import org.tearne.beaner.plant._
import org.tearne.beaner.chroma._
import org.tearne.beaner.cross._
import org.scalatest.mock.MockitoSugar
import org.scalatest.Assertions._

class PlantTest extends JUnitSuite with MockitoSugar{

  class PlantImpl(val chromasomes:Array[Chromosome], val spec:PlantSpec) extends Plant{}
  
  @Test def pairs() {
    val p1 = new PlantImpl(null, null)
    val p2 = new PlantImpl(null, null)
    
    val pair:PlantPair = p1 x p2
    assert(p1 === pair.first)
    assert(p2 === pair.second)
  }
}
