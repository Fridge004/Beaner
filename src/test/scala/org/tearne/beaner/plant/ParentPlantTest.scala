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

import org.junit.{Test, Before}
import org.junit.Assert._
import org.mockito.Matchers._
import org.mockito.Mockito._
import org.scalatest.junit.JUnitSuite
import org.scalatest.Assertions._
import org.scalatest.junit.AssertionsForJUnit
import org.scalatest.mock.MockitoSugar

class ParentPlantTest extends JUnitSuite with MockitoSugar{
	val tolerance = 1e-16
	var p:Plant = null
  val plantName = "myPlant"
	
	@Before def setup {
		p = PhaseolusVulgaris(plantName)
	}
		
	@Test def creationOfChromasomes {
		assertEquals(PhaseolusVulgaris.chromosomeLengths(0), p.chromosomes(0).size)
		assertEquals(PhaseolusVulgaris.chromosomeLengths(3), p.chromosomes(3).size)
		assertEquals(PhaseolusVulgaris.chromosomeLengths(6), p.chromosomes(6).size)
		
		val c = p.chromosomes(6)
		for(i<-0 until p.chromosomes(6).size){
			assertEquals(1.0, c.firstChromatid.probabilityOf(p,i), tolerance)
			assertEquals(1.0, c.secondChromatid.probabilityOf(p,i), tolerance)
		}
	}
		
	@Test def correctNumberOfChromasomes {
		assertEquals(PhaseolusVulgaris.chromosomeLengths.size, p.chromosomes.size)
	}

  @Test def nameFromConstructor {
    assert(p.name.get === plantName)

    val otherName = "otherPlant"
    p = new ParentPlant(PhaseolusVulgaris, otherName)
    assert(p.name.get === otherName)
  }
}
