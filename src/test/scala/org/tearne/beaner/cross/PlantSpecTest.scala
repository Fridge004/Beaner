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

import org.scalatest.junit.AssertionsForJUnit._
import org.junit.Assert._
import org.junit.Test
import org.scalatest.junit.JUnitSuite
import org.tearne.beaner.plant._
import org.scalatest.mock.MockitoSugar

class PlantSpecTest extends JUnitSuite with MockitoSugar{
  class PlantSpecImpl1 extends PlantSpec{
    val chromosomeLengths = Array(5,4,2)
  }
  class PlantSpecImpl2 extends PlantSpec{
    val chromosomeLengths = Array(5,4,1)
  }

	@Test def phaseolusVulgaris {
		val expectedLengths = Array(107, 175, 132, 95, 72, 113, 102, 133, 105, 89, 100)
		assertArrayEquals(expectedLengths, PhaseolusVulgaris.chromosomeLengths)
	}
	
	@Test def equalityAndHashcode {
		val spec1 = new PlantSpecImpl1
		val spec2 = new PlantSpecImpl2
		val spec3 = new PlantSpecImpl1
		
		assertEquals(spec1, spec3)
		assertFalse(spec1 == spec2)
		
		assertEquals(spec1.hashCode, spec3.hashCode)
		assertFalse(spec1.hashCode == spec2.hashCode)
	}
}
