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

class PlantSpecTest extends JUnitSuite{
	@Test def phaseolusVulgaris {
		val expectedLengths = Array(107, 175, 132, 95, 72, 113, 102, 133, 105, 89, 100)
		assertArrayEquals(expectedLengths, PlantSpec.phaseolusVulgaris.chromasomeLengths)
	}
	
	@Test def equalityAndHashcode {
		val spec1 = new PlantSpec(Array(1,2,3,4))
		val spec2 = new PlantSpec(Array(1,2,3))
		val spec3 = new PlantSpec(Array(1,2,3,4))
		
		assertEquals(spec1, spec3)
		assertFalse(spec1 == spec2)
		
		assertEquals(spec1.hashCode, spec3.hashCode)
		assertFalse(spec1.hashCode == spec2.hashCode)
	}
}
