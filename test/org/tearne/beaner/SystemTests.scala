/*******************************************************************************
 * Copyright (c) 2010 Oliver Tearne (tearne at gmail dot com).
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 ******************************************************************************/
package org.tearne.beaner

import org.tearne.beaner.chroma._
import org.tearne.beaner.plant._
import org.tearne.beaner.plant.spec._
import org.tearne.beaner.plant.selection._
//import org.scalatest.junit.AssertionsForJUnit
import org.junit.{Test, Before}
import org.junit.Assert.{assertEquals, assertTrue}
import org.scalatest.mock.MockitoSugar
import org.scalatest.Assertions._
import org.mockito.Mockito._
import org.mockito.Matchers._

class AccTests extends MockitoSugar{
	val tolerance = 1e-16

	@Before def crossingTest2 {
		
	}
	
	@Test def crossingTest1 {
		val spec = mock[PlantSpec]; when(spec.chromasomeLengths).thenReturn(Array[Int](50,70,90))

		val p1 = new ParentPlant("p1", spec)
		val p2 = new ParentPlant("p2", spec)
		val pV = new ParentPlant("p3", spec)
		val criteria = List(new Criteria(p1, 0, 9), new Criteria(p2, 1, 39))
		val chromaCrosser = new ChromasomeCrosser()
		
		val f1 = new PlantCrosser(p1, p2, chromaCrosser).selectHeterozygousOffspring(criteria).get
		assertEquals(1.0, f1.selectionProbability.get, tolerance)
		assertEquals(0.0, f1.chromasomes(2).proportionOf(pV), tolerance)
		
		val bc1 = new PlantCrosser(f1, pV, chromaCrosser).selectHeterozygousOffspring(criteria).get
		assertEquals(0.25, bc1.selectionProbability.get, tolerance)
		assertEquals(0.5, bc1.chromasomes(2).proportionOf(pV), tolerance)
		
		val bc2 = new PlantCrosser(bc1, pV, chromaCrosser).selectHeterozygousOffspring(criteria).get
		assertEquals(0.25, bc2.selectionProbability.get, tolerance)
		assertEquals(0.75, bc2.chromasomes(2).proportionOf(pV), tolerance)
		
		//Homozygous
		val fin = new PlantCrosser(bc2, bc2, chromaCrosser).selectHomozygousOffspring(criteria).get
		assertEquals(1.0/16.0, fin.selectionProbability.get, tolerance)
		assertEquals(0.75, fin.chromasomes(2).proportionOf(pV), tolerance)
	}
	
}
