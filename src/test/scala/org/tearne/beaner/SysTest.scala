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

class SysTest extends MockitoSugar{
	val tolerance = 1e-16
	val lowTolerance = 0.001

	val spec = mock[PlantSpec]; when(spec.chromasomeLengths).thenReturn(Array[Int](50,70,90))
	val p1 = new ParentPlant("p1", spec) 
	val p2 = new ParentPlant("p2", spec)
	val pV = new ParentPlant("p3", spec)
	var f1, bc1, bc2, fin:OffspringPlant = null
	val chromaCrosser = new ChromasomeCrosser()
	val criteria = List(new Criteria(p1, 0, 9), new Criteria(p2, 1, 39))

	@Before def setup {
		//Heterozygous selection
		f1 = new PlantCrosser(p1, p2, chromaCrosser).selectHeterozygousOffspring(criteria).get
		bc1 = new PlantCrosser(f1, pV, chromaCrosser).selectHeterozygousOffspring(criteria).get
		bc2 = new PlantCrosser(bc1, pV, chromaCrosser).selectHeterozygousOffspring(criteria).get

		//Homozygous selection
		fin = new PlantCrosser(bc2, bc2, chromaCrosser).selectHomozygousOffspring(criteria).get
	}
		
	@Test def crossingTest1 {	
		assertEquals(1.0, f1.selectionProbability.get, tolerance)
		assertEquals(0.0, f1.chromasomes(2).proportionOf(pV), tolerance)
		
		assertEquals(0.25, bc1.selectionProbability.get, tolerance)
		assertEquals(0.5, bc1.chromasomes(2).proportionOf(pV), tolerance)
		
		assertEquals(0.25, bc2.selectionProbability.get, tolerance)
		assertEquals(0.75, bc2.chromasomes(2).proportionOf(pV), tolerance)
		
		assertEquals(1.0/16.0, fin.selectionProbability.get, tolerance)
		assertEquals(0.75, fin.chromasomes(2).proportionOf(pV), tolerance)

	}
	
	@Test def crossingTest2 {
		assertEquals(0.75, fin.chromasomes(0).firstChromatid.probabilityOf(p1, 0), lowTolerance)
		assertEquals(0.78, fin.chromasomes(0).firstChromatid.probabilityOf(p1, 1), lowTolerance)
		assertEquals(0.80, fin.chromasomes(0).firstChromatid.probabilityOf(p1, 2), lowTolerance)
		assertEquals(0.83, fin.chromasomes(0).firstChromatid.probabilityOf(p1, 3), lowTolerance)
		assertEquals(0.86, fin.chromasomes(0).firstChromatid.probabilityOf(p1, 4), lowTolerance)
		assertEquals(0.88, fin.chromasomes(0).firstChromatid.probabilityOf(p1, 5), lowTolerance)
		assertEquals(0.91, fin.chromasomes(0).firstChromatid.probabilityOf(p1, 6), lowTolerance)
		assertEquals(0.94, fin.chromasomes(0).firstChromatid.probabilityOf(p1, 7), lowTolerance)
		assertEquals(0.97, fin.chromasomes(0).firstChromatid.probabilityOf(p1, 8), lowTolerance)
		assertEquals(1.0, fin.chromasomes(0).firstChromatid.probabilityOf(p1, 9), lowTolerance)
		assertEquals(0.97, fin.chromasomes(0).firstChromatid.probabilityOf(p1, 10), lowTolerance)
		assertEquals(0.91, fin.chromasomes(0).firstChromatid.probabilityOf(p1, 11), lowTolerance)
		assertEquals(0.94, fin.chromasomes(0).firstChromatid.probabilityOf(p1, 12), lowTolerance)
		assertEquals(0.94, fin.chromasomes(0).firstChromatid.probabilityOf(p1, 13), lowTolerance)
		assertEquals(0.91, fin.chromasomes(0).firstChromatid.probabilityOf(p1, 14), lowTolerance)
		assertEquals(0.88, fin.chromasomes(0).firstChromatid.probabilityOf(p1, 15), lowTolerance)
		assertEquals(0.86, fin.chromasomes(0).firstChromatid.probabilityOf(p1, 16), lowTolerance)
		assertEquals(0.83, fin.chromasomes(0).firstChromatid.probabilityOf(p1, 17), lowTolerance)
		assertEquals(0.80, fin.chromasomes(0).firstChromatid.probabilityOf(p1, 18), lowTolerance)
		
	}
}
