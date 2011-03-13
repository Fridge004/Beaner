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

import org.tearne.beaner.chroma._
import org.tearne.beaner.cross._
import org.junit.{Test, Before}
import org.junit.Assert._
import org.scalatest.junit.JUnitSuite
import org.scalatest.mock.MockitoSugar

import org.mockito.Mockito._
class OffspringPlantTest extends JUnitSuite with MockitoSugar{
	val tolerance = 1e-16
	var chroma1, chroma2, chroma3 : Chromosome = null
	var spec : PlantSpec = null
	var chromas:Array[Chromosome] = null
  val name = "name"
  var parents:PlantPair = null
	
	@Before def setup {
		spec = mock[PlantSpec]
		when(spec.chromosomeLengths).thenReturn(Array[Int](1,2,3))
		
		chroma1 = new Chromosome(mock[Plant],1)
		chroma2 = new Chromosome(mock[Plant],2)
		chroma3 = new Chromosome(mock[Plant],3)
		
		chromas = Array(chroma1, chroma2, chroma3)

    parents = mock[PlantPair]
	}

  @Test def numRequiredForConfidenceInSelection {
    val plant = new OffspringPlant(chromas, spec, 0.1)
    assertEquals(275, plant.numPlantsForConfidence(0.95, 20))
  }

	@Test def proportionOf {
		val p = mock[Plant]
		
		chroma1 = mock[Chromosome]
		chroma2 = mock[Chromosome]
		chroma3 = mock[Chromosome]
		when(chroma1.size).thenReturn(1)
		when(chroma2.size).thenReturn(2)
		when(chroma3.size).thenReturn(3)
		when(chroma1.proportionOf(p)).thenReturn(1.0)
		when(chroma2.proportionOf(p)).thenReturn(0.5)
		when(chroma3.proportionOf(p)).thenReturn(2.0/3.0)
		chromas = Array(chroma1, chroma2, chroma3)
		
		val plant = new OffspringPlant(chromas, spec)
		val expectedProportion = (1.0*spec.chromosomeLengths(0)+
								  0.5*spec.chromosomeLengths(1)+
								  2.0/3.0*spec.chromosomeLengths(2))/6
		
		assertEquals(expectedProportion, plant.proportionOf(p), tolerance)
	}
	
	@Test def defaultSelectionProbabilityIsNone {
		assertEquals(None, new OffspringPlant(chromas, spec).selectionProbability)
	}
	
	@Test def nonNoneSelectionProbability {
		assertEquals(0.3, new OffspringPlant(chromas, spec, 0.3).selectionProbability.get, tolerance)
	}
	
	@Test def exceptionIfChromasDontMatchSpec {
		chroma3 = new Chromosome(mock[Plant],4)
		chromas = Array(chroma1, chroma2, chroma3)
		
		intercept[OffspringPlantException]{
			new OffspringPlant(chromas, spec, 0.0)
		}
	}
	
	@Test def exceptionifSelectionProbOutOfRange {
		intercept[OffspringPlantException]{
			new OffspringPlant(chromas, spec, -0.3)
		}
		
		intercept[OffspringPlantException]{
			new OffspringPlant(chromas, spec, -1.1)
		}
	}

  @Test def attributes_nameAndParents {
    val plant = new OffspringPlant(chromas, spec, None, Option(name), Option(parents))

    assert(plant.parents.get === parents)
    assert(plant.name.get === name)
  }
	
	@Test def attributes {		
		val chromas = Array(chroma1, chroma2, chroma3)
		
		val plant = new OffspringPlant(chromas, spec, 0.5)
		
		assertTrue(plant.isInstanceOf[Plant])
		assertEquals(3, plant.chromosomes.size)
		assertEquals(chromas(0), plant.chromosomes(0))
		assertEquals(chromas(1), plant.chromosomes(1))
		assertEquals(chromas(2), plant.chromosomes(2))
		assertEquals(0.5, plant.selectionProbability.get, tolerance)
	}
}
