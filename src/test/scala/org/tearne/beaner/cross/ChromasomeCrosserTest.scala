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

import org.tearne.beaner.plant._
import org.tearne.beaner.chroma._
import org.scalatest.mock.MockitoSugar
import org.scalatest.junit.JUnitSuite
import org.mockito.Mockito._
import org.mockito.Matchers._
import org.junit.Assert._
import org.junit.Test
import org.junit.Before

class ChromosomeCrosserTest extends JUnitSuite with MockitoSugar{
	val tolerance = 1e-16
	var chromosome1, chromosome2: Chromosome = null
	var p1,p2:Plant = null
  var gamete1, gamete2: Chromatid = null
	
	var chromosomeCrosser:ChromosomeCrosser = null
  var gameter:Gameter = null;
	
	@Before def setup{
    gameter = mock[Gameter]
		chromosomeCrosser = new ChromosomeCrosser(gameter)

		val spec = mock[PlantSpec]
		when(spec.chromosomeLengths).thenReturn(Array(1))
		p1 = mock[Plant]
		p2 = mock[Plant]

    chromosome1 = mock[Chromosome]
    chromosome2 = mock[Chromosome]
    gamete1 = mock[Chromatid]
    gamete2 = mock[Chromatid]
	}
	
	@Test def getOffspringWithoutSelection {
	  when(gameter.withoutSelection(chromosome1)).thenReturn(gamete1)
    when(gameter.withoutSelection(chromosome2)).thenReturn(gamete2)

		val result = chromosomeCrosser.getOffspringWithoutSelection(chromosome1, chromosome2)
		
		assertEquals(gamete1, result.firstChromatid)
		assertEquals(gamete2, result.secondChromatid)
		assertEquals(None, result.selectionProbability)
	}
	
	@Test def failedHomozygousSelection {
    when(gameter.probContains(p1, 50, chromosome1)).thenReturn(0)
    when(gameter.probContains(p1, 50, chromosome2)).thenReturn(0)

    intercept[ChromosomeCrosserException]{
      chromosomeCrosser.selectHomozygousOffspring(chromosome1, chromosome2, p1, 50)
    }

    verify(gameter).probContains(p1, 50, chromosome1)
    verify(gameter).probContains(p1, 50, chromosome2)
	}
	
	@Test def homozygousSelection {
    when(gameter.probContains(p1, 50, chromosome1)).thenReturn(0.5)
    when(gameter.probContains(p1, 50 ,chromosome2)).thenReturn(0.4)
    when(gameter.selectOn(p1, 50, chromosome1)).thenReturn(gamete1)
    when(gameter.selectOn(p1, 50, chromosome2)).thenReturn(gamete2)

		val result = chromosomeCrosser.selectHomozygousOffspring(chromosome1, chromosome2, p1, 50)

		assertEquals(gamete1, result.firstChromatid)
		assertEquals(gamete2, result.secondChromatid)
		assertEquals(0.5*0.4, result.selectionProbability.get, tolerance)
	}
	
	@Test def failedHeterozygousSelection {
    when(gameter.probContains(p1, 50, chromosome1)).thenReturn(0)
    when(gameter.probContains(p1, 50, chromosome2)).thenReturn(0)

	  intercept[ChromosomeCrosserException]{
      chromosomeCrosser.selectHeterozygousOffspring(chromosome1, chromosome2, p1, 50)
    }

    verify(gameter).probContains(p1, 50, chromosome1)
    verify(gameter).probContains(p1, 50, chromosome2)
	}
	
	@Test def simpleHeterozygousSelection {
    when(gameter.probContains(p1, 50, chromosome1)).thenReturn(0.4)
    when(gameter.probContains(p1, 50, chromosome2)).thenReturn(0)
    when(gameter.selectOn(p1, 50, chromosome1)).thenReturn(gamete1)
    when(gameter.withoutSelection(chromosome2)).thenReturn(gamete2)

		val result = chromosomeCrosser.selectHeterozygousOffspring(chromosome1, chromosome2, p1, 50)
		
		assertEquals(gamete1, result.firstChromatid)
		assertEquals(gamete2, result.secondChromatid)
		assertEquals(0.4, result.selectionProbability.get, tolerance)
		
		verify(gameter, never()).withoutSelection(chromosome1)
	}
	
	@Test def exceptionIfTryHeterozygousSelectionAndBothChromosomesCanSupplyAllele {
		when(gameter.probContains(p1, 50, chromosome1)).thenReturn(0.5)
    when(gameter.probContains(p1, 50, chromosome2)).thenReturn(0.5)

		intercept[ChromosomeCrosserException]{
			chromosomeCrosser.selectHeterozygousOffspring(chromosome1, chromosome2, p1, 50)
		}
	}
}
