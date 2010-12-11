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

package org.tearne.beaner.chroma

import org.tearne.beaner.plant._
import org.scalatest.mock.MockitoSugar
import org.scalatest.Assertions._
import org.scalatest.junit.JUnitSuite
import org.mockito.Mockito._
import org.mockito.Matchers._
import org.junit.Assert._
import org.junit.Test
import org.junit.Before

class ChromasomeCrosserTest extends JUnitSuite with MockitoSugar{

	val tolerance = 1e-16
	var chromasome1, chromasome2 : Chromosome = null
	var p1,p2:Plant = null
	
	var chromasomeCrosser:ChromosomeCrosser = null
	
	@Before def setup{
		chromasomeCrosser = new ChromosomeCrosser()
		val spec = mock[PlantSpec]
		when(spec.chromasomeLengths).thenReturn(Array(1))
		p1 = mock[Plant]
		p2 = mock[Plant]
		
	}
	
	@Test def getOffspringWithoutSelection {
		val gamete1 = mock[Chromatid]
		val gamete2 = mock[Chromatid]
		
		chromasome1 = mock[Chromosome]
		when(chromasome1.makeGameteNoSelection).thenReturn(gamete1)
		chromasome2 = mock[Chromosome]
		when(chromasome2.makeGameteNoSelection).thenReturn(gamete2)
		
		val result = chromasomeCrosser.getOffspringWithoutSelection(chromasome1, chromasome2).get
		
		assertEquals(gamete1, result.firstChromatid)
		assertEquals(gamete2, result.secondChromatid)
		assertEquals(None, result.selectionProbability)
	}
	
	@Test def failedHomozygousSelection {
		chromasome1 = mock[Chromosome]
		when(chromasome1.probabilityGameteContains(p1, 50)).thenReturn(0)
		chromasome2 = mock[Chromosome]
		when(chromasome2.probabilityGameteContains(p1, 50)).thenReturn(0)
		
		val result = chromasomeCrosser.selectHomozygousOffspring(chromasome1, chromasome2, p1, 50)
		
		assertEquals(None, result)
	}
	
	@Test def homozygousSelection {
		val gamete1 = mock[Chromatid]
		val gamete2 = mock[Chromatid]
		
		chromasome1 = mock[Chromosome]
		when(chromasome1.probabilityGameteContains(p1, 50)).thenReturn(0.5)
		when(chromasome1.makeGameteSelectingFor(p1, 50)).thenReturn(gamete1)
		chromasome2 = mock[Chromosome]
		when(chromasome2.probabilityGameteContains(p1, 50)).thenReturn(0.4)
		when(chromasome2.makeGameteSelectingFor(p1, 50)).thenReturn(gamete2)
		
		val result = chromasomeCrosser.selectHomozygousOffspring(chromasome1, chromasome2, p1, 50).get
		
		assertEquals(gamete1, result.firstChromatid)
		assertEquals(gamete2, result.secondChromatid)
		assertEquals(0.5*0.4, result.selectionProbability.get, tolerance)
	}
	
	@Test def failedHeterozygousSelection {
		chromasome1 = mock[Chromosome]
		when(chromasome1.probabilityGameteContains(p1, 50)).thenReturn(0)
		chromasome2 = mock[Chromosome]
		when(chromasome2.probabilityGameteContains(p1, 50)).thenReturn(0)
		
		val result = chromasomeCrosser.selectHeterozygousOffspring(chromasome1, chromasome2, p1, 50)
		
		assertEquals(None,result)
	}
	
	@Test def simpleHeterozygousSelection {
		val gamete1 = mock[Chromatid]
		val gamete2 = mock[Chromatid]
		
		chromasome1 = mock[Chromosome]
		when(chromasome1.probabilityGameteContains(p1, 50)).thenReturn(0.4)
		when(chromasome1.makeGameteSelectingFor(p1, 50)).thenReturn(gamete1)
		chromasome2 = mock[Chromosome]
		when(chromasome2.probabilityGameteContains(p1, 50)).thenReturn(0)
		when(chromasome2.makeGameteNoSelection).thenReturn(gamete2)

		val result = chromasomeCrosser.selectHeterozygousOffspring(chromasome1, chromasome2, p1, 50).get
		
		assertEquals(gamete1, result.firstChromatid)
		assertEquals(gamete2, result.secondChromatid)
		assertEquals(0.4, result.selectionProbability.get, tolerance)
		
		verify(chromasome2, never()).makeGameteSelectingFor(anyObject(),anyInt())
	}
	
	@Test def exceptionIfTryHeterozygousSelectionAndBothChromosomesCanSupplyAllele {
		chromasome1 = mock[Chromosome]
		when(chromasome1.probabilityGameteContains(p1, 50)).thenReturn(0.5)
		chromasome2 = mock[Chromosome]
		when(chromasome2.probabilityGameteContains(p1, 50)).thenReturn(0.5)

		intercept[ChromasomeCrosserException]{
			chromasomeCrosser.selectHeterozygousOffspring(chromasome1, chromasome2, p1, 50) 
		}
	}
	
//	@Test def zeroSuccessHeterozygousSelectionProbability {
//		val chromatid1a = makeChromatidWithPlantAtIndex(p1, 100, 0.0)
//		val chromatid1b = makeChromatidWithPlantAtIndex(p1, 100, 0.0)
//		
//		val chromatid2a = makeChromatidWithPlantAtIndex(p1, 100, 0.0)
//		val chromatid2b = makeChromatidWithPlantAtIndex(p1, 100, 0.0)
//
//		chromasome1 = new Chromasome(chromatid1a, chromatid1b)
//		chromasome2 = new Chromasome(chromatid2a, chromatid2b)
//		
//		val expectedSuccessProbability = 0.0
//		val successProbability = chromasomeCrosser.probabilityOf(chromasome1, chromasome2, p1, 100)
//
//		assertEquals(expectedSuccessProbability, successProbability, tolerance)
//	}
//	
//	@Test def nonZeroSuccessHeterozygousSelectionProbabilty {
//		val chromatid1a = makeChromatidWithPlantAtIndex(p1, 100, 0.5)
//		val chromatid1b = makeChromatidWithPlantAtIndex(p1, 100, 0)
//		
//		val chromatid2a = makeChromatidWithPlantAtIndex(p1, 100, 0.5)
//		val chromatid2b = makeChromatidWithPlantAtIndex(p1, 100, 0.5)
//
//		chromasome1 = new Chromasome(chromatid1a, chromatid1b)
//		chromasome2 = new Chromasome(chromatid2a, chromatid2b)
//		
//		val expectedSuccessProbability = 0.625
//		val successProbability = chromasomeCrosser.probabilityOf(chromasome1, chromasome2,p1, 100)
//		
//		assertEquals(expectedSuccessProbability, successProbability, tolerance)
//	}
//	
//	@Test def certainSuccessHeterozygousSelectionProbabilty {
//		val chromatid1a = makeChromatidWithPlantAtIndex(p1, 100, 1)
//		val chromatid1b = makeChromatidWithPlantAtIndex(p1, 100, 1)
//		
//		val chromatid2a = makeChromatidWithPlantAtIndex(p1, 100, 0)
//		val chromatid2b = makeChromatidWithPlantAtIndex(p1, 100, 0)
//
//		chromasome1 = new Chromasome(chromatid1a, chromatid1b)
//		chromasome2 = new Chromasome(chromatid2a, chromatid2b)
//		
//		val expectedSuccessProbability = 1
//		val successProbability = chromasomeCrosser.probabilityOf(chromasome1, chromasome2, p1, 100)
//		
//		assertEquals(expectedSuccessProbability, successProbability, tolerance)
//	}
//	
//	private def makeChromatidWithPlantAtIndex(plant:Plant, index:Int, prob:Double)={
//		val chromatid = mock[Chromatid]
//		when(chromatid.probabilityOf(plant, index)).thenReturn(prob)
//		chromatid
//	}
}
