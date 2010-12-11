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
import org.junit.Assert._
import org.junit.Test
import org.junit.Before
import org.scalatest.mock.MockitoSugar
import org.scalatest.junit.JUnitSuite
import org.mockito.Mockito._
import org.mockito.Matchers._

class ChromatidTest extends JUnitSuite with MockitoSugar{

	val tolerance = 1e-16
	var p1, p2:Plant = null
	var cm1, cm2:Centimorgan = null
	
	@Before def setup(){
		p1 = mock[Plant]
		p2 = mock[Plant]
		
		cm1 = new Centimorgan(p1)
		cm2 = new Centimorgan(p2)
	}
	
	@Test def updating {
		val cTid = new Chromatid(p1, 10)
		cTid(4) = cm2
		
		assertEquals(1, cTid(0).alleles(p1), tolerance)
		assertEquals(1, cTid(1).alleles(p1), tolerance)
		assertEquals(1, cTid(2).alleles(p1), tolerance)
		assertEquals(1, cTid(3).alleles(p1), tolerance)
		assertEquals(1, cTid(4).alleles(p2), tolerance)
		assertEquals(1, cTid(5).alleles(p1), tolerance)
		assertEquals(1, cTid(6).alleles(p1), tolerance)
		assertEquals(1, cTid(7).alleles(p1), tolerance)
		assertEquals(1, cTid(8).alleles(p1), tolerance)
		assertEquals(1, cTid(9).alleles(p1), tolerance)
	}
	
	@Test def sumProbabilitiesOf {
		val cm3 = new Centimorgan()
		cm3.alleles(p1) = 0.5
		cm3.alleles(p2) = 0.5
		
		val cm4 = new Centimorgan()
		cm4.alleles(p1) = 0.7
		cm4.alleles(p2) = 0.3
		
		val cTidArray1 = new Array[Centimorgan](4)
		cTidArray1(0) = cm1
		cTidArray1(1) = cm1
		cTidArray1(2) = cm2
		cTidArray1(3) = cm2
		val chromatid1 = new Chromatid(cTidArray1)
		
		val cTidArray2 = new Array[Centimorgan](5)
		cTidArray2(0) = cm3
		cTidArray2(1) = cm3
		cTidArray2(2) = cm3
		cTidArray2(3) = cm4
		cTidArray2(4) = cm4
		val chromatid2 = new Chromatid(cTidArray2)		
		
		assertEquals(1.0+1.0+0.0+0.0, chromatid1.sumProbabilitiesOf(p1), tolerance)
		assertEquals(0.5+0.5+0.5+0.7+0.7, chromatid2.sumProbabilitiesOf(p1), tolerance)
		assertEquals(4, chromatid1.size)
		assertEquals(5, chromatid2.size)
	}
	
	@Test def accessLikeAnArray {
		val cm1 = mock[Centimorgan]
		val cm2 = mock[Centimorgan]
		
		val cTidArray1 = new Array[Centimorgan](3)
		cTidArray1(0) = cm1
		cTidArray1(1) = cm1
		cTidArray1(2) = cm2
		val chromatid = new Chromatid(cTidArray1)
		
		assertEquals(3, chromatid.size)
		assertEquals(cm1, chromatid(0))
		assertEquals(cm1, chromatid(1))
		assertEquals(cm2, chromatid(2))
	}
}
