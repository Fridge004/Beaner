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
package org.tearne.beaner.chroma

import org.tearne.beaner.plant._
import org.tearne.beaner.plant.spec._
import org.scalatest.Assertions._
import org.scalatest.junit.AssertionsForJUnit
import org.scalatest.mock.MockitoSugar
import org.junit.Assert._
import org.junit.Test
import org.junit.Before
import org.mockito.Mockito._
import org.mockito.Matchers._

class CentimorganTest extends MockitoSugar{
	val tolerance = 1e-16
	var cM1, cM2:Centimorgan = null
	var p1,p2,p3,p4:Plant = null
	
	@Before def setup {
		p1 = mock[Plant]
		p2 = mock[Plant]
		p3 = mock[Plant]
		p4 = mock[Plant]
	}
	
	@Test def exceptionIfTryToCombineWhenDoesntSumToOne() {
		cM1 = new Centimorgan()
		cM1.alleles(p1) = 0.1
		cM2 = new Centimorgan()
		cM2.alleles(p1) = 0.4
		cM2.alleles(p3) = 0.6
		
		intercept[CentimorganException]{
			cM1.combinedWith(cM2, 20)
		}
	}
	
	@Test def combinedWith {
		cM1 = new Centimorgan()
		cM1.alleles(p1) = 0.1
		cM1.alleles(p2) = 0.9
		cM2 = new Centimorgan()
		cM2.alleles(p1) = 0.4
		cM2.alleles(p3) = 0.6
		
		val result = cM1.combinedWith(cM2, 0.8)
		
		assertEquals(0.1*0.8+0.4*0.2,result.alleles(p1), tolerance)
		assertEquals(0.9*0.8,result.alleles(p2), tolerance)
		assertEquals(0.6*0.2,result.alleles(p3), tolerance)
	}
	
	@Test def probabilityOfGeneFrom {
		cM1 = new Centimorgan()
		cM1.alleles(p1) = 0.1
		
		assertProbEquals(0.1, cM1.probabilityOf(p1))
		assertProbEquals(0.0, cM1.probabilityOf(p3))
	}
	
	@Test def exceptionIfTryToGameteifyBadCentimorgans {
		cM1 = new Centimorgan(p3)
		cM2 = new Centimorgan()
		cM2.alleles(p1) = 0.1
		cM2.alleles(p2) = 0.2
		
		intercept[CentimorganException]{
			cM1.gameteify(cM2, 0.1)
		}
		
		intercept[CentimorganException]{
			cM2.gameteify(cM1, 0.1)
		}
	}
		
	@Test def gemeteifyComplexCentimorgans {
		cM1 = new Centimorgan()
		cM1.alleles(p1)=0.5
		cM1.alleles(p2)=0.5
		
		cM2 = new Centimorgan()
		cM2.alleles(p3)=0.3
		cM2.alleles(p4)=0.7
		
		val gameteCM = cM1.gameteify(cM2, 0.8)
		
		assertProbEquals(0.5*0.8, gameteCM.alleles(p1))
		assertProbEquals(0.5*0.8, gameteCM.alleles(p2))
		assertProbEquals(0.3*0.2, gameteCM.alleles(p3))
		assertProbEquals(0.7*0.2, gameteCM.alleles(p4))
	}
	
	
	@Test
	def gameteifySimpleCentimorgans {
		val cM1 = new Centimorgan(p1)
		val cM2 = new Centimorgan(p2)
		
		val gameteCM = cM1.gameteify(cM2, 0.5)
		
		assertProbEquals(0.5, gameteCM.alleles(p1))
		assertProbEquals(0.5, gameteCM.alleles(p2))
	}
	
	@Test def emptyCentimorgan {
		val cM = new Centimorgan()
		assertEquals(0, cM.alleles.size)
	}
	
	@Test def defaultProbIsCertainty{
		val cM = new Centimorgan(p1)
		assertProbEquals(1.0, cM.alleles(p1))
	}
	
	private def assertProbEquals(a:Double, b:Double) = assertEquals(a,b,tolerance)
}
