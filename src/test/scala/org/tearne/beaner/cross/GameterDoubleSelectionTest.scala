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

import org.scalatest.junit.JUnitSuite
import org.scalatest.mock.MockitoSugar
import org.junit.Test
import org.junit.Assert._

import org.tearne.beaner._
import chroma._
import chroma._
import plant._
import model._
import math.State.{IN, OUT}
import math.DoubleSelectionProb

class GameterDoubleSelectionTest extends JUnitSuite with MockitoSugar{
  val tolerance = 1e-16
  val a = mock[Plant]
  val A = mock[Plant]
  
  val recombinationModel = new RecombinationModel{
    def probInAtDistance(d: Int) = if(d==0) 1 else 1/d
  }
  
  val gameter = new Gameter(recombinationModel)
 
  private def makeChromatid(plants:Plant*)={
    val tid = new Chromatid(a, plants.length)
    plants.zipWithIndex.foreach{case (p, i) =>
      tid(i) = new Centimorgan(p)
    }
    tid
  }
  
  val chromosome1 = new Chromosome(
			    	 makeChromatid(A,A,a,A,A,a,a),
			         makeChromatid(a,A,a,a,A,A,a)
			       )
  
  @Test def probabilityGameteContains {    
    assert( gameter.probContains(A, 0, 2, chromosome1) === 0 )
    assert( gameter.probContains(A, 0, 3, chromosome1) === 0.5*recombinationModel.probInAtDistance(3) )
    assert( gameter.probContains(A, 0, 5, chromosome1) === 0.5*(1-recombinationModel.probInAtDistance(2)) )
    
    intercept[UnsupportedOperationException]{
              gameter.probContains(A, 1, 4, chromosome1)
    }
    intercept[UnsupportedOperationException]{
              gameter.probContains(A, 1, 3, chromosome1)
    }
  }
  
  @Test def exceptionIfTryToSelectWhenNotPossibleWithProbabilityOneAtBothLocations {
    intercept[GameterException]{
      gameter.selectOn(A, 0, 2, chromosome1)
    }
    intercept[GameterException]{
      gameter.selectOn(A, 2, 6, chromosome1)
    }
    
    intercept[UnsupportedOperationException]{
              gameter.probContains(A, 1, 4, chromosome1)
    }
    intercept[UnsupportedOperationException]{
              gameter.probContains(A, 1, 3, chromosome1)
    }
  }
  
  @Test def exceptionIfSecondIndexBeforeFirst(){
    intercept[UnsupportedOperationException]{
    	gameter.probContains(A, 2, 0, chromosome1)
    }
    
    intercept[UnsupportedOperationException]{
      gameter.selectOn(A, 2, 0, chromosome1)
    }
  }
  
  @Test def selectionWhenBothOnSingleTid{
    val chromo = new Chromosome(
			    	 	makeChromatid(A,A,A,A,A),
			    	 	makeChromatid(a,a,a,a,a)
			       	 )
    val gamete = gameter.selectOn(A, 0, 4, chromo)
    
    val probs = new DoubleSelectionProb(recombinationModel).probabilityArray(3, IN, IN)
    assertEquals( 1, gamete.probabilityOf(A,0), tolerance )
    assertEquals( probs(0), gamete.probabilityOf(A,1), tolerance )
    assertEquals( probs(1), gamete.probabilityOf(A,2), tolerance )
    assertEquals( probs(2), gamete.probabilityOf(A,3), tolerance )
    assertEquals( 1, gamete.probabilityOf(A,4), tolerance )
  }
}