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

package org.tearne.beaner.criteria

import org.scalatest.junit.JUnitSuite
import org.scalatest.mock.MockitoSugar
import org.junit.Test
import org.tearne.beaner._
import plant.Plant

class CriterionTest extends JUnitSuite with MockitoSugar{
  val p = mock[Plant]
  
  @Test def easyExtractionOfPlant{
    val c1 = new Criterion(p, 5, 1)
    val c2 = new Criterion(p, 5, 10)
    
    val d = new DoubleCriterion(c1, c2)
    
    assert( d.plant === p)    
  }
  
  @Test def easyExtractionOfChromosomeIndex{
    val c1 = new Criterion(p, 5, 1)
    val c2 = new Criterion(p, 5, 10)
    
    val d = new DoubleCriterion(c1, c2)
    
    assert( d.chromosomeIndex === 5 )
  }
  
  @Test def exceptionIfInconsistentChromosomeIndex {
    val c1 = new Criterion(p, 5, 1)
    val c2 = new Criterion(p, 6, 10)
      
    intercept[CriterionException]{
      new DoubleCriterion(c1, c2)
    }
  }
  
  @Test def exceptionIfDifferentPlants {
    val c1 = new Criterion(mock[Plant], 5, 1)
    val c2 = new Criterion(mock[Plant], 5, 10)
      
    intercept[CriterionException]{
      new DoubleCriterion(c1, c2)
    }
  }
  
  @Test def exceptionIfCentiMorgansSame {
    val c1 = new Criterion(p, 5, 1)
    val c2 = new Criterion(p, 5, 1)
      
    intercept[CriterionException]{
      new DoubleCriterion(c1, c2)
    }
  }
}