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

import org.tearne.beaner._
import plant._
import criteria._
import cross._

import org.scalatest.junit.AssertionsForJUnit
import org.junit.{ Test, Before }
import org.junit.Assert._
import org.scalatest.mock.MockitoSugar
import org.scalatest.Assertions._
import org.scalatest.junit.JUnitSuite
import collection._
import org.mockito.Matchers._

class SelectionCriterionTest extends JUnitSuite with MockitoSugar {

  var p1, p2: Plant = _

  @Before def setup {
    p1 = mock[Plant]
    p2 = mock[Plant]
  }

  @Test def andIngCriterion {
    val c1 = new Criterion(p1, 1, 2)
    val c2 = new Criterion(p2, 1, 2)
    
    val criteria = c1 + c2

    assertTrue(criteria.contains(c1))
    assertTrue(criteria.contains(c2))
    assert(criteria.size == 2)
  }

  //TODO Why necessary?  Helpful for testing?
//  @Test def orderPreserved {
//    val c1 = new Criterion(p1, 1, 2)
//    val c2 = new Criterion(p1, 2, 2)
//    val c3 = new Criterion(p1, 1, 4)
//    val c4 = new Criterion(p1, 5, 6)
//
//    val criteria = c1 + c2 + c3 + c4
//    val expectedOrder = List(c1,c2,c3,c4)
//
//    expectedOrder.zip(criteria).foreach{
//      case(expected, actual) => assert(expected === actual)
//    }
//  }


  @Test def failIfDifferentPlants {
    //TODO May remove this restriction in future, to allow different
    // donors to provide the required alleles

    val c1 = new Criterion(p1, 0, 1)
    val c2 = new Criterion(p2, 0, 10)

    intercept[CriterionException]{
      new DoubleCriterion(c1, c2)
    }
  }

  @Test def failIfSameCentimorgan {
    val c1 = new Criterion(p1, 0, 1)
    val c2 = new Criterion(p1, 0, 1)

    intercept[CriterionException]{
      new DoubleCriterion(c1, c2)
    }
  }

  @Test def failIfDifferentChromasome {
    val c1 = new Criterion(p1, 0, 1)
    val c2 = new Criterion(p1, 1, 1)

    intercept[CriterionException]{
      new DoubleCriterion(c1, c2)
    }
  }

  @Test def containsDoubleSelection {
    val c1 = new Criterion(p1, 0, 1)
    val c2 = new Criterion(p1, 0, 10)

    val dc = new DoubleCriterion(c1, c2)
    assert( dc.criterion1 === c1 )
    assert( dc.criterion2 === c2 )
  }
}
