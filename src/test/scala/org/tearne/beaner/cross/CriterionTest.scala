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

import org.scalatest.junit.AssertionsForJUnit
import org.junit.{ Test, Ignore }
import org.junit.Assert._
import org.scalatest.mock.MockitoSugar
import org.scalatest.Assertions._
import org.scalatest.junit.JUnitSuite
import org.mockito.Mockito._
import org.mockito.Matchers._

class CriterionTest extends JUnitSuite with MockitoSugar {

  @Test def andIngCriterion {
    val c1 = new Criterion(mock[Plant], 1, 2)
    val c2 = new Criterion(mock[Plant], 1, 2)
    
    val criteria: Set[Criterion] = c1 + c2
    assertTrue(criteria.contains(c1))
    assertTrue(criteria.contains(c2))
    assert(criteria.size == 2)
  }
}
