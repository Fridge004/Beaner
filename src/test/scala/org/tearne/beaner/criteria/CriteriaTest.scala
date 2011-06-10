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

import org.tearne.beaner.plant.Plant
import org.scalatest.junit.JUnitSuite
import org.scalatest.mock.MockitoSugar
import org.junit.Test
import org.junit.Assert.fail

class CriteriaTest extends JUnitSuite with MockitoSugar {
  
  @Test def separatingSingleAndDoubleSelections {
    val p1 = mock[Plant]
    val p2 = mock[Plant]

    val c1 = new Criterion(p1, 0, 0)
    val c2 = new Criterion(p1, 1, 0)
    val c3 = new Criterion(p1, 1, 10)
    val c4 = new Criterion(p2, 2, 0)

    val criteria: Criteria = c1 + c2 + c3 + c4

    val gatheredCriterion = criteria.getGatheredSelectionCriterion

    assert( gatheredCriterion.size === 3 )

    assert( gatheredCriterion contains c1 )

    val expectedDouble = new DoubleCriterion(c2, c3)
    assert( gatheredCriterion contains expectedDouble )

    assert( gatheredCriterion contains c4 )
  }

  @Test def checkSetLikeBahaviour {
    val c1 = mock[Criterion]
    val c2 = mock[Criterion]
    val c3 = mock[Criterion]

    var criteria = new Criteria()
    assert(criteria.size === 0)

    criteria = criteria + c1
    assert(criteria.size === 1)
    assert(criteria.contains(c1))

    criteria = criteria + c1
    assert(criteria.size === 1)
    assert(criteria.contains(c1))

    criteria = criteria + c2
    assert(criteria.size === 2)
    assert(criteria.contains(c1))
    assert(criteria.contains(c2))

    criteria = criteria + c3
    criteria = criteria + c1
    assert(criteria.size === 3)
    assert(criteria.contains(c1))
    assert(criteria.contains(c2))
    assert(criteria.contains(c3))
  }

  def checkOrderingMaintained() {
    val c1 = mock[Criterion]
    val c2 = mock[Criterion]
    val c3 = mock[Criterion]

    var criteria = new Criteria() + c1 + c1 + c2 + c3

    assert(criteria(0)===c3)
    assert(criteria(1)===c2)
    assert(criteria(2)===c3)
  }
}