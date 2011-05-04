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
import org.junit.Assert._
import org.junit.Test
import org.junit.Before

class ChromosomeCriteriaTest extends JUnitSuite with MockitoSugar {

  var p1, p2: Plant = _

  @Before def setup {
    p1 = mock[Plant]
    p2 = mock[Plant]
  }

  @Test def failIfDifferentPlants {
    //TODO May remove this restriction in future, to allow different
    // donors to provide the required alleles

    val c1 = new Criterion(p1, 0, 1)
    val c2 = new Criterion(p2, 0, 10)

    intercept[ChromosomeCriteriaException]{
      ChromosomeCriteria(c1, c2)
    }
  }

  @Test def failIfSameCentimorgan {
    val c1 = new Criterion(p1, 0, 1)
    val c2 = new Criterion(p1, 0, 1)

    intercept[ChromosomeCriteriaException]{
      ChromosomeCriteria(c1, c2)
    }
  }

  @Test def failIfDifferentChromasome {
    val c1 = new Criterion(p1, 0, 1)
    val c2 = new Criterion(p1, 1, 1)

    intercept[ChromosomeCriteriaException]{
      ChromosomeCriteria(c1, c2)
    }
  }

  @Test def containsSingleSelection {
    val c = new Criterion(p1, 0, 1)

    val cc = ChromosomeCriteria(c)

    assert( cc.isInstanceOf[SingleChromosomeCriteria] )
    assert( cc.criterion === c )
  }

  @Test def containsDoubleSelection {
    val c1 = new Criterion(p1, 0, 1)
    val c2 = new Criterion(p1, 0, 10)

    val cc = ChromosomeCriteria(c1, c2)

    assert( cc.isInstanceOf[DoubleChromosomeCriteria] )
    assert( cc.criterion1 === c1 )
    assert( cc.criterion2 === c2 )
  }
}