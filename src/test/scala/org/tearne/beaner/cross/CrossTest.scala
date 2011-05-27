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
import org.mockito.Mockito._
import org.junit.{Test, Before}
import org.junit.Assert._
import org.tearne.beaner.plant._
import org.tearne.beaner.cross._
import org.tearne.beaner.chroma._

class CrossTest extends JUnitSuite with MockitoSugar{
  var plantPair: PlantPair = null
  var criteria: Criteria = null
  val name = "named"
  var plantCrosser: PlantCrosser = null
  var cross: Cross = null

  @Before def setup {
    plantPair = mock[PlantPair]
    criteria = new Criteria(mock[Criterion])
    plantCrosser = mock[PlantCrosser]

    cross = new Cross(plantPair, criteria, null)
  }

  @Test def crossWithPlantReturnsMixedPait {
    val plant = mock[Plant]
    assertTrue(cross.x(plant).isInstanceOf[MixedPair])
  }


  @Test def exceptionIfNotNamed {
    val cross = new Cross(plantPair, criteria, Option(SelectionType.Homozygous))

    intercept[UnnamedPlantException]{
      cross.evaluateUsing(plantCrosser)
    }
  }

  @Test def naming {
    val cross = new Cross(plantPair, criteria, Option(SelectionType.Homozygous))
    val namedCross: Cross = cross named name

    assert(classOf[Cross] === namedCross.getClass)
    assert(name === namedCross.name.get)
    assert(plantPair === namedCross.pair)
    assert(criteria === namedCross.criteria)
    assert(SelectionType.Homozygous === namedCross.selectionType.get)
  }

  @Test def evaluatingHomozygous {
    val cross = new Cross(plantPair, criteria, Option(SelectionType.Homozygous), Option(name))

    var expected = new OffspringPlant(Array(), mock[PlantSpec], None, Option(name), None)

    when(plantCrosser.selectHomozygousOffspring(plantPair, criteria)).thenReturn(expected)
    val result = cross.evaluateUsing(plantCrosser)

    assert(result === expected)
  }

  @Test def evaluatingHeterozygous {
    val cross = new Cross(plantPair, criteria, Option(SelectionType.Heterozygous), Option(name))

    var expected = new OffspringPlant(Array(), mock[PlantSpec], None, Option(name), None)

    when(plantCrosser.selectHeterozygousOffspring(plantPair, criteria)).thenReturn(expected)
    val result = cross.evaluateUsing(plantCrosser)

    assert(result === expected)
  }
}