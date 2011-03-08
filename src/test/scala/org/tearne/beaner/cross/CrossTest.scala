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
  var criteria: Set[Criterion] = null
  val name = "named"
  var plantCrosser: PlantCrosser = null
  var cross: Cross = null

  @Before def setup {

    plantPair = mock[PlantPair]
    criteria = Set(mock[Criterion])
    plantCrosser = mock[PlantCrosser]

    cross = new Cross(plantPair, criteria, null)
  }

  @Test def crossAndPlantReturnsCrossParents {
    val plant = mock[Plant]
    assertTrue(cross.x(plant).isInstanceOf[MixedPair])
  }

//  @Test def crossDrillsDownToParentPlantsWithSpec {
//    spec = mock[Spec]
//    parent1 = mock[ParentPlant]
//    parent2 = mock[ParentPlant]
//    parent3 = mock[ParentPlant]
//    parent4 = mock[ParentPlant]
//    when(parent1.spec).thenReturn(spec)
//    when(parent2.spec).thenReturn(spec)
//    when(parent3.spec).thenReturn(spec)
//    when(parent4.spec).thenReturn(spec)
//
//    val cross1 = new Cross(new PlantPair(parent1, parent2), null, null)
//    val cross2 = new Cross(new PlantPair(parent3, parent4), null, null)
//    val cross3 = new Cross(new CrossPair(cross1, cross2), null, null)
//
//    assert(cross1.spec === spec)
//    assert(cross2.spec === spec)
//    assert(cross3.spec === spec)
//  }

//  @Test def exceptionIfCantDrillDownToParentSpec {
//    fail("not written yet")
//  }

//  @Test def aCrossIsAlsoAPlant {
//    fail("not written yet")
//    val cross = new Cross(plantPair, criteria, SelectionType.Homozygous)
//
//    assert(classOf[Plant] === cross.getClass)
//  }

  @Test def exceptionIfNotNamed {
    when(plantCrosser.selectHomozygousOffspring(plantPair, criteria)).thenReturn(None)

    val cross = new Cross(plantPair, criteria, SelectionType.Homozygous)

    intercept[UnnamedPlantException]{
      cross.evaluateUsing(plantCrosser)
    }
  }

  @Test def naming {
    val cross = new Cross(plantPair, criteria, SelectionType.Homozygous)
    val namedCross: Cross = cross named name

    assert(classOf[Cross] === namedCross.getClass)
    assert(name === namedCross.name.get)
    assert(plantPair === namedCross.pair)
    assert(criteria === namedCross.criteria)
    assert(SelectionType.Homozygous === namedCross.selectionType.get)
  }

  @Test def evaluatingHomozygous {
    val cross = new Cross(plantPair, criteria, SelectionType.Homozygous) named name

    var expected = new OffspringPlant(Array(), mock[PlantSpec], None, Option(name))

    when(plantCrosser.selectHomozygousOffspring(plantPair, criteria)).thenReturn(Option(expected))
    val result = cross.evaluateUsing(plantCrosser)

    assert(result === expected)
  }

  @Test def evaluatingHeterozygous {
    val cross = new Cross(plantPair, criteria, Option(name), Option(SelectionType.Heterozygous))

    var expected = new OffspringPlant(Array(), mock[PlantSpec], None, Option(name))

    when(plantCrosser.selectHeterozygousOffspring(plantPair, criteria)).thenReturn(Option(expected))
    val result = cross.evaluateUsing(plantCrosser)

    assert(result === expected)
  }
}