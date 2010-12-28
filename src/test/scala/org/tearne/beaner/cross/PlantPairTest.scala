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

import org.junit.{ Test, Before }
import org.scalatest.junit.JUnitSuite
import org.mockito.Mockito._
import org.mockito.Matchers._
import org.scalatest.mock.MockitoSugar
import org.tearne.beaner.plant._

class PlantPairTest extends JUnitSuite with MockitoSugar {
  var pair: PlantPair = null
  var criteria: Set[Criterion] = null
  var plantCrosser: PlantCrosser = null
  val resultPlant: Option[OffspringPlant] = Option(mock[OffspringPlant])

  @Before
  def setup() {
    pair = PlantPair(mock[Plant], mock[Plant])

    criteria = Set(mock[Criterion])
    plantCrosser = mock[PlantCrosser]

    PlantPair.setPlantCrosser(plantCrosser)
  }

  @Test
  def exceptionIfCrossingFails() {
    when(plantCrosser.selectHeterozygousOffspring(anyObject(), anyObject())).thenReturn(None)
    PlantPair.setPlantCrosser(plantCrosser)
    intercept[OffspringPlantException] {
      pair selectHet criteria
    }
  }

  @Test
  def selectHet() {
    when(plantCrosser.selectHeterozygousOffspring(pair, criteria)).thenReturn(resultPlant)
    assert(resultPlant.get === pair.selectHet(criteria))
  }

  @Test
  def selectHom() {
    when(plantCrosser.selectHomozygousOffspring(pair, criteria)).thenReturn(resultPlant)
    assert(resultPlant.get === pair.selectHom(criteria))
  }

  @Test
  def holdsPairOfPlants() {
    val p1 = mock[Plant]
    val p2 = mock[Plant]
    pair = new PlantPair(p1, p2)
    assert(p1 === pair.first)
    assert(p2 === pair.second)

    pair = PlantPair(p1, p2)
    assert(p1 === pair.first)
    assert(p2 === pair.second)
  }
}
