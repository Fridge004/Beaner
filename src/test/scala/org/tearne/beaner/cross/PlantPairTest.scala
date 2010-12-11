package org.tearne.beaner.cross

import org.junit.{ Test, Before }
import org.scalatest.junit.JUnitSuite
import org.mockito.Mockito._
import org.mockito.Matchers._
import org.scalatest.mock.MockitoSugar
import org.tearne.beaner.plant._
import org.tearne.beaner.cross._

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
