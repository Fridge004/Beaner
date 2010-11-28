package org.tearne.beaner.cross

import org.junit.{ Test, Before }
import org.scalatest.junit.JUnitSuite
import org.mockito.Mockito._
import org.mockito.Matchers._
import org.scalatest.mock.MockitoSugar
import org.tearne.beaner.plant._
import org.tearne.beaner.cross._
import org.tearne.beaner.plant.selection.Criteria

class PlantPairTest extends JUnitSuite with MockitoSugar {
  var pair: PlantPair = null
  var criteriaList: List[Criteria] = null
  var plantCrosser: PlantCrosser = null
  val resultPlant: Option[OffspringPlant] = Option(mock[OffspringPlant])

  @Before
  def setup() {
    pair = PlantPair(mock[Plant], mock[Plant])

    criteriaList = mock[Criteria] :: Nil
    plantCrosser = mock[PlantCrosser]

    PlantPair.setPlantCrosser(plantCrosser)
  }

  @Test
  def exceptionIfCrossingFails() {
    when(plantCrosser.selectHeterozygousOffspring(anyObject(), anyObject())).thenReturn(None)
    PlantPair.setPlantCrosser(plantCrosser)
    intercept[OffspringPlantException] {
      pair selectHet criteriaList
    }
  }

  @Test
  def selectHet() {
    when(plantCrosser.selectHeterozygousOffspring(pair, criteriaList)).thenReturn(resultPlant)
    assert(resultPlant.get === pair.selectHet(criteriaList))
  }

  @Test
  def selectHom() {
    when(plantCrosser.selectHomozygousOffspring(pair, criteriaList)).thenReturn(resultPlant)
    assert(resultPlant.get === pair.selectHom(criteriaList))
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