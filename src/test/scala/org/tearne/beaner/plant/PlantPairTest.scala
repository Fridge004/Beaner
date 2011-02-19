package org.tearne.beaner.plant

import org.scalatest.junit.JUnitSuite
import org.scalatest.mock.MockitoSugar

class PlantPairTest extends JUnitSuite with MockitoSugar {
  val p1 = mock[ParentPlant]
  val p2 = mock[ParentPlant]
  val criteria = Set(mock[Criteria])
  val pair = new PlantPair(p1, p2)

  @Test def heterozygousSelection {
    val result = pair.selectHet(criteria)

    assert(Cross.getClass === result.getClass)
    assert(criteria === result.critera)
    assert(pair === result.plantPair)
    assert(none === result.name)
    assert(CrossType.Heterozygous === result.crossType)
  }

@Test def homozygousSelection {
    val result = pair.selectHom(criteria)

    assert(Cross.getClass === result.getClass)
    assert(criteria === result.critera)
    assert(pair === result.plantPair)
    assert(none === result.name)
    assert(CrossType.Homozygous === result.crossType)
  }
}