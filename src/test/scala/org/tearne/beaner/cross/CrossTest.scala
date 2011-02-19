package org.tearne.beaner.cross

import org.scalatest.junit.JUnitSuite
import org.scalatest.mock.MockitoSugar
import org.junit.Test
import org.tearne.beaner.plant._

class CrossTest extends JUnitSuite with MockitoSugar{
  val pair = mock[PlantPair]
  val criterion = Set(mock[Criterion])
  val name = "named"
  val cross = new Cross(pair, criterion)

  @Test def exceptionIfNotNamed {
    val recombModel = mock[RecombinationModel]

    intercept[UnnamedPlantException]{
      cross.evaluateWith(recombModel)
    }
  }

  @Test def naming {
    assert(Cross.getClass === cross.getClass)
    assert(name === cross.name)
    assert(pair === cross.pair)
    assert(criterion === cross.criterion)
  }

  @Test def evaluatingWithParentPlant {
    fail("Not written yet")
    val recombModel = mock[RecombinationModel]
    val offspring = cross.evaluateWith(recombModel)

    //verify(recombModel).
  }
}