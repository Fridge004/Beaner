package org.tearne.beaner.math

import org.junit.Test
import org.junit.Assert.assertEquals
import org.scalatest.junit.JUnitSuite
import org.scalatest.mock.MockitoSugar
import org.mockito.Mockito._
import org.tearne.beaner.model.RecombinationModel
import State.{IN, OUT}

class CombinationTest extends JUnitSuite with MockitoSugar{
  
  val probStick = 0.99
  
  @Test def sequenceBahaviour {
    val combination = Combination(IN, OUT, OUT, IN, IN)
    
    assert( IN === combination(0))
    assert(OUT === combination(1))
    assert(OUT === combination(2))
    assert( IN === combination(3))
    assert( IN === combination(4))
  }
  
  @Test def probabilityOf {
    val combination = Combination(IN, OUT)
    val recombinationModel = mock[RecombinationModel]
    when(recombinationModel.probInAtDistance(1)).thenReturn(probStick)
    
    assert(combination.probability(IN, IN, recombinationModel)  === probStick*(1-probStick)*(1-probStick) )
    assert(combination.probability(OUT, IN, recombinationModel) === (1-probStick)*(1-probStick)*(1-probStick) )
    assert(combination.probability(IN, OUT, recombinationModel) === probStick*(1-probStick)*probStick )
    assert(combination.probability(OUT, OUT, recombinationModel) === (1-probStick)*(1-probStick)*probStick )
  }
  
  @Test def sumToOne {
    val recombinationModel = mock[RecombinationModel]
    when(recombinationModel.probInAtDistance(1)).thenReturn(probStick)
    
    val total = Combination(IN, IN).probability(IN, IN, recombinationModel)
    			+ Combination(OUT, IN).probability(IN, IN, recombinationModel)
    			+ Combination(IN, OUT).probability(IN, IN, recombinationModel)
    			+ Combination(OUT, OUT).probability(IN, IN, recombinationModel)
    assertEquals(1.0, total, 1e-3)
  }

}