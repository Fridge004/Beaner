package org.tearne.beaner.math

import org.junit.Test
import org.junit.Assert.assertEquals
import org.scalatest.junit.JUnitSuite
import org.scalatest.mock.MockitoSugar
import org.mockito.Mockito._
import org.tearne.beaner.model.RecombinationModel
import State.{IN, OUT}

class CombinationTest extends JUnitSuite with MockitoSugar{
  
  val tolerance = 1e-5
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
    
    assertEquals( combination.probabilityUnnormalised( IN,  IN, recombinationModel), probStick*(1-probStick)*(1-probStick), tolerance )
    assertEquals( combination.probabilityUnnormalised(OUT,  IN, recombinationModel), (1-probStick)*(1-probStick)*(1-probStick), tolerance )
    assertEquals( combination.probabilityUnnormalised( IN, OUT, recombinationModel), probStick*(1-probStick)*probStick, tolerance )
    assertEquals( combination.probabilityUnnormalised(OUT, OUT, recombinationModel), (1-probStick)*(1-probStick)*probStick, tolerance )
  }
  
  @Test def normalisedProbabilities {
    //Probabilities wont naturally sum to one, since there are constratins
    // imposed by the endpoints of the combination, so some normalisation
    // is needed
    val recombinationModel = mock[RecombinationModel]
    when(recombinationModel.probInAtDistance(1)).thenReturn(probStick)
    
    val c1 = Combination( IN,  IN)
    val c2 = Combination( IN, OUT)
    val c3 = Combination(OUT,  IN)
    val c4 = Combination(OUT, OUT)
    
    assertEquals( probStick*probStick*probStick, 		c1.probabilityUnnormalised(IN, IN, recombinationModel), tolerance )
    assertEquals( probStick*(1-probStick)*(1-probStick), c2.probabilityUnnormalised(IN, IN, recombinationModel), tolerance )
    assertEquals( (1-probStick)*(1-probStick)*probStick, c3.probabilityUnnormalised(IN, IN, recombinationModel), tolerance )
    assertEquals( (1-probStick)*probStick*(1-probStick), c4.probabilityUnnormalised(IN, IN, recombinationModel), tolerance )
    
    val total = c1.probabilityUnnormalised(IN, IN, recombinationModel) +
    			c2.probabilityUnnormalised(IN, IN, recombinationModel) + 
    			c3.probabilityUnnormalised(IN, IN, recombinationModel) +			
    			c4.probabilityUnnormalised(IN, IN, recombinationModel)
    
    assertEquals( probStick*probStick*probStick/total, 			c1.probability(IN, IN, recombinationModel), tolerance )
    assertEquals( probStick*(1-probStick)*(1-probStick)/total,	c2.probability(IN, IN, recombinationModel), tolerance )
    assertEquals( (1-probStick)*(1-probStick)*probStick/total,	c3.probability(IN, IN, recombinationModel), tolerance )
    assertEquals( (1-probStick)*probStick*(1-probStick)/total,	c4.probability(IN, IN, recombinationModel), tolerance )
  }

}