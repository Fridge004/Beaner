package org.tearne.beaner.math

import org.tearne.beaner.math.State.{IN, OUT}
import org.junit.Test
import org.junit.Assert.fail
import org.scalatest.junit.JUnitSuite
import org.scalatest.mock.MockitoSugar
import org.tearne.beaner.model.RecombinationModel
import org.mockito.Mockito.when

class DoubleSelectionProbTest extends JUnitSuite with MockitoSugar{

  val probStick = 0.9
  
  @Test def probLength2 {
    //Assume endpoints are both IN
    // i.e. IN - ? - ? - IN
    
    val recombinationModel = mock[RecombinationModel]
    when(recombinationModel.probInAtDistance(1)).thenReturn(probStick)
    
    val dsp = new DoubleSelectionProb(recombinationModel)
    
    val p1 = Combination(OUT, OUT).probability(IN, IN, recombinationModel)
    val p2 = Combination( IN, OUT).probability(IN, IN, recombinationModel)
    val p3 = Combination(OUT,  IN).probability(IN, IN, recombinationModel)
    val p4 = Combination( IN,  IN).probability(IN, IN, recombinationModel)
    val pAll = p1 + p2 + p3 + p4
    
    val expected = Array(
      p2+p4,
      p3+p4
    )
    
    assert( expected === dsp.probabilityArray(2, IN, IN) )
  }
}