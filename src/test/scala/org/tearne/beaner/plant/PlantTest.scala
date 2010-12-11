package org.tearne.beaner.plant

import org.junit.Test
import org.mockito.Mockito._
import org.mockito.Spy._
import org.tearne.beaner.plant._
import org.tearne.beaner.chroma._
import org.tearne.beaner.cross._
import org.scalatest.mock.MockitoSugar
import org.scalatest.Assertions._

class PlantTest extends MockitoSugar{

  class PlantImpl(val chromasomes:Array[Chromosome], val spec:PlantSpec) extends Plant{}
  
  @Test def pairs() {
    val p1 = new PlantImpl(null, null)
    val p2 = new PlantImpl(null, null)
    
    val pair:PlantPair = p1 x p2
    assert(p1 === pair.first)
    assert(p2 === pair.second)
  }
}
