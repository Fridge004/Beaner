package org.tearne.beaner.math
import org.junit.Test
import org.scalatest.junit.JUnitSuite

class CombinationGeneratorTest extends JUnitSuite{
  import org.tearne.beaner.math.State.{IN, OUT}
  
  @Test def lengthTwo { 
    val gen: Set[Combination] = CombinationGenerator(2)
    
    assert(gen.size === 4)
    assert(gen contains Combination(OUT, OUT))
    assert(gen contains Combination(IN, OUT))
    assert(gen contains Combination(OUT, IN))
    assert(gen contains Combination(IN, IN))
  }
  
  @Test def lengthThree { 
    val gen: Set[Combination] = CombinationGenerator(3)
    
    assert(gen.size === 8)
    assert(gen contains Combination(OUT, OUT, OUT))
    
    assert(gen contains Combination(IN, OUT, OUT))
    assert(gen contains Combination(OUT, IN, OUT))
    assert(gen contains Combination(OUT, OUT, IN))
    
    assert(gen contains Combination(IN, IN, OUT))
    assert(gen contains Combination(OUT, IN, IN))
    assert(gen contains Combination(IN, OUT, IN))
    
    assert(gen contains Combination(IN, IN, IN))
  }
}