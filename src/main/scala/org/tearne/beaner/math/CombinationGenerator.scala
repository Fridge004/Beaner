package org.tearne.beaner.math

import org.tearne.beaner.math.State.{IN, OUT}

object CombinationGenerator{
  def apply(length: Int): Set[Combination] = {
    def addLayersFromDepth(
    	currentDepth: Int, 
    	builder: Combination = Combination.empty, 
    	acc: Set[Combination] = Set[Combination]()
    ): Set[Combination] = {
	  if(currentDepth == length)
	    acc + builder
	  else{
	    var acc1 = addLayersFromDepth(currentDepth + 1, builder :+ IN, acc)
	    acc1 = addLayersFromDepth(currentDepth + 1, builder :+ OUT, acc1)
	    acc1
	  }
	}
	
	addLayersFromDepth(0)
  }
}