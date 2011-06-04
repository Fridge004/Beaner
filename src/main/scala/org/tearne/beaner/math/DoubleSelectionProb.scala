package org.tearne.beaner.math

import org.tearne.beaner.model.RecombinationModel
import org.tearne.beaner.math.State.{IN, OUT}

class DoubleSelectionProb(val recombinationModel: RecombinationModel) {
  
  def probabilityArray(length: Int, startState: State.Value, endState: State.Value): Array[Double] = {
    val result = new Array[Double](length)
    val combinations = CombinationGenerator(length).toArray
    val probs = combinations.map(_.probability(startState, endState, recombinationModel))
    val arraysOfProbs: IndexedSeq[Array[Double]] = combinations.zip(probs).map(pair => makeProbabilitiesArray(pair._1, pair._2))

    arraysOfProbs.fold(new Array[Double](length))(addArraysTogether(_,_))
  }
  
  private def makeProbabilitiesArray(combination:Combination, prob:Double):Array[Double] = {
    combination.map(state => if(state==IN) prob else 0).toArray
  }
  
  private def addArraysTogether(a: Array[Double], b: Array[Double]):Array[Double] = {
	a.zip(b).map{case (a,b) => a+b}.toArray
  }
}