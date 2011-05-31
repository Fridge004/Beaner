package org.tearne.beaner.math
import scala.collection.mutable.ArrayBuffer
import scala.collection.SetLike
import scala.collection.generic.CanBuildFrom
import scala.collection.mutable.Builder
import scala.collection.IndexedSeqLike
import scala.collection.immutable.VectorBuilder
import org.tearne.beaner.model.RecombinationModel

class Combination(buf: Vector[State.Value])
	extends IndexedSeq[State.Value]
	with IndexedSeqLike[State.Value, Combination]{
  
  override def newBuilder = Combination.newBuilder
  //override def size = buf.size
  override def stringPrefix = "Combination"
  //override def iterator: Iterator[Combination.State.Value] = buf.iterator
  
  //def empty = Combination.empty   
  
  def apply(idx: Int) = buf.apply(idx)
  //def contains(elem: Combination.State.Value) = buf.contains(elem)
  def length = buf.length
  //def +(elem: Combination.State.Value) = new Combination(elem +: buf)
  
  def probability(startState: State.Value, endState: State.Value, model: RecombinationModel) = {    
    def probBetween(stateA: State.Value, stateB: State.Value) = {
      val prob = model.probInAtDistance(1) 
      if(stateA == stateB) 
    	prob 
      else 
        1-prob  
    }
    
    val it = this.iterator
    var prev: State.Value = startState
    var next: State.Value = null
    var prob = 1.0
    while(it.hasNext){
      next = it.next
      prob = prob * probBetween(prev, next)
      prev = next
    }

    prob * probBetween(prev, endState)
  }
}

object State extends Enumeration {
  val IN, OUT = Value
}

object Combination {
  def apply(values: State.Value*) = new Combination(Vector[State.Value]() ++ values)
  
  implicit def combinationCanBuildFrom = new CanBuildFrom[Combination, State.Value, Combination](){
    def apply(from: Combination) = newBuilder
    def apply() = newBuilder
  }

  def newBuilder: Builder[State.Value, Combination] =
    new VectorBuilder[State.Value] mapResult (buf => new Combination(buf))

  def empty: Combination = new Combination(Vector.empty[State.Value])
}