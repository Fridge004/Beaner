package org.tearne.beaner.cross

import math.abs
import org.tearne.beaner._
import plant._
import cross._
import chroma._
import model._
import org.tearne.beaner.math._
import org.tearne.beaner.math.State.{IN, OUT}

class Gameter(val recombinationModel:RecombinationModel) {
	
  /** Do a cross without any selection  
   */
  def withoutSelection(chromosome: Chromosome): Chromatid = {
    val firstChromatid = chromosome.firstChromatid
    val secondChromatid = chromosome.secondChromatid

    val gameteArray = new Array[Centimorgan](firstChromatid.size)
    for (i <- 0 until firstChromatid.size) {
      gameteArray(i) = firstChromatid(i).gameteify(secondChromatid(i), 0.5)
    }

    new Chromatid(gameteArray)
  }
  
  /** Probability that a gamete of the chromosome
   * contains the plant gene at the index
   */
  def probContains(plant: Plant, index: Int, chromosome: Chromosome):Double = {
    val firstChromatid = chromosome.firstChromatid
    val secondChromatid = chromosome.secondChromatid

    0.5 * (firstChromatid.probabilityOf(plant, index) + secondChromatid.probabilityOf(plant, index))
  }

  /** Make a gamete selecting for the given plant at the index 
   */
  def selectOn(plant: Plant, index: Int, chromosome: Chromosome): Chromatid = {
    val firstChromatid = chromosome.firstChromatid
    val secondChromatid = chromosome.secondChromatid

    if (firstChromatid(index).probabilityOf(plant) == 1) {
      if (secondChromatid(index).probabilityOf(plant) == 1) {
        selectOnBothTids(index, plant, chromosome)
      } else
        selectOnSingleTid(true, index, plant, chromosome)
    } else if (secondChromatid(index).probabilityOf(plant) == 1) {
      selectOnSingleTid(false, index, plant, chromosome)
    } else {
      throw new GameterException("Allele not present with probability one on either chromatid")
    }
  }
  
   /** Probability that a gamete produced by the chromosome
   *  would contain the plant gene at two specified indexes
   */
  def probContains(plant: Plant, firstIndex: Int, secondIndex: Int, chromosome: Chromosome):Double = {
    if(secondIndex <= firstIndex)
    	return probContains(plant, secondIndex, firstIndex, chromosome)
    
    val firstChromatid = chromosome.firstChromatid
    val secondChromatid = chromosome.secondChromatid
    
    val probChro1Pos1 = firstChromatid.probabilityOf(plant, firstIndex) 
    val probChro1Pos2 = firstChromatid.probabilityOf(plant, secondIndex) 
    val probChro2Pos1 = secondChromatid.probabilityOf(plant, firstIndex)
    val probChro2Pos2 = secondChromatid.probabilityOf(plant, secondIndex)
    
    if(probChro1Pos1 == 0 && probChro2Pos1 == 0) 
      return 0.0
    if(probChro1Pos2 == 0 && probChro2Pos2 == 0) 
      return 0.0
    
    if(probChro1Pos1 != 1 && probChro2Pos1 != 1)
      throw new UnsupportedOperationException("Not supported yet: Prob gamete contains double alleles when neither chromotids contains gene with certainty at position 1")
    if(probChro1Pos2 != 1 && probChro2Pos2 != 1)
      throw new UnsupportedOperationException("Not supported yet: Prob gamete contains double alleles when neither chromotids contains gene with certainty at position 2")
    
    if(probChro1Pos1 == 1 && probChro2Pos1 == 1 && probChro1Pos2 == 1 && probChro2Pos2 == 1)
      return 1
    
	if(probChro1Pos1 == 1 && probChro2Pos1 == 1)
	  throw new UnsupportedOperationException("Not supported yet: Prob gamete contains double alleles when both chromatids contain gene with certainty at position 1")
	if(probChro1Pos2 == 1 && probChro2Pos2 == 1)
	  throw new UnsupportedOperationException("Not supported yet: Prob gamete contains double alleles when both chromatids contain gene with certainty at position 2")
	
    if(probChro1Pos1*probChro1Pos2 == 1){
      //On same tid
      0.5*getProbInAtDist(secondIndex-firstIndex)
    }
    else{
      //On different tids
      0.5*(1-getProbInAtDist(secondIndex-firstIndex))
    }
  }
  
  /** Make gamete by selection at two centiMorgans
   */
  def selectOn(plant: Plant, firstIndex: Int, secondIndex: Int, chromosome: Chromosome): Chromatid = {
	if(secondIndex <= firstIndex)
	  return selectOn(plant, secondIndex, firstIndex, chromosome)

    val firstChromatid = chromosome.firstChromatid
    val secondChromatid = chromosome.secondChromatid
    
    val probChro1Pos1 = firstChromatid.probabilityOf(plant, firstIndex) 
    val probChro1Pos2 = firstChromatid.probabilityOf(plant, secondIndex) 
    val probChro2Pos1 = secondChromatid.probabilityOf(plant, firstIndex)
    val probChro2Pos2 = secondChromatid.probabilityOf(plant, secondIndex)
    
    if(probChro1Pos1 != 1 && probChro2Pos1 != 1)
      throw new GameterException("Allele is not present at position 1 on either chromatid")
    if(probChro1Pos2 != 1 && probChro2Pos2 != 1)
      throw new GameterException("Allele is not present at position 2 on either chromatid")
    
    if(probChro1Pos1 != 1 && probChro2Pos1 != 1)
      throw new UnsupportedOperationException("Not supported yet: Selection for double alleles when neither chromotids contains gene with certainty at position 1")
    if(probChro1Pos2 != 1 && probChro2Pos2 != 1)
      throw new UnsupportedOperationException("Not supported yet: Selection for double alleles when neither chromotids contains gene with certainty at position 2")
    
	if(probChro1Pos1 == 1 && probChro2Pos1 == 1 && probChro1Pos2 == 1 && probChro2Pos2 == 1)
	  return withoutSelection(chromosome)
	  
	if(probChro1Pos1 == 1 && probChro2Pos1 == 1)
	  throw new UnsupportedOperationException("Not supported yet: Selection for double alleles when both chromatids contain gene with certainty at position 1")
	if(probChro1Pos2 == 1 && probChro2Pos2 == 1)
	  throw new UnsupportedOperationException("Not supported yet: Selection for double alleles when both chromatids contain gene with certainty at position 2")
    
	//We now know that both alleles can be provided, but are they on the same chromatid? 
	if(probChro1Pos1 == 1)
	  if(probChro1Pos1 == 1){
	    //Both on first tid
	    buildDoubleSelectionGamete(firstIndex, secondIndex, firstChromatid, secondChromatid, true)
	  }
	  else{
	    //First on first tid second second on second
	    buildDoubleSelectionGamete(firstIndex, secondIndex, firstChromatid, secondChromatid, false)
	  }
	else{
	  	if(probChro1Pos2 == 1){
	    //First on second tid, second on first tid
	  	buildDoubleSelectionGamete(firstIndex, secondIndex, secondChromatid, firstChromatid, false)
	  }
	  else{
	    //Both on second tid
	    buildDoubleSelectionGamete(firstIndex, secondIndex, secondChromatid, firstChromatid, true)
	  }
	}
  }
  
  private def buildDoubleSelectionGamete(firstIndex:Int, secondIndex:Int, startingChromatid:Chromatid, secondaryChromatid:Chromatid, bothOnFirstTid:Boolean)={
    val probsBetween = 
      if(bothOnFirstTid) 
    	  betweenSelectionDragArray(secondIndex-firstIndex, IN, IN)
      else
    	  betweenSelectionDragArray(secondIndex-firstIndex, IN, OUT)

    val gamete = new Chromatid(startingChromatid)
    	    	    
    0 until firstIndex foreach{ i =>
      gamete(i) = startingChromatid(i).combinedWith(secondaryChromatid(i), getProbInAtDist(i-firstIndex))
    }
    
    gamete(firstIndex) = startingChromatid(firstIndex)
    
    firstIndex+1 until secondIndex foreach{ i =>
      gamete(i) = startingChromatid(i).combinedWith(secondaryChromatid(i), probsBetween(i-firstIndex))
    }
    
    gamete(secondIndex) = if(bothOnFirstTid) startingChromatid(secondIndex) else secondaryChromatid(secondIndex)
    
    secondIndex+1 until gamete.size foreach{ i =>
      gamete(i) = startingChromatid(i).combinedWith(secondaryChromatid(i), getProbInAtDist(i-firstIndex))
    }
    
    gamete
  }

  private def selectOnBothTids(index: Int, plant: Plant, chromosome: Chromosome) = {
    val firstChromatid = chromosome.firstChromatid
    val secondChromatid = chromosome.secondChromatid

    val gamete = new Chromatid(firstChromatid)
    for (i <- 0 until gamete.size) {
      if (i == index && (firstChromatid(i).alleles(plant) != 1.0 || secondChromatid(i).alleles(plant) != 1.0))
        throw new GameterException("Allele unexpectidly not present with probability one on both chromatids")
      else{
    	  gamete(i) = firstChromatid(i).combinedWith(secondChromatid(i), 0.5)
      }
    }
    gamete
  }

  private def selectOnSingleTid(selectFirstChromatid: Boolean, index: Int, plant: Plant, chromosome: Chromosome) = {
    val firstChromatid = chromosome.firstChromatid
    val secondChromatid = chromosome.secondChromatid

    var cMa, cMb: Centimorgan = null
    val gamete = new Chromatid(firstChromatid)
    var tid1, tid2: Chromatid = null
    if (selectFirstChromatid) {
      tid1 = firstChromatid
      tid2 = secondChromatid
    } else {
      tid1 = secondChromatid
      tid2 = firstChromatid
    }

    for (i <- 0 until gamete.size) {
      if (i == index)
        if (tid1(i).alleles(plant) != 1.0)
          throw new GameterException("Allele unexpectidly not present with probability one on chromatid")
        else
          gamete(i) = new Centimorgan(plant)
      else {
        cMa = tid1(i); cMb = tid2(i)
        gamete(i) = cMa.combinedWith(cMb, getProbInAtDist(index - i))
      }
    }
    gamete
  }

  private def getProbInAtDist(d: Int) = recombinationModel.probInAtDistance(abs(d))
  
  private def betweenSelectionDragArray(length:Int, startParity:State.Value, endParity:State.Value) = 
      new DoubleSelectionProb(recombinationModel).probabilityArray(abs(length), startParity, endParity)
}